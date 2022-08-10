sentimentTimelineUI <- function(id){
  ns <- NS(id)

  tagList(
    description = div(
      "This chart shows the average sentiment of matching search results for each month, by data set."
    ),
    plot = div(
      style = "max-width: 1200px;",
      fluidRow(
        column(
          offset = 8, # this puts the switch over to the right
          width = 3,
          shinyWidgets::materialSwitch(inputId = ns("show_trend"),
                                       value = FALSE,
                                       label = "Show Trend Line?",
                                       status = "primary",
                                       right = TRUE)
        )
      ),
      shiny::uiOutput(ns("sentiment_timeline_plot_ui"))  %>%
        shinycssloaders::withSpinner(id = ns("this spinner"))
    )
  )
}

sentimentTimeline <- function(input, output, session,
                              data_set_info,
                              query_info) {

  plot_data <- reactive({
    req(query_info()$num_hits > 0)

    aggregations = query_text_depot(query_info = query_info(),
                                    aggregates_json = sentimentTimelineQuery())
    aggregations = parse_aggregates(es_results = aggregations)

    plot_hits = aggregations$month_counts.buckets %>%
      transmute(Date = as.Date(stringr::str_sub(key_as_string, 1, 10)), Sentiment = sentiment.value, index_name = index, Count = doc_count) %>%
      left_join(select(data_set_info(), index_name, display_name), by = "index_name") %>% # to get display name
      mutate(Count = ifelse(Count == 0, NA, Count)) %>%
      arrange(Date) %>%
      filter(!is.na(Sentiment)) %>%
      arrange(display_name)

    return(plot_hits)
  })

  output$sentiment_timeline_plotly <- plotly::renderPlotly({
    req(plot_data)

    p <- plot_data() %>%
      plot_timeseries_td(
        date_var = Date,
        value_var = Sentiment,
        group_var = display_name,
        colour_var = display_name,
        data_set_info = data_set_info(),
        scales = "free_y",
        show_trend = input$show_trend,
        date_format = "%Y-%b"
      )

    # hack: blank dummy line with text aesthetic to define the tooltip text that
    # is shown in the plotly plot:
    # https://stackoverflow.com/questions/44569551/date-format-in-hover-for-ggplot2-and-plotly
    p <- p +
      geom_line(aes(text = paste0("<b>", display_name, '</b>\n',
                                  "Average Sentiment: ", sigfig(Sentiment, n = 2), '\n',
                                  "Document Count: ", Count, '\n',
                                  "Date: ", format(Date, format = "%b-%Y")))) %>%
      suppressWarnings()

    not_enough_data <- plot_data() %>%
      group_by(display_name) %>%
      filter(n() <= 1)

    if (nrow(not_enough_data) > 0) {
      p <- p + geom_point(data = not_enough_data, mapping = aes(text = paste0("<b>", display_name, '</b>\n',
                                                                              "Average Sentiment: ", sigfig(Sentiment, n = 2), '\n',
                                                                              "Document Count: ", Count, '\n',
                                                                              "Date: ", format(Date, format = "%b-%Y")))) %>%
        suppressWarnings()
    }

    p1 <- plotly::ggplotly(p, tooltip = c("text"), dynamicTicks = TRUE)  %>%
      layout(margin = list(l = 75, r = 75))

    for (x in names(p1$x$layout)[grepl("yaxis", names(p1$x$layout))]) {
      p1[["x"]][["layout"]][[x]][["fixedrange"]] <- TRUE
    }

    return(p1)
  })

  output$sentiment_timeline_plot_ui <- renderUI({
    n_facets <- n_distinct(plot_data()$display_name)
    plotly::plotlyOutput(session$ns("sentiment_timeline_plotly"), height = 200 + (100 * n_facets))
  })

}

sentimentTimelineQuery <- function() {
  '
  "aggs": {
    "group_by_index": {
      "terms": {
        "field": "_index"
      },
      "aggs" : {
        "month_counts": {
          "date_histogram" : {
            "field" : "date",
            "calendar_interval" : "month"
          },
          "aggs" : {
            "sentiment" : {
              "avg" : {
                "field" : "sentiment_polarity"
              }
            }
          }
        }
      }
    }
  }
  '
}

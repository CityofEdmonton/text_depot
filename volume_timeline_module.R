volumeTimelineUI <- function(id){
  ns <- NS(id)

  tagList(
    div(
      style = "max-width: 1200px;",
      fluidRow(
        column(
          width = 7,
          "This chart shows the number of matching search results for each month. Click and drag to zoom, and double-click to reset the zoom level."
        ),
        column(
          width = 3,
          shinyWidgets::materialSwitch(inputId = ns("show_trend"),
                                       value = FALSE,
                                       label = "Show Trend Line?",
                                       status = "primary",
                                       right = TRUE),
        )
      )
    ),
    div(
      style = "max-width: 1200px;",
      shiny::uiOutput(ns("volume_timeline_plot_ui")) %>%
        shinycssloaders::withSpinner()
    )
  )
}

volumeTimeline <- function(input, output, session,
                           data_set_info,
                           query_info) {

  plot_data <- reactive({
    req(query_info()$num_hits > 0)

    aggregations <- query_text_depot(query_info = query_info(),
                                     aggregates_json = volumeTimelineQuery())
    aggregations = parse_aggregates(es_results = aggregations)

    plot_hits <- aggregations$month_counts.buckets %>%
      transmute(Date = as.Date(key_as_string), Count = doc_count, index_name = index) %>%
      left_join(select(data_set_info(), index_name, display_name), by = "index_name") %>% # to get display name
      mutate(Date = strftime(Date, format = "%Y-%m")) %>%
      mutate(Date = as.Date(paste0(Date, "-01"))) %>%
      arrange(display_name)

    # Pad months:
    all_months <- data_set_info() %>%
      filter(index_name %in% unique(aggregations$month_counts.buckets$index)) %>%
      group_by(display_name) %>%
      transmute(Date = list( seq(lubridate::floor_date(date_range_min, unit = "month"),
                                 lubridate::ceiling_date(date_range_max, unit = "month") - as.difftime(1, unit = "days"),
                                 by = "months")) ) %>%
      ungroup() %>%
      tidyr::unnest_longer(Date) %>%
      mutate(fill_value = 0)

    plot_hits <- plot_hits %>%
      right_join(all_months, by = c("Date" = "Date", "display_name" = "display_name")) %>%
      mutate(Count = dplyr::coalesce(Count, fill_value)) # when count is NA, use the fill value

    return(plot_hits)

  })

  output$volume_timeline_plot <- plotly::renderPlotly({

    req(plot_data)
    req(data_set_info)

    p <- plot_data() %>%
      plot_timeseries_td(
        date_var = Date,
        value_var = as.integer(Count),
        group_var = display_name,
        colour_var = display_name,
        data_set_info = data_set_info(),
        scales = "free_y",
        show_trend = input$show_trend,
        date_format = "%Y-%b"
      )

    # https://stackoverflow.com/questions/44569551/date-format-in-hover-for-ggplot2-and-plotly
    p <- p +
      geom_line(aes(text = paste0("<b>", display_name, '</b>\n',
                                  "Document Count: ", Count, '\n',
                                  "Date: ", format(Date, format = "%b-%Y")))) %>%
      suppressWarnings()

    not_enough_data <- plot_data() %>%
      group_by(display_name) %>%
      filter(n() <= 1)

    if (nrow(not_enough_data) > 0) {
      p <- p + geom_point(data = not_enough_data, mapping = aes(text = paste0("<b>", display_name, '</b>\n',
                                                                              "Document Count: ", Count, '\n',
                                                                              "Date: ", format(Date, format = "%b-%Y")))) %>%
        suppressWarnings()
    }


    p1 <- plotly::ggplotly(p, tooltip = c("text"), dynamicTicks = TRUE) %>%
      layout(margin = list(l = 75, r = 75))

    # loop over y axes (facets) in the plotly plot:
    for (x in names(p1$x$layout)[grepl("yaxis", names(p1$x$layout))]) {
      # turn off y zoom for each y axis
      p1[["x"]][["layout"]][[x]][["fixedrange"]] <- TRUE
    }

    return(p1)

  })


  output$volume_timeline_plot_ui <- renderUI({
    n_facets <- n_distinct(plot_data()$display_name)
    plotly::plotlyOutput(session$ns("volume_timeline_plot"), height = 200 + (100 * n_facets))
  })


}

volumeTimelineQuery <- function() {
  '
  "aggs": {
    "group_by_index": {
      "terms": {
        "field": "_index"
      },
      "aggs" : {
        "month_counts" : {
          "date_histogram" : {
              "field" : "date",
              "calendar_interval" : "month"
          }
        }
      }
    }
  }
  '
}

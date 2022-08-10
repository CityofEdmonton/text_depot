sentimentHistogramUI <- function(id){
  ns <- NS(id)

  tagList(
    description = div(
      "
       Sentiment histogram for matching documents. This chart shows how many
       documents have each sentiment value, per data set. Sentiment
       above zero is positive (e.g. happy, positive, satisfied),
       and below zero is negative (e.g. angry, disappointed, sad). Note
       that the sentiment is predicted using AI, and may be incorrect
       in some cases (e.g. when sarcasm is used).
      "
    ),
    plot = div(
      style = "max-width: 1200px;",
      shiny::uiOutput(ns("sentiment_histogram_ui")) %>%
        shinycssloaders::withSpinner()
    )
  )
}

sentimentHistogram <- function(input, output, session,
                               data_set_info,
                               query_info) {

  plot_data <- reactive({
    req(query_info()$num_hits > 0)

    aggregations = query_text_depot(query_info = query_info(),
                                    aggregates_json = volumeSentimentQuery())
    aggregations = parse_aggregates(es_results = aggregations)

    plot_hits = aggregations$sentiment.buckets %>%
      transmute(Sentiment = key, Count = doc_count, index_name = index) %>%
      left_join(select(data_set_info(), index_name, display_name), by = "index_name") %>% # to get display name
      arrange(display_name)

    return(plot_hits)

  })

  output$sentiment_histogram <- plotly::renderPlotly({

    req(plot_data)

    p <- plot_data() %>%
      ggplot2::ggplot(mapping = aes(x = Sentiment, y = Count, fill = display_name)) +
      facet_wrap("display_name", scales = "free_y", ncol = 1) +
      # hack: blank dummy line with text aesthetic to define the tooltip text that
      # is shown in the plotly plot:
      # https://stackoverflow.com/questions/44569551/date-format-in-hover-for-ggplot2-and-plotly
      suppressWarnings(
        geom_col(aes(text = paste0("<b>", display_name, '</b>\n',
                                   "Sentiment: ", sigfig(Sentiment, n = 2), '\n',
                                   "Document Count: ", Count)))
      ) +
      scale_fill_manual(breaks = data_set_info()$display_name,
                        values = data_set_info()$colours) + ggthemes::theme_gdocs() + theme(legend.position = "none")

    p1 <- plotly::ggplotly(p, tooltip = c("text"), dynamicTicks = TRUE)  %>%
      layout(margin = list(l = 75, r = 75))

    for (x in names(p1$x$layout)[grepl("yaxis", names(p1$x$layout))]) {
      p1[["x"]][["layout"]][[x]][["fixedrange"]] <- TRUE
    }

    return(p1)

  })

  output$sentiment_histogram_ui <- renderUI({
    n_facets <- n_distinct(plot_data()$display_name)
    plotly::plotlyOutput(session$ns("sentiment_histogram"), height = 200 + (100 * n_facets))
  })
}

volumeSentimentQuery <- function() {
  '
  "aggs": {
    "group_by_index": {
      "terms": {
        "field": "_index"
      },
      "aggs" : {
        "sentiment" : {
          "histogram" : {
            "field" : "sentiment_polarity",
            "interval" : 0.05,
            "extended_bounds" : {
              "min" : -1,
              "max" : 1
            }
          }
        }
      }
    }
  }
  '
}

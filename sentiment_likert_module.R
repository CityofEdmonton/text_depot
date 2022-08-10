sentimentLikertUI <- function(id){
  ns <- NS(id)

  tagList(
    description = div(
          "This chart uses a Likert scale to show the spread of sentiment for each dataset.  Positive and Negative sentiment are indicated by blue and red respectively. The height of each bar indicates the percentage of responses that fell into the respective sentiment category."
    ),
    plot = div(
      div(
      style = "max-width: 1200px;",
      shiny::uiOutput(ns("sentiment_likert_plot_ui"))  %>%
        shinycssloaders::withSpinner()
    ),
    br(),br(),
    div(
      align="center",
      style = "max-width: 1200px;",
      sentiment_legend()
    )
    )
  )
}

sentimentLikert <- function(input, output, session,
                            data_set_info,
                            query_info) {

  plot_data_likert <- reactive({

    req(query_info()$num_hits > 0)

    colour_map <- get_sentiment_colourmap() %>%
      arrange(order)

    aggregations <- query_text_depot(query_info = query_info(),
                                    aggregates_json = sentimentBucketsQuery(colour_map))

    plotting_data <- aggregations$aggregations$group_by_index$buckets %>%
      select(index_name = key, doc_count, ends_with(".doc_count")) %>%
      wrangle_likert_data(colour_map = colour_map, data_set_info = data_set_info())

    return(plotting_data)
  })

  output$sentiment_likert_plotly <- plotly::renderPlotly({

    req(plot_data_likert)

    colour_map <- get_sentiment_colourmap()

    p <- plot_data_likert() %>%
      mutate(hover_label = paste0("<b>", display_name, '</b>\n',
                                  "Sentiment Category: ", sentiment_label, '\n',
                                  "Document Count: ", display_count, '\n',
                                  "Document Percent: ", sigfig(display_frac * 100, n = 2), '%', '\n')) %>%
      plot_likert(colour_map = get_sentiment_colourmap(),
                  x_var = display_name,
                  fractions_var = frac_mod,
                  fill_name = Sentiment,
                  hover_label = hover_label)

    p <- p + scale_x_discrete(limits = rev)

    p1 <- plotly::ggplotly(p, tooltip = c("text"))  %>%
      layout(margin = list(l = 75, r = 75))

    p1
  })

  output$sentiment_colour_scale <- renderPlot({
    colour_map <- get_sentiment_colourmap()
    sentimentLegendPlot(colour_mapping = colour_map)
  })

  output$sentiment_likert_plot_ui <- renderUI({
    n_facets <- n_distinct(plot_data_likert()$display_name)
    plotly::plotlyOutput(session$ns("sentiment_likert_plotly"))
  })

}

sentimentBucketsQuery <- function(mapping) {

  bucket_str = build_likert_bins(mapping)

  glue::glue(
    '
    "aggs": {
      "group_by_index": {
        "terms": {
          "field": "_index"
        },
        "aggs" : {
          "sentiment" : {
            "range" : {
              "field" : "sentiment_polarity",
              "keyed" : true,
              "ranges" : [
                <{bucket_str}>
              ]
            }
          }
        }
      }
    }
    ',
    .open = "<{", .close = "}>")
}

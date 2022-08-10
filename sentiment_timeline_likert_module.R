sentimentTimelineLikertUI <- function(id){
  ns <- NS(id)

  tagList(
    description = div(
      "This chart uses a Likert scale, rotated 90°,  to show the spread of sentiment for each dataset by month.  Positive and Negative sentiment are indicated by blue and red respectively. The height of each bar indicates the percentage of responses that fell into the respective sentiment category."
    ),
    plot = div(
      div(
        style = "max-width: 1200px;",
        shiny::uiOutput(ns("sentiment_timeline_likert_plot_ui"))  %>%
          shinycssloaders::withSpinner()
      ),
      div(
        id = ns("sentiment_legend"),
        style = "max-width: 1200px;",
        align="center",
        sentiment_legend()
      )
    )
  )
}

sentimentTimelineLikert <- function(input, output, session,
                                    data_set_info,
                                    query_info) {

  plot_data_likert <- reactive({
    req(query_info()$num_hits > 0)

    # arrange by order to make sure colours show up in correct order in chart
    colour_map <- get_sentiment_colourmap() %>%
      arrange(order)

    aggregations = query_text_depot(query_info = query_info(),
                                    aggregates_json = sentimentBucketsTimelineQuery(colour_map))
    aggregations = parse_aggregates(es_results = aggregations)

    plotting_data <- aggregations$month_counts.buckets %>%
      select(index_name = index, key_as_string, key, doc_count, ends_with(".doc_count")) %>%
      mutate(Date = as.Date(stringr::str_sub(key_as_string, 1, 10))) %>%
      select(-key_as_string) %>%
      wrangle_likert_data(colour_map = colour_map, data_set_info = data_set_info())

    return(plotting_data)
  })

  output$sentiment_timeline_likert_plotly <- plotly::renderPlotly({

    req(plot_data_likert)

    p <- plot_data_likert() %>%
      mutate(hover_label = paste0("<b>", display_name, '</b>\n',
                                  "Sentiment Category: ", sentiment_label, '\n',
                                  "Document Count: ", display_count, '\n',
                                  "Document Percent: ", sigfig(display_frac * 100, n = 2), '%', '\n',
                                  "Date: ", format(Date, format = "%b-%Y"))) %>%
      tidyr::replace_na(list(frac_mod=0)) %>% # this enables proper scaling in the plotly conversion of ggplot (plotly ignores NAs)
      plot_likert(colour_map = get_sentiment_colourmap(),
                  x_var = Date,
                  fractions_var = frac_mod,
                  fill_name = Sentiment,
                  group_var = "display_name",
                  hover_label = hover_label)

    p1 <- plotly::ggplotly(p, tooltip = c("text"),dynamicTicks = "x")  %>%
      layout(margin = list(l = 75, r = 75),
             xaxis = list(
               type = 'date',
               tickformat = "%b<br>%Y"
             ))

    for (x in names(p1$x$layout)[grepl("yaxis", names(p1$x$layout))]) {
      p1[["x"]][["layout"]][[x]][["fixedrange"]] <- TRUE
    }

    p1
  })

  output$sentiment_colour_scale <- renderPlot({
    colour_map <- get_sentiment_colourmap()
    sentimentLegendPlot(colour_mapping = colour_map)
  })

  output$sentiment_timeline_likert_plot_ui <- renderUI({
    n_facets <- n_distinct(plot_data_likert()$display_name)
    plotly::plotlyOutput(session$ns("sentiment_timeline_likert_plotly"), height = 200 + (150 * n_facets))
  })

}

sentimentBucketsTimelineQuery <- function(mapping) {

  bucket_str = build_likert_bins(mapping)

  glue::glue(
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
      }
    }
    ',
    .open = "<{", .close = "}>")
}
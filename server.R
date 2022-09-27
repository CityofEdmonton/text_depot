function(input, output, session) {
  params = isolate(parseQueryString(session$clientData$url_search))
  if ("query" %in% names(params)) { # Did the user specify a query in the URL?
    query_param = params$query
  } else {
    query_param = ""
  }

  es_connection <- connect_ES()

  # The mapping from actual Elastic Search index to alias name.
  # This is needed because when Elastic Search returns results, it returns the
  # index name for each result, not the alias that was originally passed into
  # the query. We dont want to expose the underlying index name to the end user,
  # so we map back to the original name with this data structure.
  data_set_info <- reactive({
    aliases <- get_aliases(session)

    dataset_info <- index_to_alias_mapping(es_connection, aliases) %>%
      mutate(display_name = tools::toTitleCase(gsub("_", " ", alias_name, fixed = TRUE))) %>%
      mutate(db_size = purrr::map_dbl(alias_name, function(index) {
        query_count(es_connection, index = index)
  
      }))

    dataset_info <- dataset_info %>%
      left_join(alias_to_colours_mapping(dataset_info$alias_name), by = "alias_name")

    # computing max/min dates for each index to be used in plotting functions...
    dataset_info <- dataset_info %>%
      group_by(alias_name) %>%
      mutate(date_range = list(stats_for_field(es_connection, index_name, "date")[c("min", "max")])) %>%
      tidyr::unnest_wider(date_range, names_sep = "_") %>%
      mutate(date_range_min = as.Date(date_range_min),
             date_range_max = as.Date(date_range_max)) %>%
      ungroup()

    #computing max/min sentiment_polarity for each index to be used in plotting functions...
    dataset_info <- dataset_info %>%
      group_by(alias_name) %>%
      mutate(sentiment_range = list(stats_for_field(es_connection, index_name, "sentiment_polarity",numeric = TRUE)[c("min", "max")])) %>%
      tidyr::unnest_wider(sentiment_range, names_sep = "_") %>%
      ungroup()

    dataset_info
  })

  observeEvent(input$more_info, {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Additional Information",
      text = tagList(
        div(sprintf("
            Text Depot is a tool for searching and analyzing text data (documents, survey responses etc.)
            at %s. Text Depot has gathered, organized and securely stored multiple text datasets. You can
            instantly search the data and analyze the results. You may also download the results for your
            own further analyses.
          ", get_configs()$location_name),
          br(), br(),
          sprintf("
            You have access to the following %s data sources:
          ", length(data_set_info()$alias_name)),
          style = "text-align: left"
        ),
        br(), br(),
        tableOutput("index_stats") %>% shinycssloaders::withSpinner(proxy.height = "50px"),
        br(), br(),
        "Have another data set to suggest? Email ", a(get_configs()$contact_email, href = paste0("mailto:", get_configs()$contact_email))
      ),
      width = 1000,
      html = TRUE
    )

    output$index_stats <- renderTable({
      req(data_set_info)

      data_set_info() %>%
        group_by(alias_name) %>%
        mutate(avg_sentiment = stats_for_field(es_connection, index_name, "sentiment_polarity", numeric = TRUE)[['avg']],
               avg_sentences = stats_for_field(es_connection, index_name, "num_sentences", numeric = TRUE)[['avg']]) %>%
        ungroup() %>%
        transmute("Data Source" = tools::toTitleCase(gsub("_", " ", display_name)),
                  "Min. Date" = as.character(date_range_min),
                  "Max. Date" = as.character(date_range_max),
                  Documents = format(db_size, nsmall = 0),
                  'Avg. Sentence Count' = avg_sentences,
                  "Avg. Sentiment" = avg_sentiment) %>%
        arrange(`Data Source`)

    }, align = "cccrrr")
  })

  query_info <- shiny::callModule(module = searchBar,
                                  id = "search_bar",
                                  es_connection = es_connection,
                                  data_set_info = data_set_info,
                                  query_param = query_param)

  #------------------------------------
  # query summary - possible additional module
  query_db_size <- reactive({
    req(query_info)
    if (length(query_info()$index) == 0) return(0)
    query_count(es_connection, index = query_info()$index)
  })

  output$query_summary <- renderText({
    req(query_info)
    req(nchar(query_info()$query) > 0)

    output_str = paste0(query_info()$num_hits, " out of ", query_db_size(), " documents matched your search.")

    output_str
  })
  #------------------------------------

  shiny::callModule(module = dataDownload,
                    id = "data_download",
                    query_info = query_info,
                    data_set_info = data_set_info)

  shiny::callModule(module = searchResultsTable,
                    id = "search_results_table",
                    query_info = query_info,
                    results_per_page = RESULTS_PER_PAGE,
                    data_set_info = data_set_info,
                    n_char_no_match = N_CHAR_NO_MATCH,
                    n_char_highlights = N_CHAR_HIGHLIGHTS)

  shiny::callModule(module = volumeTimeline,
                    id = "volume_timeline",
                    data_set_info = data_set_info,
                    query_info = query_info)

  shiny::callModule(module = sentimentTimeline,
                    id = "sentiment_timeline",
                    data_set_info = data_set_info,
                    query_info = query_info)

  shiny::callModule(module = sentiment,
                    id = "sentiment",
                    data_set_info = data_set_info,
                    query_info = query_info)

  shiny::callModule(module = neighbourhoodMap,
                    id = "neighbourhood_map",
                    query_info = query_info)

}

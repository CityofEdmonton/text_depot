dataDownloadUI <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        "
        Click the button below to download your search results. After
        clicking the button, your search results will be
        gathered and prepared for download. You can continue using
        the application during this process, and you will be notified
        when your download is ready.
        ",
        width = 6
      )
    ),
    br(), br(),
    actionButton(ns("download_search_data"), label = "Prepare Download"),
    textOutput(ns("data_loading_message"))
  )
}

dataDownload <- function(input, output, session,
                         query_info, data_set_info) {
  shinyjs::hide("data_loading_message") # initially hide data loading message (will show when user is downloading dataset)

  observeEvent(input$download_search_data, {
    req(query_info)
    req(query_info()$num_hits > 0)

    search_data(NULL)

    # hide data download button while data is prepared, so that user cannot download multiple at same time, replace with message
    shinyjs::hide("download_search_data")
    #shinyjs::hide("ttt_input")
    shinyjs::show("data_loading_message")
    # The async functionality here is "intra-session", and is not officially supported yet.
    # However, the following issue has a good discussion and solution that we use here (see where future call is piped):
    # https://github.com/rstudio/promises/issues/23

    # Note - all variables used within the future below must be "taken out" of their reactive contexts here,
    #        b/c inside the future is not considered a reactive context and therefore it throws an error.
    es_connection <- query_info()$conn
    sources <- query_info()$index
    query_text <- query_info()$query
    search_fields <- query_info()$search_fields
    min_date <- query_info()$min_date
    max_date <- query_info()$max_date
    data_set_info <- data_set_info()
    ttt_input <- FALSE #input$ttt_input
    query_info <- query_info()

    future({
      # TODO: Should we remove refs to Text Themer?
      if (ttt_input == TRUE) {
        # the Text Theming Tool output is currently disabled, since the results are not looking great.
        data_return = query_text_depot(query_info,
                                       source_json = source_body(c("id",
                                                                   "text",
                                                                   "date")),
                                       highlights_json = NULL,
                                       from = 0,
                                       size = 500) %>%
          parse_hits(data_set_info) %>%
          select(response_id = id,
                 date,
                 text) %>%
          mutate(text = gsub("^((\\w+\\W+){100}\\w+).*$","\\1",text))
      } else {
        data_return = query_text_depot(query_info,
                                       source_json = source_body(c("id",
                                                                   "num_chars",
                                                                   "num_sentences",
                                                                   "sentiment_polarity",
                                                                   "sentiment_subjectivity",
                                                                   "date",
                                                                   "source_url",
                                                                   "source_title",
                                                                   "parent_source_url",
                                                                   "parent_source_title")),
                                       highlights_json = highlights_body(c("text",
                                                                           "source_title",
                                                                           "parent_source_title")),
                                       from = 0,
                                       size = 10000) %>%
          parse_hits(data_set_info) %>%
          group_by(`id`) %>%
          mutate(highlights = collapse_highlights(highlight.source_title,
                                                  highlight.parent_source_title,
                                                  highlight.text,
                                                  collapse = " ... ",
                                                  rm_em = TRUE)) %>%
          ungroup() %>%
          data.table() %>%
          select(Type = display_name,
                 Date = date,
                 `Source Title` = source_title,
                 `Source URL` = source_url,
                 `Parent Source Title` = parent_source_title,
                 `Parent Source URL` = parent_source_url,
                 `Document Sentiment` = sentiment,
                 `Document Sentiment Subjectivity` = sentiment_subj,
                 `Search Similarity` = similarity,
                 `Matching Text` = highlights)
      }
      data_return
    }) %...>%
      search_data() %...!%
      (function(e){
        search_data(NULL)
        warning(e)
      })
    NULL
  })

  search_data <- reactiveVal()

  output$data_loading_message <- renderText("Preparing your data - this may take a few minutes. You can not download more than one file at a time...")

  # This observe event is needed to get around some peculiar behaviour
  # of downloadHandler that stops control being given back to the application
  # during the computation of the future (despite the considerations in
  # https://github.com/rstudio/promises/issues/23) We are effectively
  # preventing the the filehandler from being called until search_data is
  # finished computing.
  observeEvent({input$download_search_data; search_data()}, {
    req(nrow(search_data()) > 0)
    output$download_search <- downloadHandler(contentType = "text/csv",
                                              filename = function() {
                                                paste0(query_info()$query, '_search_responses.csv')
                                              },
                                              content = function(file) {
                                                shinyjs::hide("download_search") # stop data download being attempted twice once modal is shown

                                                # reset data download button
                                                shinyjs::hide("data_loading_message")
                                                shinyjs::show("download_search_data")
                                                #shinyjs::show("ttt_input")

                                                search_data() %>%
                                                  write.csv(file = file,
                                                            row.names = FALSE)
                                                search_data(NULL)
                                              }
    )

    modalDialog(
      title = "Download Data",
      size = "l",
      downloadButton(session$ns("download_search"), label = 'Download Data'),
      easyClose = TRUE
    ) %>%
      showModal()
    # just to catch all, when modal closes, reset data download button again
    shinyjs::hide("data_loading_message")
    shinyjs::show("download_search_data")
    #shinyjs::show("ttt_input")
  })
}
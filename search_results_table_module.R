searchResultsTableUI <- function(id, include_table_options = FALSE){
  ns <- NS(id)

  tagList(
    div(
      id = ns("results_wrapper"),
      class = "shinyjs-hide", # Hide the div initially
      if (include_table_options) {
        shinyWidgets::dropdown(
          uiOutput(ns("column_picker")),
          label = "table options",
          circle = FALSE,
          status = "primary",
          icon = icon("cog"),
          size = "xs",
          inputId = ns("table_options"))
      }
      ),
    DT::dataTableOutput(ns("results")) %>% shinycssloaders::withSpinner()
  )
}

searchResultsTable <- function(input, output, session,
                               query_info,
                               results_per_page,
                               data_set_info,
                               n_char_no_match,
                               n_char_highlights) {
  hits <- reactiveVal(c())
  aggregations <- reactiveVal(c())
  rowstart <- reactiveVal(c())
  default_columns = c("Matching Text", "Type", "Date")
  column_mapping = c(
    'Matching Text' = 'matches',
    'Type' = 'display_name',
    'Date' = 'date',
    'Characters' = 'num_chars',
    'Sentences' = 'num_sentences',
    'Sentiment' = 'sentiment',
    'Similarity' = 'similarity'
  )

  # When search is clicked, get the first page of hits (plus 1 to enable paging buttons).
  observeEvent(query_info(), {
    query_info <- query_info()
    hits(c()) # Remove previous hits
    rowstart(0) # Reset place in datatable output

    if ( (nchar(query_info$query) == 0) |
         (length(query_info$index) == 0) |
         (length(query_info$search_fields) == 0) |
         (query_info$num_hits == 0)) {
      shinyjs::hideElement(session$ns("results_wrapper"), asis = TRUE)
    } else {
      shinyjs::showElement(session$ns("results_wrapper"), asis = TRUE)

      req(query_info)

      result <- query_text_depot(query_info,
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
                                 highlights_json = highlights_body(highlight_cols = c("text",
                                                                                      "source_title",
                                                                                      "parent_source_title"),
                                                                   n_char_no_match = n_char_no_match,
                                                                   n_char_highlights = n_char_highlights),
                                 from = 0,
                                 size = (results_per_page + 1))

      if (is.character(result)) {
        # There was an error
        shinyjs::alert(result)
      } else {
        hits_df = parse_hits(result, data_set_info())
        hits(hits_df)
      }
    }
  }, ignoreNULL = FALSE)

  # This is triggered when the user clicks on "Next" to go to the next page. It is also
  # triggered again when the hits() data is updated. results_rows_current contains the indices
  # of the rows that are being displayed on the current page.
  observeEvent(input$results_rows_current, {
    req(query_info)
    query_info <- query_info()
    req(nrow(hits()) > results_per_page) # Return if we have a small number of results
    req(input$results_rows_current)
    isolate(total_rows <- query_info()$num_hits) # isolate because sometimes aggregates is done before hits, which causes error in if below

    # for first page
    #          length(input$results_rows_current) = 30, nrow(hits()) = 31, results_per_page = 30 >> if statement FALSE
    # for second page
    #          length(input$results_rows_current) = 1, nrow(hits()) = 31, results_per_page = 30 >> if statement TRUE, initially
    # after new hits are pulled, second page:
    #          length(input$results_rows_current) = 30, nrow(hits()) = 61, results_per_page = 30 >> if statement FALSE
    #
    # If the current page is not a full page and we have not loaded all data, then load more data!
    if ( (length(input$results_rows_current) < results_per_page) &
         (nrow(hits()) < total_rows)) {
      rowstart(input$results_rows_current[1] - 1) # results_rows_current starts indexing at 1, rowstart starts at 0.

      next_chunk = query_text_depot(query_info(),
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
                                    highlights_json = highlights_body(highlight_cols = c("text",
                                                                                         "source_title",
                                                                                         "parent_source_title"),
                                                                      n_char_no_match = n_char_no_match,
                                                                      n_char_highlights = n_char_highlights),
                                    from = nrow(hits()),
                                    size = results_per_page)

      if (is.character(next_chunk)) {
        # There was an error
        shinyjs::alert(next_chunk)
        hits(c())
      } else {
        next_chunk_hits = parse_hits(next_chunk, data_set_info())
        hits(bind_rows(hits(), next_chunk_hits))
      }
    }
  })

  formatted_hits <- reactive({
    hits = hits()
    req(hits)
    req(nrow(hits) > 0)

    hits
  })

  output$column_picker <- renderUI({
    shinyWidgets::pickerInput(inputId = session$ns("show_columns"),
                              label = "Show Columns",
                              choices = names(column_mapping),
                              selected = default_columns,
                              multiple = TRUE)
  })

  # Need a reactive function here, because if the "table options" has not
  # been clicked yet, then input$show_columns will be NULL
  chosen_cols <- reactive({
    if (!is.null(input$show_columns)) {
      return(input$show_columns)
    } else {
      return(default_columns)
    }
  })

  # This is regenerated when hits() is updated. rowstart() keeps track of which
  # row (and therefore page) we are now on.
  output$results <- DT::renderDataTable({
    isolate(row_start <- rowstart())
    total_rows = query_info()$num_hits
    req(formatted_hits)
    req(data_set_info)

    results_table <- formatted_hits() %>%
      data.table::data.table() %>%
      dplyr::select(all_of(column_mapping)) %>% # Rename columns to human-readable
      select(all_of(chosen_cols())) %>%
      DT::datatable(rownames = FALSE,
                    selection = 'single',
                    escape = FALSE,
                    options = list(
                      ordering = FALSE,
                      pagingType = 'simple',
                      pageLength = results_per_page,
                      displayStart = row_start,
                      dom = 'Btip',
                      language = list(
                        info = paste0('Showing _START_ to _END_ of ', total_rows, '  entries')#  (loaded _TOTAL_) to show total in data frame
                      ),
                      headerCallback = JS("
                        function(thead, data, start, end, display) {
                          $(thead).remove();
                        }
                      ")
                    )
      )

    if ("Type" %in% chosen_cols()) {
      results_table = DT::formatStyle(results_table,
                                      "Type",
                                      color = DT::styleEqual(data_set_info()$display_name, data_set_info()$colours),
                                      "white-space" = "nowrap")
    }

    if ("Date" %in% chosen_cols()) {
      results_table = DT::formatStyle(results_table,
                                      "Date",
                                      "white-space" = "nowrap")
    }

    results_table
  })

  # Triggered when a row is clicked
  observeEvent(input$results_rows_selected, {
    row = formatted_hits()[input$results_rows_selected, ]

    color_row = get_sentiment_colourmap_for_value(row$sentiment)
    doc = get_document_by_id(query_info()$conn, row$id, row$alias_name)
    matching_text = collapse_highlights(row$highlight.source_title,
                                        row$highlight.parent_source_title,
                                        row$highlight.text,
                                        collapse = " ... <br />")
    neighbourhoods = ifelse(length(doc$neighbourhoods) > 0, paste0(doc$neighbourhoods, collapse = ", "), "None")

    display_text = doc$text %>%
      gsub("\n\\s*\n\\s*\n\\s*", "\n\n\n" ,.) %>% # Replace 3+ newlines with just 3 newlines
      str_replace_all("\t", "    ")
      # str_replace_all(" ", "&nbsp;") %>%
      # str_replace_all("\n", "<br />") %>%
      # HTML()

    shinyWidgets::confirmSweetAlert( # confirmSweetAlert allows us to track when the "OK" button is clicked
      session = session,
      inputId = session$ns("doc_info"),
      title = "Matching Document Information",
      type = "", # no icon
      btn_labels = "OK",
      text = tagList(
        strong(sprintf('Query: "%s"', query_info()$query),
               style = "font-size: 14px; padding: 5px"),
        div(
          style = sprintf("background-color: %s; border-radius: 10px; padding: 5px; margin: 10px; font-size: 14px", main_div_background()),
          tags$u(format_title(row$parent_source_title, row$parent_source_url, add_link = TRUE), style = "font-size: 16px;"), br(),
          span(format_title(row$source_title, row$source_url, add_link = TRUE), style = "font-size: 16px;"), br(),
          br(),
          strong("Date:"), row$date, br(),
          strong("Data Set:"), row$display_name, br(),
          HTML(sprintf("<strong>Sentiment:</strong> <span style='color: %s'>%s</span> (%s)", color_row$colour, color_row$label, row$sentiment)), br(),
          strong("Detected Neighbourhoods:"), neighbourhoods, br(),
          br(),
          strong(sprintf("Full Document Text (%s Characters, %s Sentences)", row$num_chars, row$num_sentences)),
          br(),
          tags$textarea(id="foo", rows = 16, style = "width: 90%; resize:vertical", readonly = TRUE, display_text)
        ),
        # Only turn on summaries if we've specified an API Host,
        # and this is a long piece of text:
        if (!is.null(get_configs()$embedding_api_host) & (row$num_chars > 1000)) {
          actionButton(session$ns("summary_button"), 
                       "Summarize Text", 
                       icon("robot"), 
                       class = "btn-info")
        } else {
          ""
        },
        uiOutput(session$ns("summary_section")),
      ),
      width = 1000,
      html = TRUE
    )

    observeEvent(input$doc_info, {
      output$summary_section = renderText({""})
    })
    
    observeEvent(input$summary_button, {
      req(input$summary_button)
      req(input$summary_button != 0)
      configs = get_configs()

      shinycssloaders::showPageSpinner(caption = "🤖 Reading Text... 🤖", type = 4, color = main_text_color())
      summary = get_document_summary(display_text, 
                                     configs$embedding_api_host,
                                     configs$embedding_api_user,
                                     configs$embedding_api_password)
      output$summary_section = renderUI({
        div(
          style = sprintf("width: 90%%; margin: auto; background-color: %s; border-radius: 10px; padding: 5px; font-size: 15px", secondary_div_background()),
          strong("AI-Generated Text Summary"),
          br(), br(),
          summary,
          br(), br(),
          span(style = "font-size: 13px; font-style: italic", "Note: This text is automatically generated by an AI language model, and should be reviewed.")
        )
      })
      shinyjs::hide("summary_button")
      shinycssloaders::hidePageSpinner()
    }, ignoreInit = TRUE, once = TRUE) # https://community.rstudio.com/t/issue-with-r-shiny-modal-dialog-and-observe-event-on-action-button/9501
  })

}

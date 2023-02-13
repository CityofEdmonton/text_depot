searchBarUI <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        fluidRow(
          column(
            uiOutput(ns("query_text_ui")),
            width = 9
          ),
          column(
            # Cant use "dropdownButton" because of https://github.com/dreamRs/shinyWidgets/issues/38
            shinyWidgets::dropdown(
              # https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-simple-query-string-query.html#simple-query-string-syntax',
              actionLink(ns("advanced_query_info"), label = "Click here for documentation on Advanced Queries", style = "color: blue; font-weight: bold"),

              br(), br(),

              shinyWidgets::checkboxGroupButtons(ns("search_fields"),
                                                 "Search Fields",
                                                 choices = search_fields(),
                                                 status = 'info',
                                                 selected = search_fields(),
                                                 checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                                  no = icon("remove", lib = "glyphicon"))),
              uiOutput(ns("date_range_ui"), style = "padding-left: 10px"),
              uiOutput(ns("sentiment_range_ui")),
              shinyWidgets::radioGroupButtons(ns("sort_by"),
                                              label = "Sort Results By",
                                              choices = c("Search Score" = "score", 
                                                          "Date (Oldest First)" = "date_asc", 
                                                          "Date (Newest First)" = "date_desc"),
                                              status = "success",
                                              selected = "score"),
              uiOutput(ns("data_source_ui")),

              # Only turn on AI search if we've specified an API Host:
              if (!is.null(get_configs()$embedding_api_host)) {
                tagList(
                  shinyWidgets::switchInput(ns("use_embeddings"),
                                            label = "🤖 AI Search 🤖",
                                            labelWidth = "110px",
                                            value = FALSE,
                                            inline = TRUE),
                  span("BETA", class = "label label-info") # Bootstrap label
                )
              },

              circle = FALSE,
              status = "primary",
              icon = icon("cog"),
              width = "600px",
              tooltip = shinyWidgets::tooltipOptions(title = "Advanced Options")
            ),
            width = 3
          ),
          style = "
            background-color: #D3EDF0;
            margin: 0 0 0 0;
            padding: 20px 0 5px 0;
            border-radius: 10px;
            vertical-align = center;
            min-height: 75px; // Should be 50 + padding
          "
        ),
        width = 6
      )
    )
  )
}

searchBar <- function(input, output, session,
                      es_connection,
                      data_set_info,
                      query_param) {
  output$query_text_ui <- renderUI({
    shinyWidgets::searchInput(inputId = session$ns("query_text"),
                              label = NULL,
                              value = query_param,
                              placeholder = "Type Search Here",
                              btnSearch = icon("search"),
                              btnReset = icon("times"),
                              width = "100%")
  })

  observeEvent(input$advanced_query_info, {
    modalDialog(
      title = "Text Depot - Advanced Queries",
      size = "l",
        '
        Text Depot provides basic keyword search (enter the words you are interested 
        in), but also provides a way to put together advanced queries using several 
        logical operators that you can use in your searches:
        ',
        br(),
        br(),
      fluidRow(
        column(strong("+ (plus sign)"), width = 3), 
        column("Indicates that the term following it must be present in the search results.", width = 6)
      ), 
      br(),
      fluidRow(
        column(strong("- (minus sign)"), width = 3), 
        column("Indicates that the term following it must be absent from the search results.", width = 6)
      ), 
      br(),
      fluidRow(
        column(strong('" " (quotation marks)'), width = 3), 
        column("Search for an exact phrase. Results will only include documents that contain the exact phrase.", width = 6)
      ), 
      br(),
      fluidRow(
        column(strong("* (asterisk)"), width = 3), 
        column('Multiple character wildcard. For example, a search for "bus*" would match both "buses" and "business".', width = 6)
      ), 
      br(),
      fluidRow(
        column(strong("? (question mark)"), width = 3), 
        column('Wildcard, but it matches only one character. For example, a search for "te?t" would match "test" and "text" but not "tenant".', width = 6)
      ), 
      br(),
      fluidRow(
        column(strong("~ (tilde)"), width = 3), 
        column("Fuzzy matching. It will match documents that are similar to the search term, with a specified level of tolerance for differences, in the format [term]~[edit distance]. For example, a search for test~1 would match test, text, best, and other words that are an edit-distance of 1 away from your query. This is useful when including mis-spellings.", width = 6)
      ), 
      br(),
      fluidRow(
        column(strong("AND"), width = 3), 
        column('Returns documents that match all of the search terms. Example: "dog AND cat" returns documents containing both "dog" and "cat".', width = 6)
      ), 
      br(),
      fluidRow(
        column(strong("OR"), width = 3), 
        column('Returns documents that match at least one of the search terms. Example: "dog OR cat" returns documents containing either "dog" or "cat" or both.', width = 6)
      ), 
      br(),
      fluidRow(
        column(strong("NOT"), width = 3), 
        column('Excludes documents that match the search term. Example: "dog NOT cat" returns documents containing "dog" but not "cat".', width = 6)        
      ),
      br(),
      'Note that you can combine the above operators, and group them with parenthesis (). For example: "safety AND (bus OR train) -budget".',
      easyClose = TRUE
    ) %>%
      showModal()
  })

  # Set the focus on the search bar, once input is loaded
  execute_at_first_input(runjs(sprintf(
    'document.getElementById("%s").focus();', session$ns("query_text_text")
  )))

  output$data_source_ui <- renderUI({
    choices = data_set_info()$alias_name
    names(choices) = paste0(data_set_info()$display_name, " (", data_set_info()$db_size, " docs)")
    shinyWidgets::pickerInput(session$ns("data_sources"),
                              label = "Data Sources",
                              choices = choices,
                              selected = choices,
                              multiple = TRUE,
                              options = shinyWidgets::pickerOptions(
                                actionsBox = TRUE,
                                title = "Please Select a Data Source"
                              ))
  })

  # Setting initial value so that we dont have to load the UI
  # element before setting the date ranges:
  date_stats <- reactive({
    stats_for_field(es_connection, data_set_info()$alias_name, "date")
  })

  selected_date_range <- reactive({
    range = c(as.Date(date_stats()$min), as.Date(date_stats()$max))

    # Check that UI element exists and that valid dates are there:
    if (!is.null(input$date_range)) {
      if ((!is.na(input$date_range[1])) &
          (!is.na(input$date_range[2]))) {
        range = input$date_range
      }
    }

    return(range)
  })

  output$date_range_ui <- renderUI({  
    min = as.Date(date_stats()$min)
    max = as.Date(date_stats()$max)
    dateRangeInput(
      inputId = session$ns("date_range"),
      label = "Date Range",
      start = min,
      end = max)
  })
  
  sentiment_stats <- reactive({
    stats_for_field(es_connection, data_set_info()$alias_name, "sentiment_polarity",numeric = TRUE)
  })

  selected_sentiment_range <- reactive({
    range = c(sentiment_stats()$min, sentiment_stats()$max)

    # Check that UI element exists and that valid sentiment are there:
    if (!is.null(input$sentiment_range)) {
      if ((!is.na(input$sentiment_range[1])) &
          (!is.na(input$sentiment_range[2]))) {
        range = input$sentiment_range
      }
    }

    return(range)
  })

  output$sentiment_range_ui <- renderUI({
    min_val = sentiment_stats()$min
    max_val = sentiment_stats()$max
    sliderInput(
      inputId = session$ns("sentiment_range"),
      label = "Sentiment Range",
      min = min_val,
      max = max_val,
      value = c(min_val,max_val),step=0.05)
  })


  selected_data_sources <- reactive({
    data_sets <- data_set_info()$alias_name
    # On page load, input$data_sources is null because it's buried in the dropdown options
    if (!is.null(input$data_sources)) {
      data_sets = input$data_sources
    }
    return(data_sets)
  })

  cleaned_query <- reactive({
    req(input$query_text)
    query = input$query_text
    query = gsub('"', '\"', query)
    query = gsub("'", "\"", query)

    query
  })

  aggregations <- reactiveVal(c())

  use_embeddings_setting <- reactive({
    ifelse(is.null(input$use_embeddings), FALSE, input$use_embeddings)
  })

  # folowing is for computing num_hits. This is used in this module, but also in other modules.
  # Get aggregates for ALL results when search is clicked:
  observe({
    aggregations(c())
    req(nchar(cleaned_query()) > 0)
    req(length(selected_data_sources()) > 0)
    req(length(input$search_fields) > 0)

    aggregations = query_text_depot(query_info = list(conn = es_connection,
                                                      query = cleaned_query(),
                                                      index = selected_data_sources(),
                                                      search_fields = input$search_fields,
                                                      min_score = 0,
                                                      min_date = selected_date_range()[1],
                                                      max_date = selected_date_range()[2],
                                                      min_sentiment = selected_sentiment_range()[1],
                                                      max_sentiment = selected_sentiment_range()[2],
                                                      use_embeddings = use_embeddings_setting()),
                                    aggregates_json = statsPerIndexQuery())

    aggregations = parse_aggregates(aggregations)

    if (is.character(aggregations)) {
      # There was an error
      shinyjs::alert(aggregations)
      aggregations(c())
    } else {
      parsed <- parse_aggregates(aggregations)

      aggregations(aggregations)
    }

  })

  query_info <- reactive({
    list(conn = es_connection,
         query = cleaned_query(),
         index = selected_data_sources(),
         search_fields = input$search_fields,
         min_score = 0,
         min_date = selected_date_range()[1],
         max_date = selected_date_range()[2],
         min_sentiment = selected_sentiment_range()[1],
         max_sentiment = selected_sentiment_range()[2],
         sort_by = input$sort_by,
         num_hits = sum(aggregations()$counts_by_index$doc_count),
         use_embeddings = use_embeddings_setting())
  })

  return(query_info)
}

statsPerIndexQuery <- function(){
  '
  "aggs": {
    "group_by_index": {
      "terms": {
        "field": "_index"
      }
    }
  }
  '
}

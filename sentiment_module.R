
sentimentUI <- function(id){
  ns <- NS(id)

  sentiment_timeline <- sentimentTimelineUI(ns("sentiment_timeline"))

  fluidPage(
    tagList(
      fluidRow(
        style = "max-width: 1200px;",
        column(
          width = 3,
          shinyWidgets::pickerInput(
            inputId = ns("select_chart"),
            inline = TRUE,
            label = "",
            choices = c("Average Sentiment over Time" = ns("pick_sentiment_timeline"),
                        "Distribution of Sentiment" = ns("pick_sentiment_histogram"),
                        "Sentiment Categories" = ns("pick_sentiment_likert"),
                        "Sentiment Categories over Time"=ns("pick_sentiment_timeline_likert")),
            selected = "pick_sentiment_timeline"
          )
          ),
        column(
          width = 9,
          br(),
          tabsetPanel(
            id = ns("sentiment_descriptions"),
            type = "hidden",
            #tabPanelBody(ns("pick_sentiment_timeline"), sentimentTimelineUI(ns("sentiment_timeline"))[["description"]]),
            tabPanelBody(ns("pick_sentiment_timeline"), sentiment_timeline[["description"]]),
            tabPanelBody(ns("pick_sentiment_histogram"), sentimentHistogramUI(ns("sentiment_histogram"))[["description"]]),
            tabPanelBody(ns("pick_sentiment_likert"), sentimentLikertUI(ns("sentiment_likert"))[["description"]]),
            tabPanelBody(ns("pick_sentiment_timeline_likert"), sentimentTimelineLikertUI(ns("sentiment_timeline_likert"))[["description"]])
          )
        )
      ),
      tabsetPanel(
        id = ns("sentiment_tabs"),
        type = "hidden",
        tabPanelBody(ns("pick_sentiment_timeline"), sentimentTimelineUI(ns("sentiment_timeline"))[["plot"]]),
        tabPanelBody(ns("pick_sentiment_histogram"), sentimentHistogramUI(ns("sentiment_histogram"))[["plot"]]),
        tabPanelBody(ns("pick_sentiment_likert"), sentimentLikertUI(ns("sentiment_likert"))[["plot"]]),
        tabPanelBody(ns("pick_sentiment_timeline_likert"), sentimentTimelineLikertUI(ns("sentiment_timeline_likert"))[["plot"]])
        )
      )
    )
}

sentiment <- function(input, output, session,
                            data_set_info,
                            query_info) {

  observeEvent(input$select_chart, {
    updateTabsetPanel(session, "sentiment_tabs", selected = input$select_chart)
  })

  observeEvent(input$select_chart, {
    updateTabsetPanel(session, "sentiment_descriptions", selected = input$select_chart)
  })

  shiny::callModule(module = sentimentTimeline,
                    id = "sentiment_timeline",
                    data_set_info = data_set_info,
                    query_info = query_info)

  shiny::callModule(module = sentimentTimelineLikert,
                    id = "sentiment_timeline_likert",
                    data_set_info = data_set_info,
                    query_info = query_info)

  shiny::callModule(module = sentimentHistogram,
                    id = "sentiment_histogram",
                    data_set_info = data_set_info,
                    query_info = query_info)

  shiny::callModule(module = sentimentLikert,
                    id = "sentiment_likert",
                    data_set_info = data_set_info,
                    query_info = query_info)

}
sentimentUI <- function(id){
  ns <- NS(id)

  # different parts of the following UI elements used in 2 places below, so compute once here:
  sentiment_timeline <- sentimentTimelineUI(ns("sentiment_timeline"))
  sentiment_histogram <- sentimentHistogramUI(ns("sentiment_histogram"))
  sentiment_likert <- sentimentLikertUI(ns("sentiment_likert"))
  sentiment_likert_timeline <- sentimentTimelineLikertUI(ns("sentiment_timeline_likert"))

  fluidPage(
    tagList(
      fluidRow(
        style = "max-width: 1200px;",
        column(
          width = 3,
          shinyWidgets::pickerInput(
            inputId = ns("select_chart"),
            inline = TRUE,
            label = "",
            choices = c("Average Sentiment over Time" = ns("pick_sentiment_timeline"),
                        "Distribution of Sentiment" = ns("pick_sentiment_histogram"),
                        "Sentiment Categories" = ns("pick_sentiment_likert"),
                        "Sentiment Categories over Time"=ns("pick_sentiment_timeline_likert")),
            selected = "pick_sentiment_timeline"
          )
          ),
        column(
          width = 9,
          br(),
          tabsetPanel(
            id = ns("sentiment_descriptions"),
            type = "hidden",
            tabPanelBody(ns("pick_sentiment_timeline"), sentiment_timeline[["description"]]),
            tabPanelBody(ns("pick_sentiment_histogram"), sentiment_histogram[["description"]]),
            tabPanelBody(ns("pick_sentiment_likert"), sentiment_likert[["description"]]),
            tabPanelBody(ns("pick_sentiment_timeline_likert"), sentiment_likert_timeline[["description"]])
          )
        )
      ),
      tabsetPanel(
        id = ns("sentiment_tabs"),
        type = "hidden",
        tabPanelBody(ns("pick_sentiment_timeline"), sentiment_timeline[["plot"]]),
        tabPanelBody(ns("pick_sentiment_histogram"), sentiment_histogram[["plot"]]),
        tabPanelBody(ns("pick_sentiment_likert"), sentiment_likert[["plot"]]),
        tabPanelBody(ns("pick_sentiment_timeline_likert"), sentiment_likert_timeline[["plot"]])
        )
    ))
}

sentiment <- function(input, output, session,
                            data_set_info,
                            query_info) {

  observeEvent(input$select_chart, {
    updateTabsetPanel(session, "sentiment_tabs", selected = input$select_chart)
  })

  observeEvent(input$select_chart, {
    updateTabsetPanel(session, "sentiment_descriptions", selected = input$select_chart)
  })

  shiny::callModule(module = sentimentTimeline,
                    id = "sentiment_timeline",
                    data_set_info = data_set_info,
                    query_info = query_info)

  shiny::callModule(module = sentimentTimelineLikert,
                    id = "sentiment_timeline_likert",
                    data_set_info = data_set_info,
                    query_info = query_info)

  shiny::callModule(module = sentimentHistogram,
                    id = "sentiment_histogram",
                    data_set_info = data_set_info,
                    query_info = query_info)

  shiny::callModule(module = sentimentLikert,
                    id = "sentiment_likert",
                    data_set_info = data_set_info,
                    query_info = query_info)

}
neighbourhoodMapUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    tagList(
      fluidRow(
        style = "max-width: 1200px;margin-top:-20px;",
        column(
          width = 2,
          shinyWidgets::pickerInput(
            inputId = ns("select_map"),
            inline = TRUE,
            label = "",
            choices = c("Count",
                        "Sentiment"),
            selected = "Count"
          )
        ),
        column(
          width = 5,
          br(),
          tabsetPanel(
            id = ns("map_descriptions"),
            type = "hidden",
            tabPanelBody(ns("map"), shiny::uiOutput(ns("description_ui")))
          )
        ),
        column(
          width = 1,
          br(),
          tabsetPanel(
            id = ns("map_exclude"),
            type = "hidden",
            tabPanelBody(ns("map"), shiny::uiOutput(ns("exclude_ui")))
          )
        )
      ),
      br(), br(),
      tabsetPanel(
        id = ns("map_tabs"),
        type = "hidden",
        tabPanelBody(ns("map"), shiny::uiOutput(ns("tabs_ui")))
      )
    )
  )
}

neighbourhoodMap <- function(input, output, session,
                      query_info) {
  
  observeEvent(input$select_map, {
    updateTabsetPanel(session, "map_tabs", selected = input$select_map)
  })
  
  observeEvent(input$select_map, {
    updateTabsetPanel(session, "map_descriptions", selected = input$select_map)
  })
  
  observeEvent(input$select_map, {
    shiny::callModule(module = mapPlot,
                      id = "map_plot",
                      query_info = query_info,
                      selected = input$select_map)
  })
  
  output$description_ui = renderUI({
    req(input$select_map)
    mapPlotUI(session$ns("map_plot"), input$select_map)[["description"]]
  })
  
  output$exclude_ui = renderUI({
    req(input$select_map)
    mapPlotUI(session$ns("map_plot"), input$select_map)[["exclude"]]
  })
  
  output$tabs_ui = renderUI({
    req(input$select_map)
    mapPlotUI(session$ns("map_plot"), input$select_map)[["plot"]]
  })
  
}

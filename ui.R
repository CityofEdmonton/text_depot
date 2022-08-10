ui <- fluidPage(
  tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Open+Sans:400,700"),

  useShinyjs(),

  # Allow user to search using enter button:
  tags$head(HTML('
    <script>
      window.onload = function() {
        document.getElementById("search_bar-query_text_text").focus();
      };
    </script>
  ')),

  fluidRow(
    column(
      a(img(src = "text_depot_icon/TextDepotIcon_Image_S.jpg", width = 105, style = "float: left; padding: 10px 10px 0 0"),
        href = "https://github.com/CityofEdmonton/text_depot"),
      span("TEXT DEPOT", style = 'font-weight: 700;
                                  font-size: 35px;
                                  font-family: "Open Sans";
                                  margin: 20px 0 20px 0;
                                  color: #32566F'),
      br(),
      span(paste0("A simplified way to search and analyze topics of interest related to ",
                  get_configs()$location_name),
           style = "color: #32566F"),
      br(),
      actionLink("more_info", paste0("Click for More Info")),
      width = 12
    )
  ),

  br(),

  searchBarUI("search_bar"),

  br(),

  fluidRow(
    column(
      textOutput("query_summary"),
      width = 6
    )
  ),

  br(),

  tabsetPanel(
    id = "main_tabset",
    tabPanel("Matching Documents",
      br(),
      searchResultsTableUI("search_results_table"),
      icon = icon("list")
    ),
    tabPanel("Timeline",
      br(),
      volumeTimelineUI("volume_timeline"),
      icon = icon("chart-line")
    ),
    tabPanel("Sentiment",
      sentimentUI("sentiment"),
      icon = icon("heartbeat")
    ),
    tabPanel("Neighbourhoods",
      br(),
      neighbourhoodMapUI("neighbourhood_map"),
      icon = icon("city")
    ),
    tabPanel("Download",
      br(),
      dataDownloadUI("data_download"),
      icon = icon("download")
    )
  ),

  br()
)

mapPlotUI <- function(id, selected){
  ns <- NS(id)
  
  if (selected == "Sentiment") {
    tagList(
      description = div(
        "This map shows the search results that mention each neighbourhood. The colors correspond to the average sentiment expressed in each of those documents."
      ),
      plot = div(
        div(
          style = "max-width: 1200px;",
          shiny::uiOutput(ns("map_plot_output_ui"))  %>%
            shinycssloaders::withSpinner()
        ),
        div(
          align="center",
          style = "max-width: 1200px;",
          sentiment_legend()
        )
      ),
      exclude = div(
        div(
          style = "max-width: 1200px;",
          uiOutput(ns("neighbourhoods_exclude_ui")),
        )
      )
    )
  } else if (selected == "Count") {
    tagList(
      description = div(
        "This map shows the number of documents in the search results that mention each neighbourhood."
      ),
      plot = div(
        div(
          style = "max-width: 1200px;",
          shiny::uiOutput(ns("map_plot_output_ui"))  %>%
            shinycssloaders::withSpinner()
        )
      ),
      exclude = div(
        div(
          style = "max-width: 1200px;",
          uiOutput(ns("neighbourhoods_exclude_ui")),
        )
      )
    )
  }
}

mapPlot <- function(input, output, session,
                            query_info, selected) {
  
  neighbourhoods = sf::st_read(get_configs()$neighbourhoods_file, quiet = TRUE)
  
  output$neighbourhoods_exclude_ui <- renderUI({
    shinyWidgets::dropdownButton(
      'Some neighbourhoods can overshadow results for other neighbourhoods. You can exclude neighbourhoods from this analysis by entering them in the box below.',
      br(),
      selectizeInput(session$ns("neighbourhoods_exclude"),
                     label = "Exclude Neighbourhoods:",
                     choices = sort(neighbourhoods$descriptive_name),
                     selected = c(),
                     multiple = TRUE,
                     options = list(placeholder = 'Type here')),
      circle = FALSE,
      status = "primary",
      icon = icon("gear"),
      width = "300px",
      tooltip = shinyWidgets::tooltipOptions(title = "Map Options")
    )
  })
  
  plot_alt_map = reactive({
    req(query_info()$num_hits > 0)
    
    colour_map = get_sentiment_colourmap()
    
    aggregations = query_text_depot(query_info = query_info(),
                                    aggregates_json = sentimentNeighbourhoodsQuery())
    aggregations = parse_aggregates(es_results = aggregations)
    
    hood_stats = aggregations$hoods.names.buckets %>%
      group_by(key) %>%
      summarize(
        doc_count = sum(doc_count),
        avg_sentiment = mean(neighbourhood_to_sentiment.avg_sentiment.value),
        .groups = "drop"
      ) %>%
      rename(name = key) %>%
      mutate(label1 = paste0(name, "<br/>", "Doc Count: ", doc_count, "<br/>",
                           "Sentiment: ", round(avg_sentiment, 2))) %>%
      mutate(label2 = paste0(name, "<br/>", "Doc Count: ", doc_count))
    
    hoods_df = neighbourhoods %>%
      left_join(hood_stats, by = c("descriptive_name" = "name"))
    na_ind = which(is.na(hoods_df$avg_sentiment))
    hoods_df$doc_count[na_ind] = 0
    hoods_df$label1[na_ind] = paste0(hoods_df$name[na_ind], ": ", NA)
    hoods_df$label2[na_ind] = paste0(hoods_df$name[na_ind], ": ", NA)
    
    if (length(input$neighbourhoods_exclude) > 0) {
      hoods_df = dplyr::filter(hoods_df, !(descriptive_name %in% input$neighbourhoods_exclude))
    }
    
    bbox = sf::st_bbox(neighbourhoods)
    min.lat = as.numeric(bbox$ymin)
    max.lat = as.numeric(bbox$ymax)
    min.lng = as.numeric(bbox$xmin)
    max.lng = as.numeric(bbox$xmax)
    pal = colorNumeric(palette = "Blues", domain = hoods_df$doc_count, reverse = FALSE)
    
    colours = get_sentiment_colourmap()
    
    hoods_df$label = ifelse(!is.na(hoods_df$avg_sentiment),
                             as.character(cut(hoods_df$avg_sentiment, 
                                             breaks = c(colour_map$lower, Inf), 
                                             labels = colour_map$label, 
                                             include.lowest = TRUE, 
                                             right = TRUE)),
                            NA)
    hoods_df = dplyr::left_join(hoods_df, colours, by = c("label" = "label"))
    
    if (selected == "Sentiment") {
      m = leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
        fitBounds(min.lng, min.lat, max.lng, max.lat) %>%
        addPolygons(
          data = hoods_df,
          # fillColor = ~pal(sentiment_mean),
          fillColor = ~colour,
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 3,
            color = "#666666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = ~lapply(label1, htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        )
    } else if (selected == "Count") {
      m = leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
        fitBounds(min.lng, min.lat, max.lng, max.lat) %>%
        addPolygons(
          data = hoods_df,
          fillColor = ~pal(doc_count),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 3,
            color = "#666666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = ~lapply(label2, htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        )
    }
    
    return(m)
  })
  
  output$map_plot_output <- leaflet::renderLeaflet({
    plot_alt_map()
  })
  
  output$map_plot_output_ui <- renderUI({
    leaflet::leafletOutput(session$ns("map_plot_output"), height = 500)
  })
  
  output$sentiment_colour_scale <- renderPlot({
    colour_map <- get_sentiment_colourmap()
    sentimentLegendPlot(colour_mapping = colour_map)
  })
}

sentimentNeighbourhoodsQuery <- function() {
  '
  "aggs": {
    "group_by_index": {
      "terms": {
        "field": "_index"
      },
      "aggs" : {
        "hoods": {
          "nested": {
            "path": "neighbourhoods"
          },
          "aggs": {
    	    "names": {
    	      "terms": {
    	        "size": 500,
    	          "field": "neighbourhoods.name.keyword"
    	        },
      		"aggs": {
      		  "count_sum": {
      		    "sum": {
      		      "field": "neighbourhoods.count"
      		    }
      		  },
      		  "ratio_sum": {
      		    "sum": {
      		      "field": "neighbourhoods.ratio"
      		    }
      		  },
      		  "neighbourhood_to_sentiment": {
      		    "reverse_nested": {},
      		      "aggs": {
      		        "avg_sentiment": {
      		          "avg": {
      		            "field": "sentiment_polarity"
      		          }
      		        }
      		      }
      		    }
      		  }
    	        }
              }
            }
          }
        }
      }'
}
library(httr, warn.conflicts = FALSE)
library(memoise, warn.conflicts = FALSE)
library(jsonlite, warn.conflicts = FALSE)
library(stringr)

search_fields <- function() {
  list("Document Text" = "text",
       "Source Title" = "source_title",
       "Parent Source Title" = "parent_source_title")
}

get_configs_from_file <- function() {
  if (file.exists('.configs')) {
    prop <- yaml::read_yaml('.configs')
  } else {
    if (file.exists('decode.sh')) {
      result = system('./decode.sh ./app_vault_dir/.configs.enc ./app_key_dir/key.bin', intern = TRUE)
      configs_str = paste0(result, collapse = "\n")
      prop = yaml::yaml.load(configs_str)
    } else {
      stop(".configs file not found!")
    }
  }

  prop
}
get_configs = memoise(get_configs_from_file)

connect_ES <- function() {
  prop <- get_configs()
  x <- elastic::connect(host = prop$apiurl,
                        user = prop$apiuser,
                        pwd = prop$apipassword,
                        port = "",
                        path = "",
                        errors = "complete",
                        transport_schema = "https"
  )
}

get_aliases <- function(session = NULL) {
  aliases <- c()
  if ("SHINYPROXY_USERGROUPS" %in% (Sys.getenv() %>% names())) {
    # Get Aliases from assigned roles for user (passed to shiny proxy from Keycloak)
    aliases <- Sys.getenv("SHINYPROXY_USERGROUPS") %>%
      stringr::str_split(pattern = ",") %>%
      unlist()

    print("reading SHINYPROXY_USERGROUPS")
  } else if (!is.null(session)) {
    if (is.character(session$clientData$shinyproxy_roles)){
      print(paste0("user: ", session$clientData$shinyproxy_user))
      print(paste0("user roles: ", session$clientData$shinyproxy_roles))
      aliases <- session$clientData$shinyproxy_roles
    }
  }

  if (length(aliases) > 0) {
    aliases <- aliases %>%
      tolower() %>%
      stringr::str_subset("text_depot_alias") %>%
      stringr::str_remove("text_depot_alias_")
  }

  # If nothing was specified via the env variables, then use the default from configs
  if (length(aliases) == 0) {
    prop <- get_configs()
    aliases <- prop$default_index_aliases
    cat("Using alias list from configs:", paste(aliases, collapse = "; "), "\n")
  }

  if (length(aliases) == 0) {
    stop("User does not have permission to access any aliases, or no aliases specified in .configs!")
  }
  return(aliases)
}

alias_to_colours_mapping <- function(aliases) {
  # List of colours available at: https://stackoverflow.com/a/56495465/786540
  # NOTE: Dont use RED for a dataset colour! (Confusing when paired with sentiment)
  # second element of google colour list should be "#dc3912" (red)
  # If we ever have more than 30 indexes, we will run out of colours,  and will need to cycle through...
  available_colours = c("#3366cc", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477", "#66aa00", "#b82e2e", "#316395", "#994499", "#22aa99", "#aaaa11", "#6633cc", "#e67300", "#8b0707", "#651067", "#329262", "#5574a6", "#3b3eac", "#b77322", "#16d620", "#b91383", "#f4359e", "#9c5935", "#a9c413", "#2a778d", "#668d1c", "#bea413", "#0c5922", "#743411")

  data.frame(
    alias_name = aliases,
    colours = available_colours[1:length(aliases)],
    stringsAsFactors = FALSE
  )
}

# Return a data frame with two columns:
#   index_name - name of the internal ES index
#   alias_name - alias used to point at the index
index_to_alias_mapping <- function(es, alias_names) {
  mapping = elastic::aliases_get(es, index = alias_names)

  mapping_df = tibble::enframe(mapping, name = "index_name") %>%
    tidyr::unnest_wider(value)

  if (!all(is.na(mapping_df$aliases))) { 
    mapping_df = mapping_df %>%
      tidyr::unnest_longer(aliases, indices_to = "alias_name", keep_empty = TRUE) %>%
      select(-aliases) %>%
      mutate(alias_name = dplyr::coalesce(alias_name, index_name)) # use index name if alias name not present
  } else {
    # None of the specified datasets had an alias, so the column doesnt exist:
    mapping_df$alias_name = mapping_df$index_name
  }

  assertthat::assert_that(all(alias_names %in% union(mapping_df$alias_name, mapping_df$index_name)), msg = "some requested aliases/indices are not in Elastic Search")

  mapping_df <- mapping_df %>%
    filter(alias_name %in% alias_names | index_name %in% alias_names)

  return(mapping_df)
}


query_count <- function(conn, index) {
  elastic::count(conn, index = index)
}

search_body <- function(query,
                        search_fields,
                        min_score = 0,
                        min_date = "1900-01-01",
                        max_date = Sys.Date(),
                        min_sentiment = -1,
                        max_sentiment = 1,
                        use_embedding_search = FALSE) {
  fields_str = paste0('"', search_fields, '"', collapse = ", ")

  filter_str = glue::glue('
    "bool": {
      "must": [
        {
          "range": {
            "date": {
              "gte": "<{min_date}>",
              "lte": "<{max_date}>"
            }
          }
        },
        {
          "range": {
            "sentiment_polarity": {
              "gte": "<{min_sentiment}>",
              "lte": "<{max_sentiment}>"
            }
          }
        }
      ]
    }
  ', .open = "<{", .close = "}>")

  if (use_embedding_search) {
    # min_score should be in [0, 1]
    # min_score = 0.55 # This suggests using at least 0.5 for Doc2Vec: https://radimrehurek.com/gensim/auto_examples/howtos/run_doc2vec_imdb.html
    min_score = 0.35 # FastText needs a lower min_score
    vector_fields = paste0(search_fields, "_vector")
    embedding_search_body(query,
                          vector_fields, 
                          filter_str, 
                          min_score, 
                          get_configs()$embedding_api_host, 
                          get_configs()$embedding_api_user, 
                          get_configs()$embedding_api_password, 
                          get_configs()$embedding_api_version)
  } else {
    standard_search_body(query, fields_str, filter_str, min_score)
  }
}

embedding_search_body <- function(query,
                                  vector_fields_to_search,
                                  filter_str,
                                  min_score,
                                  api_url,
                                  api_user,
                                  api_password,
                                  api_version) {
  vector = get_embedding_vector(query, api_url, api_user, api_password, api_version)

  vector_str = paste0("[", paste0(vector, collapse = ", "), "]")

  # https://www.elastic.co/guide/en/elasticsearch/reference/7.3/query-dsl-script-score-query.html#vector-functions
  min_score = min_score + 1.0 # Cosine similarity function below adds 1.0 to score, to avoid negative scores.

  # Calculate similarity to each field, if they've been defined as a field to search. Otherwise set to zero:
  text_sim = "def textsim = 0; "
  if ('text_vector' %in% vector_fields_to_search) { 
    text_sim = "def textsim = doc['text_vector'].size() == 0 ? 0 : cosineSimilarity(params.queryVector, 'text_vector') + 1; "
  }

  parent_sim = "def parentsim = 0; "
  if ('parent_source_title_vector' %in% vector_fields_to_search) { 
    parent_sim = "def parentsim = doc['parent_source_title_vector'].size() == 0 ? 0 : cosineSimilarity(params.queryVector, 'parent_source_title_vector') + 1; " 
  }

  title_sim = "def titlesim = 0; "
  if ('source_title_vector' %in% vector_fields_to_search) { 
    title_sim = "def titlesim = doc['source_title_vector'].size() == 0 ? 0 : cosineSimilarity(params.queryVector, 'source_title_vector') + 1; " 
  }

  # Get the max of the similarity to each field:
  script_block = paste0(text_sim, 
                        parent_sim, 
                        title_sim,
                        "def currscore = Math.max(titlesim, textsim); ",
                        "Math.max(currscore, parentsim);")
  glue::glue('
    "min_score": <{min_score}>,
    "query": {
      "script_score": {
        "query": {
          "bool": {
            "filter": [
              {
                <{filter_str}>
              }
            ]
          }
        },
        "script": {
          "source": "<{script_block}>",
          "params": {"queryVector": <{vector_str}>}
        }
      }
    }
  ', .open = "<{", .close = "}>")
}

standard_search_body <- function(query, fields_str, filter_str, min_score = 0) {
  query = glue::double_quote(query)
  glue::glue('
    "min_score": <{min_score}>,
    "query": {
      "bool": {
        "must": {
          "simple_query_string" : {
            "query": <{query}>,
            "fields": [ <{fields_str}> ],
            "default_operator": "and"
          }
        },
        "filter": {
          <{filter_str}>
        }
      }
    }
  ', .open = "<{", .close = "}>")
}


source_body <- function(source_cols) {
  if (length(source_cols) == 0) return(c())

  source_cols <- source_cols %>%
    glue::double_quote() %>%
    glue::glue_collapse(sep = ",") %>%
    (function(x) glue::glue("[", x, "]"))

  glue::glue('"_source": <{source_cols}>', .open = "<{", .close = "}>")
}

highlights_body <- function(highlight_cols = c("text", "source_title", "parent_source_title"),
                            n_char_no_match = 100,
                            n_char_highlights = 100) {
  if (length(highlight_cols) == 0) {
    return(c())
  }

  no_match_str = glue::glue(': {"no_match_size" :  <{n_char_no_match}>,
                                "fragment_size" :  <{n_char_highlights}>  }', .open = "<{", .close = "}>")

  highlight_cols <- highlight_cols %>%
    glue::double_quote() %>%
    glue::glue_collapse(sep = paste0(no_match_str, ",")) %>%
    (function(x) glue::glue("<{x}>", no_match_str, .open = "<{", .close = "}>"))

  glue::glue(
    '"highlight": {
        "fields": {
          <{highlight_cols}>
        }
      }
  ', .open = "<{", .close = "}>")
}

stats_for_field <- function(conn, index, field, numeric = FALSE) {
  aggs = glue::glue(
    '{
      "aggs": {
        "fieldstats": {
          "stats": {
            "field": "<{field}>"
          }
        }
      }
    }', .open = "<{", .close = "}>")

  result <- elastic::Search(conn, index = index, body = aggs, size = 0)

  if (numeric) {
    return(list(min = result$aggregations$fieldstats$min,
                max = result$aggregations$fieldstats$max,
                avg = result$aggregations$fieldstats$avg,
                sum = result$aggregations$fieldstats$sum))
  } else {
    return(list(min = result$aggregations$fieldstats$min_as_string,
                max = result$aggregations$fieldstats$max_as_string,
                avg = result$aggregations$fieldstats$avg_as_string,
                sum = result$aggregations$fieldstats$sum_as_string))
  }
}

combine_query <- function(...) {
  query_parts <- c(...)

  query_parts %>%
    glue::glue_collapse(sep = ",") %>%
    (function(x) glue::glue("{ <{x}>}", .open = "<{", .close = "}>")) %>%
    jsonlite::prettify()
}

query_text_depot <- function(query_info = NULL,
                             aggregates_json = NULL,
                             source_json = NULL,
                             highlights_json = NULL,
                             from = 0,
                             size = 0) {
  conn = query_info$conn
  query_str = query_info$query
  index = query_info$index
  search_fields = query_info$search_fields
  min_score = query_info$min_score
  min_date = query_info$min_date
  max_date = query_info$max_date
  min_sentiment = query_info$min_sentiment
  max_sentiment = query_info$max_sentiment
  sort_by = query_info$sort_by
  use_embeddings = query_info$use_embeddings
  query_search <- search_body(
    query = query_str,
    search_fields = search_fields,
    min_score = min_score,
    min_date = min_date,
    max_date = max_date,
    min_sentiment = min_sentiment,
    max_sentiment = max_sentiment,
    use_embedding_search = use_embeddings
  )

  if (is.null(sort_by)) { sort_by = "score" }
  sort_json = case_when(sort_by == "score" ~ '"sort": [ "_score" ]',
                        sort_by == "date_asc" ~ '"sort": [ {"date":{"order":"asc"}} ,{"_score":{"order":"desc"}}]',
                        sort_by == "date_desc" ~ '"sort": [ {"date": {"order":"desc"}},{"_score":{"order":"desc"}} ]',
                        TRUE ~ '"sort": [ "_score" ]')

  # if part of query is not defined, it will be default NULL value, and that is elegantly handled by combine_query:
  query <- combine_query(query_search,
                         aggregates_json,
                         source_json,
                         highlights_json,
                         sort_json)

  results <- elastic::Search(conn = conn,
                             index = index,
                             body = query,
                             asdf = TRUE,
                             from = from,
                             size = size
  )

  if (results$`_shards`$failed > 0) { return(paste0("Error! Shard Failed! ", paste(results$`_shards`$failures, collapse = "; "))) }

  results
}

get_document_summary <- function(query,
                                 api_url,
                                 api_user,
                                 api_password) {
  body = list(query = query, max_summary_length = 150)
  response = api_url %>%
    paste0("/summarize") %>%
    httr::POST(authenticate(api_user, api_password), body = body, encode = "json") 
    
  if (response$status != 200) { 
    stop(paste0("ERROR From API at ", api_url, " with user ", api_user, " in get_embedding_vector(): ", as.character(response))) 
  }

  content = httr::content(response)
  content$summary
}

get_embedding_vector <- function(query, 
                                 api_url, 
                                 api_user, 
                                 api_password,
                                 api_version) {
  query = URLencode(query)
  response = api_url %>%
    paste0("/embeddings_api/", api_version, "/embed_query?query=", query) %>%
    httr::GET(authenticate(api_user, api_password)) 
    
  if (response$status != 200) { 
    stop(paste0("ERROR From API at ", api_url, " with user ", api_user, " in get_embedding_vector(): ", as.character(response))) 
  }

  content = httr::content(response)

  unlist(content$embedding_vector)  
}

parse_hits <- function(es_results, index_to_db_map) {
  hits = es_results$hits$hits %>% as_tibble() # Only current page

  rows = nrow(hits)
  if (is.null(rows)) { return(data.frame()) }
  if (rows == 0) { return(data.frame()) }

  # Hits contains four columns:
  #  _id -- Elastic Search ID
  #  _type -- "_doc" -- not used.
  #  _index: Source index for the hit (e.g. "council_report_files")
  #  _score: match score
  #  _source: the document itself.
  hits_df <- hits %>%
    group_by(`_id`) %>%
    add_cols(c("_source.date", "_source.text", "_source.num_sentences", "_source.sentiment_subjectivity",
               "_id", "_source.parent_source_url", "_source.num_chars", "_source.parent_source_title",
               "_source.source_url", "_source.sentiment_polarity", "_source.source_title", "highlight.text",
               "highlight.source_title", "highlight.parent_source_title")) %>% # add any columns that did not return - with multiple indexes, some may be blank
    mutate(match_text = format_matches_for_hit(text_list = highlight.text,
                                               source_title = `_source.source_title`,
                                               source_url = `_source.source_url`,
                                               parent_source_title = `_source.parent_source_title`,
                                               parent_source_url = `_source.parent_source_url`)) %>%
    ungroup() %>%
    rename(id = `_id`,
           similarity = `_score`,
           index_name = `_index`,
           date = `_source.date`,
           text = `_source.text`,
           source_url = `_source.source_url`,
           source_title = `_source.source_title`,
           parent_source_url = `_source.parent_source_url`,
           parent_source_title = `_source.parent_source_title`,
           num_chars = `_source.num_chars`,
           num_sentences = `_source.num_sentences`,
           sentiment = `_source.sentiment_polarity`,
           sentiment_subj = `_source.sentiment_subjectivity`,
           matches = match_text) %>%
    left_join(index_to_db_map, by = c("index_name" = "index_name")) %>%
    mutate_if(is.numeric, .funs = function(x) round(x, 3)) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d"))

  hits_df
}

parse_aggregates <- function(es_results) {
  keys <- es_results$aggregations$group_by_index$buckets$key

  counts_by_index <- bind_cols(index = es_results$aggregations$group_by_index$buckets$key,
                               doc_count = es_results$aggregations$group_by_index$buckets$doc_count)

  aggregate_names <- setdiff(names(es_results$aggregations$group_by_index$buckets), c("key", "doc_count"))
  results <- es_results$aggregations$group_by_index$buckets[aggregate_names]

  results_parsed <- purrr::map(results, function(x) {
    names(x) <- keys
    bind_rows(x, .id = "index")
  })

  results_parsed <- c(list(counts_by_index = counts_by_index), results_parsed)

  return(results_parsed)
}

get_document_by_id <- function(connection,
                               id,
                               index) {
  source_body = source_body(c("text", "neighbourhoods"))
  query = glue::glue('
    {
      "query": {
        "terms": {"_id": ["<{id}>"]}
      },
      <{source_body}>
    }', .open = "<{", .close = "}>")

  results = elastic::Search(connection,
                            index = index,
                            size = 1,
                            body = query)

  fields = results$hits$hits[[1]]$`_source`
  neighbourhoods = fields$neighbourhoods
  if (length(neighbourhoods) > 0) {
    neighbourhoods = rbindlist(neighbourhoods)
  }

  return(list(
    "text" = fields$text,
    "neighbourhoods" = neighbourhoods$name
  ))
}

# text_list can contain several matching snippets for this document.
format_matches_for_hit <- function(text_list,
                                   source_title,
                                   source_url,
                                   parent_source_title,
                                   parent_source_url,
                                   add_link = FALSE) {
  formatted_parent_title = format_title(parent_source_title, parent_source_url, add_link)
  formatted_title = format_title(source_title, source_url, add_link)
  formatted_highlights = collapse_highlights(text_list, collapse = " ... ", rm_em = FALSE, color_em = TRUE)

  formatted_match = paste0(strong(formatted_parent_title),
                           br(),
                           formatted_title,
                           br(),
                           formatted_highlights,
                           br())

  formatted_match
}

format_title <- function(title_str, title_url, add_link) {
  if (is.na(title_str)) { return("") }

  link_present = TRUE
  if (is.na(title_url)) {
    link_present = FALSE
  } else if (title_url == "") {
    link_present = FALSE
  }

  if (link_present & add_link) {
    formatted_str = a(title_str, href = title_url, target = "_blank")
  } else {
    formatted_str = span(title_str, style = sprintf("color: %s;", titles_color()))
  }

  return(formatted_str)
}

collapse_highlights <- function(...,
                                collapse = "<br> ... <br>",
                                rm_em = FALSE,
                                bold = FALSE,
                                color_em = FALSE) {
  out <- list(...) %>%
    unlist() %>%
    (function(x) Filter(Negate(is.na), x)) %>% # remove NA elements
    paste(collapse = collapse)

  if (rm_em == TRUE) {
    out <- out %>%
      stringr::str_remove_all("<em>") %>%
      stringr::str_remove_all("</em>")
  }

  if (color_em) {
    #out = str_replace_all(out, "<em>", '<em style="color: #01735e;">')
    out = str_replace_all(out, "<em>", '<em style="text-decoration: underline;">')
  }

  out
}

add_cols <- function(data, col_name) {
  add <- col_name[!col_name %in% names(data)]

  if (length(add) != 0) data[add] <- NA

  data
}

## This function is coppied from Timetk package, since it is not exposed by the
# API:
# https://github.com/business-science/timetk/blob/master/R/plot-time_series.R
# it relies on 2 functions that are exposed by the API, so we still have the
# timetk package requirement
# A wrapper for smooth_vec() that handles changes in grouped idx's
auto_smooth <- function(idx, x,
                        smooth_period,
                        smooth_span,
                        smooth_degree,
                        smooth_message) {

  if (length(x) < 2) return(NA)

  if (all(c(is.null(smooth_span), is.numeric(idx)))) {
    # Numeric index
    smooth_span <- 0.75
  }

  if (all(c(!is.null(smooth_period), is.null(smooth_span)))) {
    # smooth_period = some value, and smooth span is NULL

    if (tolower(smooth_period) == "auto") {
      smooth_period <- ceiling(length(idx) * 0.75)
    }

    smooth_period <- timetk::tk_get_trend(
      idx      = idx,
      period   = smooth_period,
      message  = smooth_message
    )

    smooth_span <- NULL
  } else {
    # smooth span overrides smooth period
    smooth_period <- NULL
    smooth_span   <- as.numeric(smooth_span)
    # if (smooth_message) message(stringr::str_glue())
  }

  ret <- timetk::smooth_vec(
    x      = x,
    period = smooth_period,
    span   = smooth_span,
    degree = smooth_degree)

  return(ret)
}

plot_timeseries_td <- function(plot_data, date_var, value_var, group_var, colour_var, data_set_info, show_trend = TRUE, scales = "fixed", date_format = "%Y-%b") {

  if (show_trend == TRUE) {
    plot_data <- plot_data %>%
      group_by({{group_var}}) %>%
      mutate(smooth_value = auto_smooth(
        idx                   = {{date_var}},
        x                     = {{value_var}},
        smooth_period         = "auto",
        smooth_span           = NULL,
        smooth_degree         = 2,
        smooth_message        = FALSE)
      ) %>%
      dplyr::ungroup()
  }

  p <- plot_data %>%
    ggplot2::ggplot(mapping = aes(x = {{date_var}}, y = {{value_var}}, colour = {{colour_var}}))

  if (show_trend == TRUE) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(y = smooth_value),
        color = "dark grey",
        size  = 1,
        alpha = 0.8)
  }

  p <- p +
    geom_line() +
    facet_wrap(vars({{group_var}}), ncol = 1, scales = scales) +
    scale_color_manual(breaks = pull(data_set_info, {{group_var}}),
                       values = data_set_info$colours) +
    ggthemes::theme_gdocs() +
    theme(legend.position = "none") +
    scale_x_date(date_labels = date_format)

  return(p)
}

# https://stackoverflow.com/questions/3245862/format-numbers-to-significant-figures-nicely-in-r
sigfig <- function(vec, n=3){
  ### function to round values to N significant digits
  # input:   vec       vector of numeric
  #          n         integer is the required sigfig
  # output:  outvec    vector of numeric rounded to N sigfig

  formatC(signif(vec,digits = n), digits = n,format = "fg", flag = "#")

}      # end of function   sigfig

get_sentiment_colourmap_for_value <- function(sentiment_value) {
  get_sentiment_colourmap() %>%
    filter(sentiment_value >= lower) %>%
    filter(sentiment_value < upper)
}

get_sentiment_colourmap <- function() {
  # order is used in the data wrangling functions to ensure that colours
  # show up in correct order in chart
  colour_map <- tibble(colour = c("#C00000", "#C47070", "#C4BDBD", "#C4BDBD", "#7287FF", "#0027FF"),
                      legend_text_colour = c("#101010", "#101010", "#101010", "#101010", "#101010", "#C4BDBD"),
                      name = c("very_negative", "negative", "neutral_negative", "neutral_positive", "positive", "very_positive"),
                      label = c("Very Negative", "Negative", "Neutral", "Neutral", "Positive", "Very Positive"),
                      lower = c(-100, -0.3, -0.1, 0, 0.1, 0.3), # Inclusive
                      upper = c(-0.3, -0.1, 0, 0.1, 0.3, 100),  # Exclusive
                      order = c(1,2,3,6,5,4))

  return(colour_map)
}

# Note that this aggregation includes the from value and excludes the to value for each range.
# https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-range-aggregation.html
build_likert_bucket <- function(name, lower, upper) {
  filter_str = glue::glue('{ "key": "<{name}>", "from": <{lower}>, "to": <{upper}> }',
                          .open = "<{", .close = "}>")

}

build_likert_bins <- function(mapping) {
  bucket_str = mapping %>%
    group_by(name) %>%
    mutate(bucket_string = build_likert_bucket(name, lower, upper)) %>%
    pull(bucket_string) %>% paste0(collapse = ",\n")
}

plot_likert <- function(plot_data, colour_map, x_var, fractions_var, fill_name, group_var = NULL, hover_label) {
  p <- plot_data %>%
    ggplot2::ggplot(mapping = aes(x = {{x_var}},
                                  y = {{fractions_var}} * 100,
                                  fill = {{fill_name}},
                                  text = {{hover_label}})) +
    ggplot2::geom_bar(stat = "identity",
                      position = "stack") +
    scale_fill_manual(breaks = colour_map$name,
                      values = colour_map$colour,
                      labels = colour_map$label) +
    ggplot2::geom_hline(yintercept = 0, colour = "white") +
    ggthemes::theme_few() +
    theme(legend.position="none") +
    xlab("") + ylab("Proportion of Matching Documents (%)")

  if(!is.null(group_var)) {
    p <- p + ggplot2::facet_wrap(group_var, ncol = 1)
  }else {
    p <- p + ggplot2::coord_flip() +
      ylim(-100,100) # set y limits for facetted plots to maximize space
  }

  return(p)
}

wrangle_likert_data <- function(input_data, colour_map, data_set_info) {

  if(!"Date" %in% names(input_data)) input_data$Date = "dummy" # dummy date to make grouping work for both likert plot types

  out <- input_data %>%
    # the following line redistributes the two neutral categories equally among neutral- positive and negative, such that the bar is centred on zero
    mutate(sentiment.buckets.neutral_negative.doc_count = (sentiment.buckets.neutral_negative.doc_count + sentiment.buckets.neutral_positive.doc_count) / 2,
           sentiment.buckets.neutral_positive.doc_count = sentiment.buckets.neutral_negative.doc_count) %>%
    as_tibble() %>%
    tidyr::pivot_longer(starts_with("sentiment.buckets."),
                        names_to = "sentiment_bucket",
                        names_prefix = "sentiment.buckets.",
                        values_to = "Count") %>%
    mutate(sentiment_bucket = str_remove(sentiment_bucket, ".doc_count")) %>%
    group_by(index_name, Date) %>%
    mutate(total = sum(Count),
           frac = Count / total,
           frac_mod = ifelse(str_detect(sentiment_bucket, "negative"),
                             yes = -1 * frac,
                             no = frac)) %>%
    ungroup() %>%
    mutate(Sentiment = factor(sentiment_bucket,
                              levels = colour_map$name)) %>% # turn into factor to make sure plot in correct order
    left_join(select(colour_map, sentiment_bucket = name, sentiment_label = label), by = "sentiment_bucket") %>%
    left_join(select(data_set_info, index_name, display_name), by = "index_name")  %>% # to get display name
    group_by(display_name, sentiment_label, Date) %>%
    mutate(display_count = sum(Count),
           display_frac = display_count / total) %>% # this is a hack to aggregate content for the labels - there are two neutral categories, that we want to combine for the labels
    ungroup()

  if(!is.Date(out$Date)) out$Date = NULL

  return(out)
}

sentiment_legend <- function() {

  colour_mapping <- get_sentiment_colourmap() %>%
    filter(name != "neutral_negative")

  colour_tags <- colour_mapping %>%
    group_by(label) %>%
    mutate(tag_list = list(tags$td(
      style = paste0("background-color: ", colour, "; padding: 15px; font-size: 20px; color: ", legend_text_colour, ";"),
      label)
    )) %>%
    ungroup

  tags$table(
    style = 'font-family: "Open Sans"',
    tags$tr(
      colour_tags$tag_list
    )
  )
}

do_health_check <- function() {
  status = "healthy"
  info = ""

  aliases = get_aliases()
  es_connection = connect_ES()
  if (length(aliases) == 0) {
    status = "error"
    info = "no aliases found"
  }

  return(list(
    health_status = status,
    health_info = info,
    performance = "",
    performance_info = ""
  ))
}

# Adds health check API endpoint
add_health_check <- function(route = "/__health-check__") {
  httpHandler = function(req) {
    info = do_health_check()

    return(list(status = 200L,
                headers = list('Content-Type' = 'application/json'),
                body = jsonlite::toJSON(info,
                                        pretty = TRUE,
                                        auto_unbox = TRUE)))
  }

  shiny:::handlerManager$addHandler(handler = shiny:::routeHandler(route, httpHandler),
                                    key = "unique_id_for_health_check")
}

# Runs expr after all inputs load the first time (i.e. after all the renderUI calls)
# From: https://github.com/rstudio/shiny/issues/3348#issuecomment-810727477
execute_at_first_input <- function(expr, session = getDefaultReactiveDomain()) {
  observeEvent(once = TRUE, reactiveValuesToList(session$input), {
    force(expr)
  }, ignoreInit = TRUE)
}
#' bla
#'
#' \code{fetch_tweet_id_raw} - Convert all Emojis to some ...
#'
#' @param .tweet_id_vec ...
#' @param .bearer_token ... 
#' @param .tweet_query_pars_static ...
#' @param ... ...
#' @return \code{fetch_tweet_id_raw} - returns a ...
#' @rdname fetch_tweet_id_raw
#' @export
#' @examples
#' 1+1
fetch_tweet_id_raw <- function(
  .tweet_id_vec=bit64::integer64(), .bearer_token, 
  .tweet_query_pars_static=NULL, ...
){
  
  stopifnot(bit64::is.integer64(.tweet_id_vec))
  stopifnot(is.character(.bearer_token))
  
  .tweet_id_vec_stack <- .tweet_id_vec
  .tweet_raw_data <- tibble::tibble()
  repeat({
    
    .tweet_query <- form_tweet_query(
      ids=stringr::str_c(.tweet_id_vec_stack[c(1:100)], collapse=","), 
      .incl_context_annotations=TRUE, 
      .pars_static=.tweet_query_pars_static
    )
    
    .tweet_id_vec_stack <- .tweet_id_vec_stack[-c(1:100)]
    
    .response <- GET_twitter_safely(
      url="https://api.twitter.com/2/tweets",
      query=.tweet_query,
      httr::add_headers(Authorization=stringr::str_c("Bearer ", .bearer_token)),
      httr::config(http_version=2),
      httr::timeout(60)
    )
    
    .tweet_raw_list <- 
      .response |> 
      httr::content(as="text", encoding="UTF-8") |> 
      jsonlite::fromJSON(simplifyMatrix=FALSE)
    
    .tweet_raw_data <- 
      .tweet_raw_list |> 
      prep_raw_data.tweet(.tweet_data_query=.tweet_query) |> 
      dplyr::bind_rows(.tweet_raw_data)
    
    if(length(.tweet_id_vec_stack) == 0){break}
    
  })
  
  return(.tweet_raw_data)
  
}

#' bla
#'
#' \code{fetch_tweet_search_raw} - Convert all Emojis to some ...
#'
#' @param .search_query ...
#' @param .date_new ...
#' @param .date_old ... 
#' @param ... ...
#' @return \code{fetch_tweet_id_raw} - returns a ...
#' @rdname fetch_tweet_id_raw
#' @export
#' @examples
#' 1+1
fetch_tweet_search_raw <- function(
    .search_query, .date_new, .date_old, .bearer_token, ...
){

  stopifnot(
    all(sapply(list(.search_query, .bearer_token), is.character)),
    all(sapply(list(.date_new, .date_old), lubridate::is.POSIXct)),
    all(lengths(list(.search_query, .date_new, .date_old, .bearer_token)) == 1)
  )
  
  .tweet_query <- form_tweet_query(
    query=.search_query,
    start_time = format_zulutime(.date_old),
    end_time = format_zulutime(.date_new), 
    max_results = "100",
    .incl_context_annotations = FALSE
  )
  
  .tweet_raw_data <- tibble::tibble()
  repeat{
    
    .response <- GET_twitter_safely(
      url="https://api.twitter.com/2/tweets/search/all",
      query=.tweet_query,
      httr::add_headers(Authorization=stringr::str_c("Bearer ", .bearer_token)),
      httr::config(http_version=2),
      httr::timeout(60)
    )
    
    .tweet_raw_list <- 
      .response |> 
      httr::content(as="text", encoding="UTF-8") |> 
      jsonlite::fromJSON(simplifyMatrix=FALSE)
    
    .next_token <- purrr::pluck(
      .tweet_raw_list, "meta", "next_token", .default=NULL
    )
    
    .tweet_raw_data <- 
      .tweet_raw_data |> 
      dplyr::bind_rows(
        prep_raw_data.tweet(.tweet_raw_list, .tweet_data_query=.tweet_query)
      )
    
    if(is.null(.next_token)){
      break
    }else{
      .tweet_query[["next_token"]] <- .next_token
    }
    
  }
  
  return(.tweet_raw_data)
  
}


#' bla
#'
#' \code{fetch_tweet_search_raw} - Convert all Emojis to some ...
#'
#' @param .search_query ...
#' @param .date_new ...
#' @param .date_old ... 
#' @param .resolution ...
#' @param .bearer_token ... 
#' @param ... ...
#' @return \code{fetch_tweet_id_raw} - returns a ...
#' @rdname fetch_tweet_id_raw
#' @export
#' @examples
#' 1+1
fetch_tweet_count_raw <- function(
  .search_query, .date_new, .date_old, .resolution, .bearer_token, ...
){
  
  stopifnot(
    .resolution %in% c("minute", "hour", "day"),
    all(sapply(list(.search_query, .bearer_token), is.character)),
    all(sapply(list(.date_new, .date_old), lubridate::is.POSIXct)),
    all(lengths(list(.search_query, .date_new, .date_old, .bearer_token)) == 1)
  )
  
  .tweet_query <- form_tweet_query(
    query=.search_query,
    start_time = format_zulutime(.date_old),
    end_time = format_zulutime(.date_new), 
    granularity = .resolution,
    .incl_context_annotations = FALSE,
    .pars_static=list()
  )
  
  .tweet_raw_data <- tibble::tibble()
  repeat{
    
    .response <- GET_twitter_safely(
      url="https://api.twitter.com/2/tweets/counts/all",
      query=.tweet_query,
      httr::add_headers(Authorization=stringr::str_c("Bearer ", .bearer_token)),
      httr::config(http_version=2),
      httr::timeout(60)
    )
    
    .tweet_raw_list <- 
      .response |> 
      httr::content(as="text", encoding="UTF-8") |> 
      jsonlite::fromJSON(simplifyMatrix=FALSE)
    
    .tweet_raw_data <- 
      .tweet_raw_data |> 
      dplyr::bind_rows(
        prep_raw_data.tweet_count(
          .tweet_raw_list=.tweet_raw_list, .tweet_data_query=.tweet_query
        )
      )
    
    .next_token <- purrr::pluck(
      .tweet_raw_list, "meta", "next_token", .default=NULL
    )
    
    if(is.null(.next_token)){
      break
    }else{
      .tweet_query[["next_token"]] <- .next_token
    }
    
  }
  
  return(.tweet_raw_data)
  
}


#' bla
#'
#' \code{fetch_tweet_id_raw} - ...
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
    
    .max_results <- min(length(.tweet_id_vec_stack), 100)

    .tweet_query <- form_tweet_query(
      ids=stringr::str_c(.tweet_id_vec_stack[c(1:.max_results)], collapse=","), 
      .incl_context_annotations=TRUE, 
      .pars_static=.tweet_query_pars_static
    )
    
    .tweet_id_vec_stack <- .tweet_id_vec_stack[-c(1:.max_results)]
    
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
#' \code{fetch_tweet_search_raw} - ...
#'
#' @param .search_query ...
#' @param .date_new ...
#' @param .date_old ...
#' @param .bearer_token ... 
#' @param .search_scope ...
#' @param ... ...
#' @return \code{fetch_tweet_search_raw} - returns a ...
#' @rdname fetch_tweet_search_raw
#' @export
#' @examples
#' 1+1
fetch_tweet_search_raw <- function(
  .search_query, .date_new, .date_old, .bearer_token, .search_scope="all", ...
){

  stopifnot(
    all(sapply(list(.search_query, .bearer_token), is.character)),
    all(sapply(list(.date_new, .date_old), lubridate::is.POSIXct)),
    all(lengths(list(.search_query, .date_new, .date_old, .bearer_token)) == 1),
    isTRUE(.search_scope %in% c("all", "recent"))
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
      url=stringr::str_glue(
        "https://api.twitter.com/2/tweets/search/{.search_scope}/"
      ),
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
#' \code{fetch_tweet_count_raw} - ...
#'
#' @param .search_query ...
#' @param .date_new ...
#' @param .date_old ... 
#' @param .resolution ...
#' @param .bearer_token ... 
#' @param .scope ... 
#' @param ... ...
#' @return \code{fetch_tweet_count_raw} - returns a ...
#' @rdname fetch_tweet_count_raw
#' @export
#' @examples
#' 1+1
fetch_tweet_count_raw <- function(
  .search_query, .date_new, .date_old, .resolution, .bearer_token, .scope="all",
  ...
){
  
  stopifnot(
    .resolution %in% c("minute", "hour", "day"),
    .scope %in% c("all", "recent"),
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
  
  .tweet_url <- stringr::str_c(
    "https://api.twitter.com/2/tweets/counts/", .scope
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
    
    .tweet_raw_data <- .tweet_raw_data |> dplyr::bind_rows(
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

#' bla
#'
#' \code{fetch_tweet_timeline_raw} - ...
#'
#' @param .user_id_vec ...
#' @param .bearer_token ... 
#' @param .tweet_count_max ...
#' @param .excl_replies ... 
#' @param .excl_retweets ...
#' @param .until_tweet_id ...
#' @param .page_size ...
#' @param .tweet_query_pars_static ...
#' @param ... ...
#' @return \code{fetch_tweet_timeline_raw} - returns a ...
#' @rdname fetch_tweet_timeline_raw
#' @export
#' @examples
#' 1+1
fetch_tweet_timeline_raw <- function(
  .user_id_vec=bit64::integer64(), .bearer_token, .tweet_count_max=Inf, 
  .excl_replies=FALSE, .excl_retweets=FALSE, .until_tweet_id=NULL,
  .page_size=10L, .tweet_query_pars_static=NULL, ...
){
  
  stopifnot(bit64::is.integer64(.user_id_vec))
  stopifnot(is.character(.bearer_token))
  
  .user_id_vec_stack <- .user_id_vec
  .tweet_raw_data <- tibble::tibble()
  repeat({
    
    .tweet_query <- form_tweet_query(
      .incl_context_annotations=TRUE, 
      .pars_static=.tweet_query_pars_static
    )
    
    if(.excl_replies | .excl_retweets){
      if(!"exclude" %in% names(.tweet_query)){
        .tweet_query[["exclude"]] <- stringr::str_c(
          if(.excl_replies){"replies"}, if(.excl_retweets){"retweets"},
          sep=","
        )
      }else{
        stop("\"exclude\" query parameter already present")
      }
    }
    
    if(!is.null(.until_tweet_id)){
      if(!"until_id" %in% names(.tweet_query)){
        stopifnot(bit64::is.integer64(.until_tweet_id))
        .tweet_query[["until_id"]] <- bit64::as.character(.until_tweet_id)
      }else{
        stop("\"until_id\" query parameter already present")
      }
    }
    
    if(!"max_results" %in% names(.tweet_query)){
      stopifnot(is.integer(.page_size))
      .tweet_query[["max_results"]] <- .page_size
    }else{
      stop("\"max_results\" query parameter already present")
    }
    
    .tweet_url <- stringr::str_c(
      "https://api.twitter.com/2/users/", .user_id_vec_stack[1], "/tweets"
    )
    
    .user_id_vec_stack <- .user_id_vec_stack[-1]
    
    .tweet_count <- 0L
    repeat({
      
      .response <- GET_twitter_safely(
        url=.tweet_url,
        query=.tweet_query,
        httr::add_headers(
          Authorization=stringr::str_c("Bearer ", .bearer_token)
        ),
        httr::config(http_version=2),
        httr::timeout(60)
      )
      
      .tweet_raw_list <- 
        .response |> 
        httr::content(as="text", encoding="UTF-8") |> 
        jsonlite::fromJSON(simplifyMatrix=FALSE)
      
      .tweet_raw_data <- .tweet_raw_data |> dplyr::bind_rows(
        prep_raw_data.tweet(.tweet_raw_list, .tweet_data_query=.tweet_query)
      )
      
      .tweet_count <- .tweet_count + purrr::pluck(
        .tweet_raw_list, "meta", "result_count", .default=0L
      )
      
      .next_token <- purrr::pluck(
        .tweet_raw_list, "meta", "next_token", .default=NULL
      )
      
      if(is.null(.next_token) | .tweet_count >= .tweet_count_max){
        break
      }else{
        .tweet_query[["pagination_token"]] <- .next_token
      }
      
    })
    
    if(length(.user_id_vec_stack) == 0){break}
    
  })
  
  return(.tweet_raw_data)
  
}
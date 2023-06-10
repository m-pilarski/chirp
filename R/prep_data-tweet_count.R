prep_raw_data.tweet_count <- function(
  .tweet_raw_list, .tweet_data_query, .tweet_data_date=lubridate::now()
){
  
  .tweet_data_main_raw <- 
    .tweet_raw_list |> 
    purrr::pluck("data", .default=tibble::tibble()) |> 
    jsonlite::flatten() |> 
    janitor::clean_names() |> 
    tibble::as_tibble() 
  
  .tweet_data_error_raw <- 
    .tweet_raw_list |> 
    purrr::pluck("errors", .default=tibble::tibble()) |> 
    jsonlite::flatten() |> 
    janitor::clean_names() |> 
    tibble::as_tibble() 
  
  .tweet_data_meta <- 
    .tweet_raw_list |> 
    purrr::pluck("meta", .default=list())
  
  .tweet_raw_data <- tibble::tibble(
    tweet_data_date = .tweet_data_date,
    tweet_data_query = list(.tweet_data_query),
    tweet_data_main_raw = list(.tweet_data_main_raw),
    tweet_data_error = list(.tweet_data_error_raw),
    tweet_data_meta = list(.tweet_data_meta)
  )
  
  return(.tweet_raw_data)
  
}

#' bla
#'
#' \code{prep_tidy_data.tweet} - Convert all Emojis to some ...
#'
#' @param .tweet_raw_data ...
#' @param .include_media_url ... \code{FALSE}
#' @return \code{prep_tidy_data.tweet} - returns a ...
#' @rdname prep_tidy_data.tweet
#' @export
#' @examples
#' 1+1
prep_tidy_data.tweet_count <- function(
  .tweet_raw_data
){
  
  # .tweet_raw_data <<- .tweet_raw_data#; stop()
  
  if(nrow(.tweet_raw_data) == 0){
    return(tweet_count_tidy_data_skeleton)
  }
  
  .tweet_data_main_nobs <- 
    .tweet_raw_data |> 
    purrr::pluck("tweet_data_main_raw", .default=list(tibble::tibble())) |> 
    purrr::map_int(nrow) |>
    sum()
  
  if(.tweet_data_main_nobs == 0){
    return(tweet_count_tidy_data_skeleton)
  }
  
  ##############################################################################
  
  .tweet_data_main_raw <- 
    .tweet_raw_data |> 
    dplyr::select(tweet_data_date, tweet_data_query, tweet_data_main_raw) |> 
    dplyr::mutate(
      tweet_data_query = purrr::map_chr(
        tweet_data_query, purrr::pluck, "query", .default=NA_character_
      )
    ) |> 
    tidyr::unnest(tidyselect::everything())
  
  ##############################################################################
  
  .tweet_raw_data$tweet_data_query
  
  .tweet_data_flat <- 
    .tweet_data_main_raw |> 
    dplyr::transmute(
      tweet_query_search = tweet_data_query,
      tweet_date_created_start = parse_zulutime(start),
      tweet_date_created_end = parse_zulutime(end),
      tweet_count_date_observed = tweet_data_date,
      tweet_count = as.integer(tweet_count)
    )
  
  .tweet_tidy_data <- dplyr::bind_rows(
    tweet_count_tidy_data_skeleton, .tweet_data_flat
  )
  
  return(.tweet_tidy_data)
  
}
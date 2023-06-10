#' bla
#'
#' \code{add_tweet_is_convers_start} - Convert all Emojis to some ...
#'
#' @param .tweet_tidy_data ...
#' @param .na_to_false ... \code{FALSE}
#' @return \code{add_tweet_is_convers_start} - returns a ...
#' @rdname add_tweet_is_convers_start
#' @export
#' @examples
#' 1+1
add_tweet_is_convers_start <- function(
    .tweet_tidy_data, .na_to_false=FALSE
){
  
  stopifnot(is.logical(.na_to_false) & length(.na_to_false) == 1)
  
  if(nrow(.tweet_tidy_data) == 0){
    return(dplyr::mutate(.tweet_tidy_data, tweet_is_convers_start = logical()))
  }
  
  .tweet_data_search <- dplyr::mutate(
    .tweet_tidy_data, tweet_id_initial = tweet_id
  )
  .tweet_is_convers_start <- tibble::tibble(
    tweet_id_initial = unique(bit64::integer64(0L)), 
    tweet_is_convers_start = NA
  )
  
  repeat({
    
    .tweet_is_convers_start <- 
      .tweet_data_search |> 
      dplyr::filter(
        user_id != tweet_reply_user_id | is.na(tweet_reply_user_id)
      ) |> 
      dplyr::transmute(
        tweet_id_initial, tweet_is_convers_start = tweet_id == convers_id
      ) |> 
      dplyr::bind_rows(.tweet_is_convers_start)
    
    .tweet_data_search <- 
      .tweet_data_search |> 
      dplyr::filter(user_id == tweet_reply_user_id) |> 
      dplyr::reframe(tweet_id_vec = burrr::chunk_vector(
        tweet_reply_tweet_id, .n_elems=1e3
      )) |> 
      fetch_tweet_id_raw(
        .tweet_id_vec = tweet_id_vec,
        .bearer_token=bearer_token
      ) |> 
      prep_tidy_data.tweet() |> 
      dplyr::inner_join(
        dplyr::select(.tweet_data_search, tweet_id, tweet_id_initial),
        by=c("tweet_reply_tweet_id"="tweet_id")
      )
    
    if(nrow(.tweet_data_search) == 0){break}
    
  })
  
  .tweet_tidy_data_mod <- 
    .tweet_tidy_data |> 
    dplyr::left_join(
      dplyr::rename(.tweet_is_convers_start, tweet_id = tweet_id_initial), 
      by="tweet_id"
    ) |> 
    dplyr::mutate(tweet_is_convers_start = dplyr::case_when(
      .na_to_false ~ tidyr::replace_na(tweet_is_convers_start, FALSE),
      TRUE ~ tweet_is_convers_start
    ))
  
  return(.tweet_tidy_data_mod)
  
}



get_url_info <- function(.url){
  
  .response_head <- NULL
  .url_is_valid <- NA
  
  tryCatch(
    expr={
      .response_head <- httr::HEAD(
        .url, httr::timeout(30), httr::config(http_version=2)
      )
      .url_is_valid <- TRUE
    },
    error=function(..e){
      if(..e$message == "URL using bad/illegal format or missing URL"){
        .url_is_valid <<- FALSE
      }else{
        .url_is_valid <<- TRUE
      }
    }
  )
  
  .url_unwound <- purrr::pluck(
    .response_head, "url", .default=NA_character_
  )
  
  .url_check_status <- purrr::pluck(
    .response_head, "status_code", .default=NA_integer_
  )
  
  .url_check_date <- lubridate::now(tzone="UTC")
  
  .url_info <- tibble::tibble(
    url_unwound = .url_unwound,
    url_is_valid = .url_is_valid,
    url_check_status = .url_check_status,
    url_check_date = .url_check_date
  )
  
  return(.url_info)
  
}

################################################################################

list_get_url_info <- function(
    .list_url_input, .url_info_db_path=fs::path(data_dir, "url_info_db.rds"),
    .url_info_db_retry=FALSE
){
  
  if(length(.list_url_input) == 0){return(list())}
  
  stopifnot(
    all(purrr::map_lgl(purrr::compact(.list_url_input), is.character)),
    fs::dir_exists(fs::path_dir(.url_info_db_path)),
    is.logical(.url_info_db_retry)
  )
  
  .list_url_frame <- 
    .list_url_input |> 
    unname() |> 
    tibble::enframe(name="list_index", value="url_input") |> 
    tidyr::unnest_longer(url_input, transform=as.character) |> 
    dplyr::mutate(url_is_twitter = stringr::str_detect(
      url_input, "^(http(s)?://)?(ww[w0-9]\\.)?twitter\\.com"
    ))
  
  if(fs::file_exists(.url_info_db_path)){
    .url_info_db_last <- 
      readr::read_rds(.url_info_db_path) |> 
      dplyr::filter(if(.url_info_db_retry){url_check_status == 200}else{TRUE})
  }else{
    .url_info_db_last <- tibble::tibble(url_input = character(0L))
  }
  
  .url_info_db <- 
    .list_url_frame |> 
    dplyr::filter(!url_is_twitter) |> 
    dplyr::distinct(across(url_input, as.character)) |> 
    dplyr::anti_join(.url_info_db_last, by="url_input") |> 
    dplyr::mutate(url_info = burrr::best_map(
      url_input, get_url_info, .workers=6, .scheduling=100
    )) |> 
    tidyr::unnest_wider(url_info) |> 
    dplyr::select(-tidyselect::any_of("url_info")) |> 
    dplyr::bind_rows(.url_info_db_last) |> 
    readr::write_rds(.url_info_db_path)
  
  .list_url_frame |> 
    dplyr::left_join(.url_info_db, by="url_input") |> 
    tidyr::nest(url_info=-list_index) |> 
    tidyr::complete(
      list_index = seq_along(.list_url_input), fill=list(url_info=list(NULL))
    ) |> 
    dplyr::arrange(list_index) |> 
    dplyr::pull(url_info) |> 
    rlang::set_names(names(.list_url_input))
  
}

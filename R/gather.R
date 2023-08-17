gather_tweet_raw_data <- function(
  .tweet_data_scope_grid, .fetch_fn, .storage_dir, .bearer_token, ...
){
  
  # .tweet_data_scope_grid <<- .tweet_data_scope_grid
  # .dots <<- enquos(...)
  
  .fetch_fn_name <- stringr::str_remove(
    deparse(substitute(.fetch_fn)), "^chirp:::?"
  )
  .fetch_endpoint <- dplyr::case_when(
    .fetch_fn_name == "fetch_tweet_id_raw" ~ "/2/tweets",
    .fetch_fn_name == "fetch_tweet_search_raw" ~ "/2/tweets/search/all",
    .fetch_fn_name == "fetch_tweet_count_raw" ~ "/2/tweets/counts/all",
    .fetch_fn_name == "fetch_tweet_timeline_raw" ~ "/2/users/:id/tweets",
    TRUE ~ NA_character_
  )
  
  stopifnot(!is.na(.fetch_endpoint))
  
  .tweet_data_scope_grid |> 
    dplyr::transmute(...) |> 
    burrr::chunk_df(.n_rows=1) |> 
    burrr::best_map(function(..tweet_data_scope){
      
      ..filtering_pars <- purrr::map(..tweet_data_scope, purrr::pluck, 1)
      
      ..data_path <- fs::path(
        fs::dir_create(.storage_dir), 
        digest::digest(list(.fetch_endpoint, ..filtering_pars)), 
        ext="qs"
      )
      
      if(!fs::file_exists(..data_path)){
        
        ..tweet_raw_data <- rlang::exec(
          .fetch_fn, !!!..filtering_pars, .bearer_token=.bearer_token
        )
        
        ..tweet_data_gathered <- tibble::tibble(
          tweet_data_endpoint = .fetch_endpoint,
          tweet_data_scope = list(..tweet_data_scope),
          tweet_raw_data_path = ..data_path,
          tweet_raw_data = list(..tweet_raw_data)
        )
        
        qs::qsave(..tweet_data_gathered, ..data_path)
        
      }else{
        
        ..tweet_data_gathered <- qs::qread(..data_path)
        
      }
      
      return(..tweet_data_gathered)
      
    }) |> 
    dplyr::bind_rows()
  
}

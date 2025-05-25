prep_raw_data.tweet <- function(
    .tweet_raw_list, .tweet_data_query, .tweet_data_date=lubridate::now()
){
  
  .tweet_data_main_raw <- 
    .tweet_raw_list |> 
    purrr::pluck("data", .default=tibble::tibble()) |> 
    jsonlite::flatten() |> 
    janitor::clean_names() |> 
    tibble::as_tibble() 
  
  .tweet_data_incl_user_raw <- 
    .tweet_raw_list |> 
    purrr::pluck("includes", "users", .default=tibble::tibble()) |> 
    jsonlite::flatten() |> 
    janitor::clean_names() |> 
    tibble::as_tibble()
  
  .tweet_data_incl_tweet_raw <- 
    .tweet_raw_list |> 
    purrr::pluck("includes", "tweets", .default=tibble::tibble()) |> 
    jsonlite::flatten() |> 
    janitor::clean_names() |> 
    tibble::as_tibble()
  
  .tweet_data_incl_media_raw <- 
    .tweet_raw_list |> 
    purrr::pluck("includes", "media", .default=tibble::tibble()) |> 
    jsonlite::flatten() |> 
    janitor::clean_names() |> 
    tibble::as_tibble()
  
  .tweet_data_incl_places_raw <- 
    .tweet_raw_list |> 
    purrr::pluck("includes", "places", .default=tibble::tibble()) |> 
    jsonlite::flatten() |> 
    janitor::clean_names() |> 
    tibble::as_tibble()
  
  .tweet_data_incl_polls_raw <- 
    .tweet_raw_list |> 
    purrr::pluck("includes", "polls", .default=tibble::tibble()) |> 
    jsonlite::flatten() |> 
    janitor::clean_names() |> 
    tibble::as_tibble()
  
  .tweet_data_incl_topics_raw <- 
    .tweet_raw_list |> 
    purrr::pluck("includes", "topics", .default=tibble::tibble()) |> 
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
    tweet_data_incl_tweet_raw = list(.tweet_data_incl_tweet_raw),
    tweet_data_incl_user_raw = list(.tweet_data_incl_user_raw),
    tweet_data_incl_media_raw = list(.tweet_data_incl_media_raw),
    tweet_data_incl_places_raw = list(.tweet_data_incl_places_raw),
    tweet_data_incl_polls_raw = list(.tweet_data_incl_polls_raw),
    tweet_data_incl_topics_raw = list(.tweet_data_incl_topics_raw),
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
prep_tidy_data.tweet <- function(
  .tweet_raw_data, .include_media_url=FALSE
){
  
  # .tweet_raw_data <<- .tweet_raw_data#; stop()
  
  if(nrow(.tweet_raw_data) == 0){
    return(tweet_tidy_data_skeleton)
  }
  
  .tweet_data_main_nobs <- 
    .tweet_raw_data |> 
    purrr::pluck("tweet_data_main_raw", .default=list(tibble::tibble())) |> 
    purrr::map_int(nrow) |>
    sum()
  
  if(.tweet_data_main_nobs == 0){
    return(tweet_tidy_data_skeleton)
  }
  
  .tweet_data_incl_user_raw <- 
    .tweet_raw_data |> 
    dplyr::select(tweet_data_date, tweet_data_incl_user_raw) |> 
    tidyr::unnest(tidyselect::everything())
  
  if(nrow(.tweet_data_incl_user_raw) > 0){
    
    .tweet_data_incl_user <- 
      .tweet_data_incl_user_raw |> 
      jsonlite::flatten() |> 
      janitor::clean_names() |> 
      tibble::as_tibble() |> 
      dplyr::group_by(id) |> 
      dplyr::slice_max(tweet_data_date, with_ties=FALSE) |>
      dplyr::ungroup() |> 
      dplyr::transmute(
        user_id = bit64::as.integer64(id), 
        screen_name = username, 
        display_name = name, 
        date_observed = lubridate::with_tz(tweet_data_date, tzone="UTC"),
        date_created = parse_zulutime(created_at), 
        is_verified = verified,
        tweet_count = public_metrics_tweet_count, 
        follower_count = public_metrics_followers_count, 
        following_count = public_metrics_following_count, 
        listed_count = public_metrics_listed_count
      ) |> 
      dplyr::rename_with(
        stringr::str_replace, tidyselect::everything(), 
        "^(?!(user_|.*_id$))", "user_"
      )
    
  }else{
    
    .tweet_data_incl_user <- tibble::tibble(
      user_id = bit64::integer64(), 
      screen_name = character(), 
      display_name = character(), 
      date_observed = lubridate::as_datetime(character(), tz="UTC"),
      date_created = lubridate::as_datetime(character(), tz="UTC"), 
      is_verified = logical(),
      tweet_count = integer(), 
      follower_count = integer(), 
      following_count = integer(), 
      listed_count = integer()
    )
    
  }
  
  ##############################################################################
  
  .tweet_data_incl_tweet_raw <- 
    .tweet_raw_data |>
    dplyr::select(tweet_data_date, tweet_data_incl_tweet_raw) |> 
    tidyr::unnest(tidyselect::everything())
  
  if(nrow(.tweet_data_incl_tweet_raw) > 0){
    
    .tweet_data_incl_tweet <- 
      .tweet_data_incl_tweet_raw |> 
      dplyr::group_by(id) |> 
      dplyr::slice_max(tweet_data_date, with_ties=FALSE) |> 
      dplyr::ungroup() |> 
      dplyr::transmute(
        tweet_id = bit64::as.integer64(id),
        user_id = bit64::as.integer64(author_id)
      )
    
  }else{
    
    .tweet_data_incl_tweet <- tibble::tibble(
      tweet_id = bit64::integer64(),
      user_id = bit64::integer64()
    )
    
  }
  
  
  ##############################################################################
  
  # `%||%` <- rlang::`%||%`
  
  .tweet_data_incl_media <- 
    .tweet_raw_data |>
    dplyr::select(tweet_data_incl_media_raw) |> 
    tidyr::unnest(tidyselect::everything()) |> 
    dplyr::transmute(
      media_key = purrr::pluck(
        dplyr::pick(tidyselect::everything()), "media_key", 
        .default=NA_character_
      ), 
      media_type = purrr::pluck(
        dplyr::pick(tidyselect::everything()), "type", 
        .default=NA_character_
      ), 
      media_url_full = purrr::pluck(
        dplyr::pick(tidyselect::everything()), "url", 
        .default=NA_character_
      ), 
      media_url_preview = purrr::pluck(
        dplyr::pick(tidyselect::everything()), "preview_image_url", 
        .default=NA_character_
      )
    )
  
  ##############################################################################
  
  .tweet_data_main_raw <- 
    .tweet_raw_data |> 
    dplyr::select(tweet_data_date, tweet_data_main_raw) |> 
    tidyr::unnest(tidyselect::everything()) |> 
    dplyr::group_by(id) |> 
    dplyr::slice_max(tweet_data_date, with_ties=FALSE) |> 
    dplyr::ungroup()
  
  ##############################################################################
  
  .tweet_data_flat <- 
    .tweet_data_main_raw |> 
    dplyr::transmute(
      tweet_id = bit64::as.integer64(id),
      lang,
      text,
      user_id = bit64::as.integer64(author_id),
      convers_id = bit64::as.integer64(conversation_id),
      date_created = parse_zulutime(created_at),
      date_observed = lubridate::with_tz(tweet_data_date, tzone="UTC"),
      is_edited = lengths(edit_history_tweet_ids) > 1,
      possibly_sensitive,
      retweet_count = public_metrics_retweet_count,
      reply_count = public_metrics_reply_count,
      like_count = public_metrics_like_count,
      quote_count = public_metrics_quote_count,
      impression_count = public_metrics_impression_count
    ) |> 
    dplyr::rename_with(
      stringr::str_replace, tidyselect::everything(), 
      "^(?!(tweet_|.*_id$))", "tweet_"
    )
  
  ##############################################################################
  
  if(rlang::has_name(.tweet_data_main_raw, "referenced_tweets")){
    
    .tweet_data_ref <- 
      .tweet_data_main_raw |> 
      dplyr::transmute(
        tweet_id_top = bit64::as.integer64(id), referenced_tweets
      ) |> 
      tidyr::unnest(tidyselect::everything()) |> 
      tidyr::drop_na() |> 
      dplyr::distinct() |> 
      dplyr::transmute(
        tweet_id_top,
        tweet_id = bit64::as.integer64(id),
        ref_type = dplyr::recode(type, "replied_to"="reply", "quoted"="quote")
      ) |> 
      dplyr::left_join(.tweet_data_incl_tweet, by="tweet_id") |> 
      dplyr::left_join(
        dplyr::select(.tweet_data_incl_user, user_id, user_screen_name), 
        by="user_id"
      ) |> 
      tidyr::pivot_wider(
        id_cols=tweet_id_top, names_from=ref_type, 
        values_from=c(tweet_id, user_id, user_screen_name),
        names_glue="tweet_{ref_type}_{.value}", names_vary="slowest"
      ) |> 
      dplyr::rename(tweet_id = tweet_id_top)
    
  }else{
    
    .tweet_data_ref <- tibble::tibble(
      tweet_id=bit64::integer64(), 
      tweet_reply_tweet_id=bit64::integer64(),
      tweet_reply_user_id=bit64::integer64(),
      tweet_reply_user_screen_name=character()
    )
    
  }
  
  ##############################################################################
  
  if(rlang::has_name(.tweet_data_main_raw, "entities_urls")){
    
    .tweet_data_url_base <- 
      .tweet_data_main_raw |> 
      dplyr::transmute(tweet_id = bit64::as.integer64(id), entities_urls) |> 
      tidyr::unnest(entities_urls) |> 
      janitor::clean_names() |> 
      tibble::as_tibble() |> 
      dplyr::transmute(
        tweet_id, url_expanded = expanded_url, url_display = display_url
      ) |> 
      tidyr::nest(tweet_url_base = -tweet_id)
    
  }else{
    
    .tweet_data_url_base <- tibble::tibble(
      tweet_id = bit64::as.integer64(0L), tweet_url_base = list()
    )
    
  }
  
  ##############################################################################
  
  if(rlang::has_name(.tweet_data_main_raw, "context_annotations")){
    
    .tweet_data_annotation <- 
      .tweet_data_main_raw |> 
      dplyr::transmute(
        tweet_id = bit64::as.integer64(id), context_annotations
      ) |> 
      tidyr::unnest(context_annotations) |> 
      jsonlite::flatten() |> 
      janitor::clean_names() |> 
      tibble::as_tibble() |> 
      dplyr::select(tweet_id, domain_name, entity_name) |> 
      tidyr::nest(tweet_annotation = -tweet_id)
    
  }else{
    
    .tweet_data_annotation <- tibble::tibble(tweet_id = bit64::integer64(0L))
    
  }
  
  ##############################################################################
  
  .tweet_data_media_base <- 
    .tweet_data_main_raw |> 
    dplyr::transmute(
      tweet_id = bit64::as.integer64(id), 
      attachments_media_keys = purrr::pluck(
        dplyr::pick(tidyselect::everything()), "attachments_media_keys", 
        .default=list(character(0L))
      )
    ) |> 
    tidyr::unnest_longer(attachments_media_keys, values_to="media_key") |> 
    dplyr::inner_join(
      .tweet_data_incl_media, by="media_key", multiple="all", 
      relationship="many-to-many"
    )
  
  .tweet_data_media_type <- 
    .tweet_data_media_base |> 
    dplyr::transmute(
      tweet_id, 
      media_type = dplyr::recode(media_type, "animated_gif"="gif"), 
      has = !is.na(media_type)
    ) |> 
    dplyr::distinct() |> 
    tidyr::pivot_wider(
      id_cols=tweet_id, names_from=media_type, values_from=has, 
      values_fill=FALSE, names_glue="tweet_media_has_{media_type}"
    ) |> 
    dplyr::mutate(tweet_media_has_any = TRUE) |> 
    tidyr::complete(
      tweet_id = bit64::as.integer64(.tweet_data_main_raw$id)
    ) |> 
    dplyr::mutate(across(-tweet_id, function(..vec){
      tidyr::replace_na(..vec, FALSE)
    }))
  
  if(.include_media_url){
    
    .tweet_data_media_url <- 
      .tweet_data_media_base |> 
      dplyr::transmute(
        tweet_id,
        media_key, 
        media_type,
        media_url_is_preview = is.na(media_url_full),
        media_url = dplyr::coalesce(media_url_preview, media_url_full)
      ) |> 
      tidyr::nest(tweet_media_url = -tweet_id)
    
  }else{
    
    .tweet_data_media_url <- tibble::tibble(tweet_id = bit64::integer64(0L))
    
  }
  
  .tweet_tidy_data <- dplyr::bind_rows(
    tweet_tidy_data_skeleton,
    .tweet_data_flat |> 
      dplyr::left_join(.tweet_data_incl_user, by="user_id") |> 
      dplyr::left_join(.tweet_data_annotation, by="tweet_id") |> 
      dplyr::left_join(.tweet_data_url_base, by="tweet_id") |> 
      dplyr::left_join(.tweet_data_media_type, by="tweet_id") |> 
      dplyr::left_join(.tweet_data_media_url, by="tweet_id") |> 
      dplyr::left_join(.tweet_data_ref, by="tweet_id") 
    )
  
  return(.tweet_tidy_data)
  
}
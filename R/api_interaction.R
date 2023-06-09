GET_twitter_safely <- function(...){
  
  .args <- list(...)
  
  repeat{
    
    .response <- NULL
    .cond <- NULL
    
    tryCatch(
      expr={.response <- rlang::exec(httr::GET, !!!.args)},
      error=function(..e){.cond <<- ..e},
      warning=function(..w){.cond <<- ..w}
    )
    
    .response_is_valid <- is(.response, "response")
    .cond_is_valid <- is(.cond, "condition")
    .cond_class <- dplyr::case_when(
      !.cond_is_valid ~ NA_character_,
      is(.cond, "error") ~ "Error",
      is(.cond, "warning") ~ "Warning",
      TRUE ~ "Unknown Condition"
    )
    .cond_message <- purrr::pluck(.cond, "message", .default="")
    .cond_is_retry <- stringr::str_detect(.cond_message, "rate_limit")
    
    if(.response_is_valid & !.cond_is_retry){
      if(httr::status_code(.response) == 429){
        .rate_limit_reset <- 
          httr::headers(.response) |> 
          purrr::pluck("x-rate-limit-reset") |> 
          as.integer() |> 
          lubridate::as_datetime(origin="1970-01-01 UTC")
        message(
          "Waiting for the rate limit reset on ", 
          lubridate::with_tz(.rate_limit_reset, tzone="")
        )
        naptime::naptime(.rate_limit_reset)
        next
      }else if(.cond_is_valid){
        message(
          format(Sys.time()), " - Valid result eventhough \"", .cond_class, 
          ": ", .cond_message, "\" occurred"
        )
      }
      break
    }else if(.cond_is_retry){
      message(
        format(Sys.time()), " - Retrying in 10s due to \"", .cond_class, 
        ": ", .cond_message, "\""
      )
      naptime::naptime(lubridate::seconds(10))
      next
    }else{
      stop(.cond_message, " (originated as ", .cond_class, ")")
    }
    
  }
  
  return(.response)
  
}


form_tweet_query <- function(
    ..., .incl_context_annotations, .pars_static=NULL
){
  
  .pars_filter <- list(...)
  
  stopifnot(
    is.list(.pars_filter),
    !is.null(names(.pars_filter)),
    !any(duplicated(names(.pars_filter))),
    .incl_context_annotations %in% c(FALSE, TRUE)
  )
  
  if(is.null(.pars_static)){
    
    .pars_static <- list(
      tweet.fields = stringr::str_c(
        "attachments,author_id,conversation_id,created_at,entities,geo,id,",
        "in_reply_to_user_id,lang,public_metrics,possibly_sensitive,",
        "referenced_tweets,source,text,withheld",
        dplyr::if_else(.incl_context_annotations, ",context_annotations", "")
      ),
      expansions = stringr::str_c(
        "author_id,entities.mentions.username,geo.place_id,",
        "in_reply_to_user_id,referenced_tweets.id,",
        "referenced_tweets.id.author_id,attachments.media_keys"
      ),
      user.fields = stringr::str_c(
        "created_at,description,entities,id,location,name,pinned_tweet_id,",
        "profile_image_url,protected,public_metrics,url,username,verified,",
        "withheld"
      ),
      place.fields = stringr::str_c(
        "contained_within,country,country_code,full_name,geo,id,name,place_type"
      ),
      media.fields = stringr::str_c(
        "duration_ms,height,media_key,preview_image_url,",
        "public_metrics,type,url,width,alt_text"
      )
    )
    
  }
  
  .query <- c(.pars_filter, .pars_static)
  
  stopifnot(!any(duplicated(names(.query))))
  
  return(.query)
  
}
# all possible fields except for:
# `non_public_metrics`, `organic_metrics`, `promoted_metrics` ~> error
# `confirmed_email` ~> empty `includes.users`
query_pars_static_dict <- list(
  tweet=list(
    tweet.fields = stringr::str_c(
      "article,attachments,author_id,card_uri,community_id,",
      "context_annotations,conversation_id,created_at,display_text_range,",
      "edit_controls,edit_history_tweet_ids,entities,geo,id,",
      "in_reply_to_user_id,lang,media_metadata,note_tweet,possibly_sensitive,",
      "public_metrics,referenced_tweets,reply_settings,scopes,source,text,",
      "withheld" 
    ),
    expansions = stringr::str_c(
      "article.cover_media,article.media_entities,attachments.media_keys,",
      "attachments.media_source_tweet,attachments.poll_ids,author_id,",
      "edit_history_tweet_ids,entities.mentions.username,geo.place_id,",
      "in_reply_to_user_id,entities.note.mentions.username,",
      "referenced_tweets.id,referenced_tweets.id.attachments.media_keys,",
      "referenced_tweets.id.author_id"
    ),
    media.fields = stringr::str_c(
      "alt_text,duration_ms,height,media_key,preview_image_url,public_metrics,",
      "type,url,variants,width"
    ),
    poll.fields = stringr::str_c(
      "duration_minutes,end_datetime,id,options,voting_status"
    ),
    user.fields = stringr::str_c(
      "affiliation,connection_status,created_at,description,entities,id,",
      "is_identity_verified,location,most_recent_tweet_id,name,parody,",
      "pinned_tweet_id,profile_banner_url,profile_image_url,protected,",
      "public_metrics,receives_your_dm,subscription,subscription_type,url,",
      "username,verified,verified_followers_count,verified_type,withheld"
    ),
    place.fields = stringr::str_c(
      "contained_within,country,country_code,full_name,geo,id,name,place_type"
    )
  ),
  search_count = list(
    search_count.fields=stringr::str_c(
      "end,start,tweet_count"
    )
  )
)

usethis::use_data(query_pars_static_dict, overwrite=TRUE, internal=TRUE)

tweet_pars_old <- list(
  tweet.fields = stringr::str_c(
    "attachments,author_id,conversation_id,created_at,entities,geo,id,",
    "in_reply_to_user_id,lang,public_metrics,possibly_sensitive,",
    "referenced_tweets,source,text,withheld",
    dplyr::if_else(TRUE, ",context_annotations", "")
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

tweet_pars_new <- query_pars_static_dict$tweet


list(old=tweet_pars_old, new=tweet_pars_new) |> 
  purrr::imap(\(.list, .name){
    .list |> 
      purrr::map(\(..elem){
        ..elem |> stringi::stri_split_fixed(",") |> purrr::pluck(1) |> sort()
      }) |> 
      rlang::set_names(\(..name){stringi::stri_c(..name, ".", .name)})
  }) |> 
  purrr::list_c() |> 
  (\(.list){.list[order(names(.list))]})()
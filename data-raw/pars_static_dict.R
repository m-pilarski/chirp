pars_static_dict <- list(
  search=list(
    tweet.fields = stringr::str_c(
      "article,attachments,author_id,card_uri,community_id,",
      "context_annotations,conversation_id,created_at,display_text_range,",
      "edit_controls,edit_history_tweet_ids,entities,geo,id,",
      "in_reply_to_user_id,lang,media_metadata,non_public_metrics,",
      "note_tweet,organic_metrics,possibly_sensitive,promoted_metrics,",
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
      "alt_text,duration_ms,height,media_key,non_public_metrics,",
      "organic_metrics,preview_image_url,promoted_metrics,public_metrics,",
      "type,url,variants,width"
    ),
    poll.fields = stringr::str_c(
      "duration_minutes,end_datetime,id,options,voting_status"
    ),
    user.fields = stringr::str_c(
      "affiliation,confirmed_email,connection_status,created_at,",
      "description,entities,id,is_identity_verified,location,",
      "most_recent_tweet_id,name,parody,pinned_tweet_id,profile_banner_url,",
      "profile_image_url,protected,public_metrics,receives_your_dm,",
      "subscription,subscription_type,url,username,verified,",
      "verified_followers_count,verified_type,withheld"
    ),
    place.fields = stringr::str_c(
      "contained_within,country,country_code,full_name,geo,id,name,place_type"
    )
  )
)

usethis::use_data(pars_static_dict, overwrite=TRUE)

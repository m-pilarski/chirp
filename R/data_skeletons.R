tweet_tidy_data_skeleton <- tibble::tibble(
  tweet_id = bit64::integer64(),
  tweet_lang = character(),
  tweet_text = character(),
  user_id = bit64::integer64(),
  convers_id = bit64::integer64(),
  tweet_date_created = lubridate::as_datetime(character()),
  tweet_date_observed = lubridate::as_datetime(character()),
  tweet_is_edited = logical(),
  tweet_possibly_sensitive = logical(),
  tweet_retweet_count = integer(),
  tweet_reply_count = integer(),
  tweet_like_count = integer(),
  tweet_quote_count = integer(),
  tweet_impression_count = integer(),
  user_screen_name = character(),
  user_display_name = character(),
  user_date_observed = lubridate::as_datetime(character()),
  user_date_created = lubridate::as_datetime(character()),
  user_is_verified = logical(),
  user_tweet_count = integer(),
  user_follower_count = integer(),
  user_following_count = integer(),
  user_listed_count = integer(),
  tweet_annotation = list(),
  tweet_url_base = list(),
  tweet_media_has_photo = logical(),
  tweet_media_has_video = logical(),
  tweet_media_has_gif = logical(),
  tweet_media_has_any = logical(),
  tweet_media_url = list(),
  tweet_reply_tweet_id = bit64::integer64(),
  tweet_reply_user_id = bit64::integer64(),
  tweet_reply_user_screen_name = character(),
  tweet_quote_tweet_id = bit64::integer64(),
  tweet_quote_user_id = bit64::integer64(),
  tweet_quote_user_screen_name = character()
)

url_info_empty_df <- tibble::tibble(
  url_input = character(),
  url_is_twitter = logical(),
  url_unwound = character(),
  url_is_valid = logical(),
  url_check_status = integer(),
  url_check_date = lubridate::as_datetime(character())
)

tweet_count_tidy_data_skeleton <- tibble::tibble(
  tweet_query_search = character(),
  tweet_date_created_start = lubridate::as_datetime(character()),
  tweet_date_created_end = lubridate::as_datetime(character()),
  tweet_count_date_observed = lubridate::as_datetime(character()),
  tweet_count = integer()
)

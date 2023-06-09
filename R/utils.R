format_zulutime <- function(.datetime){
  format.Date(
    x=lubridate::with_tz(.datetime, tzone="UTC"), format="%Y-%m-%dT%H:%M:%SZ"
  )
}

parse_zulutime <- function(.str){
  lubridate::as_datetime(.str, format="%Y-%m-%dT%H:%M:%S.000Z", tz="UTC")
}

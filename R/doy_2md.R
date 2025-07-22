#' Provide month and day from day of year. Uses 2019 as the model year.
#'
#' Useful as labeling function in scale_x_continuous or scale_y_continuous
#'
#' @param i day of year
#'
#' @return character of form "Month day"
#' @export
#'
#' @examples
#' doy_2md(100)
doy_2md=function(i){
  ymd=as.Date(i-1, origin="2019-01-01")
  return(format(ymd, "%b %d"))
}

#' Provide month from day of year. Uses 2019 as the model year.
#'
#' @param i day of year
#'
#' @return character of form "Month"
#' @export
#'
#' @examples
#' doy_2m(100)
#'
doy_2m=function(i){
  ymd=as.Date(i-1, origin="2019-01-01")
  return(format(ymd, "%b"))
}

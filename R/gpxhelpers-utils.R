## Set some constants
### These are good for lighter maps ... e.g., ESRI
#clrs <- c("Highway"="red3","Paved"="gray35",
#          "Gravel"="darkorange4","Offroad"="darkorange2",
#          "Trail"="forestgreen")
#cclrs <- c("dodgerblue1","dodgerblue4")
### These are better for darker maps ... e.g., Bing
clrs <- c("Highway"="red3","Paved"="gray60",
          "Gravel"="darkorange3","Offroad"="goldenrod2",
          "Trail"="green3")
cclrs <- c("gold","darkorange3")

ltyps <- c("Public"="solid","Private"="dashed")
szs <- c("Highway"=1.5,"Paved"=1.25,
         "Gravel"=1.25,"Offroad"=1.25,"Trail"=1)


## Used to fix "no visible binding" issue for dplyr verbs
## from here ... https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887/2
## See example in iCombineAllTracks2CXV
#' @importFrom rlang .data

## Used to fix "no visible binding" issue for common ggplot2 functions
#' @importFrom ggplot2 ggplot aes geom_path geom_point geom_line geom_ribbon geom_label geom_text scale_color_manual scale_color_gradient scale_linetype_manual scale_size_manual scale_y_continuous scale_x_continuous expansion coord_sf labs theme_minimal theme element_blank

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' @title Calculate distance along the track.
#' 
#' @description Calculates the cumulative distance along the track.
#' 
#' @param d Data frame that contains the track coordinates.
#' @param vars A string vector with the names in  \code{d} that contain the longitude and latitude coordinars (in that order) along the track.
#' @param units A string with the desired units for the distances.
#' 
#' @details NONE YET
#' 
#' @seealso \code{\link[geosphere]{distGeo}}
#' 
#' @return A numeric vector with cumulative distances along the track.
#' 
#' @author Derek H. Ogle
#' @keywords manip
#' 
#' @examples
#' ## None yet.
#' 
#' @export
distAlongTrack <- function(d,vars=c("Longitude","Latitude"),
                           units=c("miles","km","m")) {
  units <- match.arg(units)
  n <- nrow(d)
  ## Make vector of cumulative distance along track ... defaults to meters
  res <- cumsum(c(0,geosphere::distGeo(d[1:(n-1),vars],d[2:n,vars])))
  ## Convert to other units if necessary
  res <- dplyr::case_when(
    units=="km" ~ res/1000,
    units=="miles" ~ res*0.00062137,
    units=="feet" ~ res*3.2808399,
    TRUE ~ res
  )
  ## Return the result
  res
}
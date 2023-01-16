## Used to fix "no visible binding" issue for dplyr verbs
## from here ... https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887/2
## See example in iCombineAllTracks2CSV
#' @importFrom rlang .data

## Used to fix "no visible binding" issue for common ggplot2 functions
#' @importFrom ggplot2 ggplot aes geom_path geom_point geom_line geom_ribbon geom_label geom_text scale_color_manual scale_color_gradient scale_linetype_manual scale_size_manual scale_y_continuous scale_x_continuous expansion coord_sf labs theme_minimal theme element_blank

## Used to fix "no visible binding" issue for common leaflet functions
#' @importFrom leaflet leaflet addTiles addProviderTiles fitBounds addPolylines addLabelOnlyMarkers addRectangles labelOptions highlightOptions addLayersControl layersControlOptions addMeasure



#' @title Compare file names in a directory to names in the information file.
#' 
#' @description Compare file names in a directory to names in the information file to see if any tracks are missing in either place.
#' 
#' @param pin Path to directory with track files.
#' @param trkinfo Database with track information.
#' 
#' @details NONE YET
#' 
#' @return None, but informative messages will be written as side effects
#' 
#' @author Derek H. Ogle
#' @keywords manip
#' 
#' @examples
#' ## None yet.
#' 
#' @export
compareFiles2Info <- function(pin,trkinfo) {
  trksInWD <- tools::file_path_sans_ext(list.files(pattern="gpx",path=pin))
  trksInInfo <- trkinfo$trackID
  tmp <- !trksInWD %in% trksInInfo
  if (any(tmp))
    cli::cli_alert_danger("Tracks in '{pin}' not in info file: {paste(trksInWD[tmp],collapse=' ')}")
  else
    cli::cli_alert_success("All tracks in '{pin}' are in the info file!")
  cat("\n")
  tmp <- !trksInInfo %in% trksInWD
  if (any(tmp))
    cli::cli_alert_danger("Tracks in the info file not in '{pin}': {paste(trksInInfo[tmp],collapse=' ')}")
  else
    cli::cli_alert_success("All tracks in info file are in '{pin}'")
}


#' @title Calculate distance along the track.
#' 
#' @description Calculates the cumulative distance along the track.
#' 
#' @param d Data frame that contains the track coordinates.
#' @param vars A string vector with the names in  \code{d} that contain the longitude and latitude coordinates (in that order) along the track.
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

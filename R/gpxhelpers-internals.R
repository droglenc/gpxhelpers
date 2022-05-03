#' @title Internal functions.
#'
#' @description Internal functions that are common to several functions in gpxhelpers.
#'
#' @rdname gpxhelpers-internals
#' @keywords internal
#' @aliases iWalkSumPts

iWalkSumPts <- function(walkdat) {
  ## Find the starting points for each track
  hdwalk <- walkdat %>%
    dplyr::group_by(.data$trknum,.data$trackID) %>%
    dplyr::slice_head(n=1L) %>%
    dplyr::rename(start_Lat=.data$Latitude,start_Lon=.data$Longitude,
                  start_Dist=.data$Distance,start_Elev=.data$Elevation) %>%
    dplyr::select(-.data$Time)
  ## Find the ending point for each track
  tlwalk <- walkdat %>%
    dplyr::group_by(.data$trknum,.data$trackID) %>%
    dplyr::slice_tail(n=1L) %>%
    dplyr::rename(end_Lat=.data$Latitude,end_Lon=.data$Longitude,
                  end_Dist=.data$Distance,end_Elev=.data$Elevation) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$trknum,.data$trackID,dplyr::starts_with("end"))
  ## Combine starting and ending points ...
  ## ... and calculate distance, elevation change, and midpoint of each track
  sumwalk <- dplyr::left_join(hdwalk,tlwalk,by="trknum") %>%
    dplyr::rename(trackID=.data$trackID.x) %>%
    dplyr::select(-.data$trackID.y) %>%        
    dplyr::mutate(Distance=.data$end_Dist-.data$start_Dist,
                  DeltaElev=.data$end_Elev-.data$start_Elev,
                  midpt_Lat=geosphere::midPoint(p1=c(.data$start_Lon,.data$start_Lat),
                                                p2=c(.data$end_Lon,.data$end_Lat))[[1,"lat"]],
                  midpt_Lon=geosphere::midPoint(p1=c(.data$start_Lon,.data$start_Lat),
                                                p2=c(.data$end_Lon,.data$end_Lat))[[1,"lon"]])
  ## Return
  sumwalk
}

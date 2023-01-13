#' @title Internal functions.
#'
#' @description Internal functions that are common to several functions in gpxhelpers.
#'
#' @rdname gpxhelpers-internals
#' @keywords internal
#' @aliases iWalkSumPts iBearing iMakeDescription

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
  ## Find a midpoint for each track
  mpwalk <- walkdat %>%
    dplyr::group_by(.data$trknum,.data$trackID) %>%
    dplyr::slice(n=floor(length(.data$Longitude)/2)) %>%
    dplyr::rename(midpt_Lat=.data$Latitude,midpt_Lon=.data$Longitude) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$trknum,.data$trackID,dplyr::starts_with("midpt"))
  ## Find bearing near midpoint
  mpbear <- walkdat %>%
    dplyr::group_by(.data$trknum,.data$trackID) %>%
    dplyr::summarize(midpt_Bearing=iBearing(.data$Longitude,.data$Latitude)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(duped=duplicated(.data$trackID),
                  lbldir=dplyr::case_when(
                  .data$midpt_Bearing %in% c("North","South") & !.data$duped ~ "left",
                  .data$midpt_Bearing %in% c("North","South") & .data$duped ~ "right",
                  .data$midpt_Bearing %in% c("East","West") & !.data$duped ~ "top",
                  .data$midpt_Bearing %in% c("East","West") & .data$duped ~ "bottom")
    ) %>%
    dplyr::select(-.data$duped)
  mpwalk <- dplyr::left_join(mpwalk,mpbear,by="trknum") %>%
    dplyr::rename(trackID=.data$trackID.x) %>%
    dplyr::select(-.data$trackID.y)
  ## Combine starting, ending, and mid points ...
  ## ... and calculate distance, elevation change, and midpoint of each track
  sumwalk <- dplyr::left_join(hdwalk,tlwalk,by="trknum") %>%
    dplyr::rename(trackID=.data$trackID.x) %>%
    dplyr::select(-.data$trackID.y) %>%
    dplyr::mutate(tDistance=.data$end_Dist-.data$start_Dist,
                  DeltaElev=.data$end_Elev-.data$start_Elev) %>%
    dplyr::left_join(mpwalk,by="trknum") %>%
    dplyr::rename(trackID=.data$trackID.x) %>%
    dplyr::select(-.data$trackID.y)
  ## Return
  sumwalk
}


iBearing <- function(Lon,Lat,buf=5) {
  mp <- floor(length(Lon)/2)
  if (mp<=buf) buf <- floor(mp/2)
  res <- (geosphere::bearing(c(Lon[mp-buf],Lat[mp-buf]),
                             c(Lon[mp+buf],Lat[mp+buf])) + 360) %% 360
  res <- res[!is.na(res)]
  dplyr::case_when(
    res<45 ~ "South",
    res<135 ~ "East",
    res<225 ~ "North",
    res<315 ~ "West",
    TRUE ~ "South"
  )
}



iMakeDescription <- function(Primary,From,To) {
  if (!is.na(From) & !is.na(To)) desc <- paste(From,"to",To)
  else if (is.na(From) & is.na(To)) desc <- Primary
  else if (is.na(From)) desc <- paste("at",To)
  else desc <- paste("at",From)
}



iRetClr <- function(x) {
  clrs <- c("Highway"="#CC0000","Paved"="#336666",
            "Gravel"="#CC6600","Offroad"="#CC9900",
            "Trail"="#999933", "ATV"="#663300")
  clrs[[which(names(clrs)==x$Type[1])]]
}
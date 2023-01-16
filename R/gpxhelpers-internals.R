#' @title Internal functions.
#'
#' @description Internal functions that are common to several functions in gpxhelpers.
#'
#' @rdname gpxhelpers-internals
#' @keywords internal
#' @aliases iBaseMap iBearing iMakeDescription iOrderWalk iRetClr iSwapFromTo iWalkSumPts

## INTERNAL: Build base leaflet map
iBaseMap <- function(dat,LAT_bottom,LAT_top,LON_left,LON_right,map_bufr) {
  ## Handle map bounding box
  rngLon <- range(dat$Longitude)*c(1-map_bufr,1+map_bufr)
  rngLat <- range(dat$Latitude)*c(1-map_bufr,1+map_bufr)
  if (is.null(LAT_bottom)) LAT_bottom <- rngLat[1]
  if (is.null(LAT_top)) LAT_top <- rngLat[2]
  if (is.null(LON_left)) LON_left <- rngLon[1]
  if (is.null(LON_right)) LON_right <- rngLon[2]
  ## Make the underlyling leaflet map
  leaflet() |>
    addTiles(group="Default") |>
    addProviderTiles(provider="OpenTopoMap",group="Topo/Roads") |>
    addProviderTiles(provider="Esri.WorldImagery",group="Imagery") |>
    addProviderTiles(provider="CartoDB.PositronNoLabels",group="CartoDB") |>
    fitBounds(LON_left,LAT_bottom,LON_right,LAT_top) |>
    addLayersControl(
      baseGroups=c("Default","Topo/Roads","Imagery","CartoDB"),
      options=layersControlOptions(collapsed=TRUE)
    ) |>
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "miles",
      primaryAreaUnit = "sqmiles",
      activeColor = "#ff4932",
      completedColor = "#e2365d")
}



## INTERNAL: Determine a cardinal direction bearing for a walk
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



## INTERNAL: Make description for track
iMakeDescription <- function(Primary,From,To) {
  if (!is.na(From) & !is.na(To)) desc <- paste(From,"to",To)
  else if (is.na(From) & is.na(To)) desc <- Primary
  else if (is.na(From)) desc <- paste("at",To)
  else desc <- paste("at",From)
}



## INTERNAL: Arrange tracks to be chained together as a walk
iOrderWalk <- function(walkIDs,trkinfo,startIDs=walkIDs[1:2]) {
  ## Initiate track order for walk
  walk <- startIDs
  ## Remove starting tracks from list of walk IDs
  walkIDs <- walkIDs[!(walkIDs %in% startIDs)]
  ##
  repeat {
    ## Get info for current track
    tmp <- dplyr::filter(trkinfo,.data$trackID==walk[length(walk)])
    ## Find if a track exists to connect to
    tmp <- tmp$Connected
    tmp <- unlist(strsplit(tmp,", "))
    next_step <- walkIDs %in% tmp
    ## If one exists add it to the walk, otherwise stop
    if (any(next_step)) {
      ## Add it to the walk
      walk <- c(walk,walkIDs[next_step])
      ## Remove the current track from walkIDs
      walkIDs <- walkIDs[!next_step]
    } else {
      return(walk)
      break
    }
  }
}



## INTERNAL: Return color base on type of track
iRetClr <- function(x) {
  clrs <- c("Highway"="#CC0000","Paved"="#336666",
            "Gravel"="#CC6600","Offroad"="#CC9900",
            "Trail"="#999933", "ATV"="#663300")
  clrs[[which(names(clrs)==x$Type[1])]]
}



## INTERNAL: Swap the From and To variables in d data.frame
iSwapFromTo <- function(d) {
  tmp <- d$From
  d$From <- d$To
  d$To <- tmp
  d
}



## INTERNAL: 
iWalkSumPts <- function(walkdat) {
  ## Find the starting points for each track
  hdwalk <- walkdat |>
    dplyr::group_by(.data$trknum,.data$trackID) |>
    dplyr::slice_head(n=1L) |>
    dplyr::rename(start_Lat=.data$Latitude,start_Lon=.data$Longitude,
                  start_Dist=.data$Distance,start_Elev=.data$Elevation) |>
    dplyr::ungroup() |>
    dplyr::select(-.data$Time)
  ## Find the ending point for each track
  tlwalk <- walkdat |>
    dplyr::group_by(.data$trknum,.data$trackID) |>
    dplyr::slice_tail(n=1L) |>
    dplyr::rename(end_Lat=.data$Latitude,end_Lon=.data$Longitude,
                  end_Dist=.data$Distance,end_Elev=.data$Elevation) |>
    dplyr::ungroup() |>
    dplyr::select(.data$trknum,.data$trackID,dplyr::starts_with("end"))
  ## Find a midpoint for each track
  mpwalk <- walkdat |>
    dplyr::group_by(.data$trknum,.data$trackID) |>
    dplyr::slice(n=floor(length(.data$Longitude)/2)) |>
    dplyr::rename(midpt_Lat=.data$Latitude,midpt_Lon=.data$Longitude) |>
    dplyr::ungroup() |>
    dplyr::select(.data$trknum,.data$trackID,dplyr::starts_with("midpt"))
  ## Find bearing near midpoint and guess a label direction from that
  mpbear <- walkdat |>
    dplyr::group_by(.data$trknum,.data$trackID) |>
    dplyr::summarize(midpt_Bearing=iBearing(.data$Longitude,.data$Latitude)) |>
    dplyr::ungroup() |>
    dplyr::mutate(duped=duplicated(.data$trackID),
                  lbldir=dplyr::case_when(
                    .data$midpt_Bearing %in% c("North","South") & !.data$duped ~ "left",
                    .data$midpt_Bearing %in% c("North","South") & .data$duped ~ "right",
                    .data$midpt_Bearing %in% c("East","West") & !.data$duped ~ "top",
                    .data$midpt_Bearing %in% c("East","West") & .data$duped ~ "bottom")
    ) |>
    dplyr::select(-.data$duped)
  ## Combine all of the midpoint data
  mpwalk <- dplyr::left_join(mpwalk,mpbear,by="trknum") |>
    dplyr::rename(trackID=.data$trackID.x) |>
    dplyr::select(-.data$trackID.y)
  ## Combine starting, ending, and mid points ...
  ## ... and calculate distance, elevation change, and midpoint of EACH track
  sumwalk <- dplyr::left_join(hdwalk,tlwalk,by="trknum") |>
    dplyr::rename(trackID=.data$trackID.x) |>
    dplyr::select(-.data$trackID.y) |>
    dplyr::mutate(Distance=.data$end_Dist-.data$start_Dist,
                  DeltaElev=.data$end_Elev-.data$start_Elev) |>
    dplyr::left_join(mpwalk,by="trknum") |>
    dplyr::rename(trackID=.data$trackID.x) |>
    dplyr::select(-.data$trackID.y)
  ## Return
  sumwalk
}

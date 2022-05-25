#' @title Visualize tracks that form a walk.
#' 
#' @description Create a map or plot elevations for tracks in a data frame that form a contiguous \dQuote{walk}.
#' 
#' @param dat A data frame that contains tracks to map, contiguous tracks for \code{walkMap} that are usually made with \code{\link{walkMaker}} and all tracks for \code{allTracksMap}.
#' @param map_type A string for the type of OpenStreet map to use under the track paths. Use \code{"none"} to not using an OpenStreet map.
#' @param map_bufr A numeric that makes the OpenStreet map slightly larger than the space that the track paths require.
#' @param LAT_bottom A latitude coordinate for the bottom of the bounding box for the map. Defaults to just below the minimum latitude found in \code{trkdata}.
#' @param LAT_top A latitude coordinate for the top of the bounding box for the map. Defaults to just above the maximum latitude found in \code{trkdata}.
#' @param LON_left A longitude coordinate for the left-side of the bounding box for the map. Defaults to just left of the minimum latitude found in \code{trkdata}.
#' @param LON_right A longitude coordinate for the right-side of the bounding box for the map. Defaults to just right the maximum latitude found in \code{trkdata}.
#' @param label_tracks A logical for whether (or not) the tracks should be labeled with a unique number in \code{walkMap} or the track ID in \code{allTracksMap}.
#' @param walk A ggplot2 object made with \code{walkMap} that will be used to highlight the \dQuote{walk} on the map of all tracks.
#' 
#' @details NONE YET
#' 
#' @return Returns a \code{leaflet} object that is the map. The map will be displayed if the returned object is not assigned a name.
#' 
#' @seealso \code{\link{walkSummary}}
#' 
#' @author Derek H. Ogle
#' @keywords manip
#' 
#' @examples
#' ## None yet.
#' 
#' @rdname walkMap
#' @export 
walkMap <- function(dat,map_type=c("OpenTopoMap"),map_bufr=0.00004,
                    LAT_bottom=NULL,LAT_top=NULL,
                    LON_left=NULL,LON_right=NULL,
                    label_tracks=TRUE) {
  ## Handle default arguments
  map_type <- match.arg(map_type)
  ## Handle map bounding box
  rngLon <- range(dat$Longitude)*c(1-map_bufr,1+map_bufr)
  rngLat <- range(dat$Latitude)*c(1-map_bufr,1+map_bufr)
  if (is.null(LAT_bottom)) LAT_bottom <- rngLat[1]
  if (is.null(LAT_top)) LAT_top <- rngLat[2]
  if (is.null(LON_left)) LON_left <- rngLon[1]
  if (is.null(LON_right)) LON_right <- rngLon[2]
  ## Get summaries of tracks in the walk (for use below)
  walksum <- iWalkSumPts(dat)
  ## Make the underlyling leaflet map
  amap <- leaflet() %>%
    addProviderTiles(provider=map_type) %>%
    fitBounds(LON_left,LAT_bottom,LON_right,LAT_top)
  ## Map the walk
  for (i in walksum$trknum) {
    tmp <- dplyr::filter(dat,.data$trknum==i)
    tmp2 <- dplyr::filter(walksum,.data$trknum==i)
    amap <- amap %>%
      addPolylines(data=tmp,
                   lng=~Longitude,lat=~Latitude,
                   color=iRetClr(tmp),opacity=0.8,
                   highlightOptions=highlightOptions(color="blue"),
                   label=~htmltools::htmlEscape(paste(trackID[1],sep="<br/>")),
                   popup=paste0('<b>',tmp2$trackID,'</b><br/>',
                                tmp2$Primary,'<br/>',
                                iMakeDescription(tmp2$Primary,tmp2$From,tmp2$To),
                                '<br/>',
                                'Distance: ',
                                formatC(tmp2$Distance,format="f",digits=2),
                                ' miles<br/>',
                                'Cumulative: ',
                                formatC(tmp2$end_Dist,format="f",digits=2),
                                ' miles<br/>'))
  }
  ## Add labels if asked for
  if (label_tracks) {
    for (i in walksum$trknum) {
      tmp <- dplyr::filter(walksum,.data$trknum==i)
      amap <- amap  %>%
        addLabelOnlyMarkers(data=tmp,
                            lng=~start_Lon,lat=~start_Lat,
                            label=~as.character(trknum),
                            labelOptions=labelOptions(noHide=TRUE,
                                                      direction=tmp$lbldir))
    }
  }
  amap
}

#' @rdname walkMap
#' @export
allTracksMap <- function(dat,map_type=c("OpenTopoMap"),map_bufr=0.00004,
                         LAT_bottom=NULL,LAT_top=NULL,
                         LON_left=NULL,LON_right=NULL,
                         walk=NULL) {
  ## Handle default arguments
  map_type <- match.arg(map_type)
  ## Handle map bounding box
  rngLon <- range(dat$Longitude)*c(1-map_bufr,1+map_bufr)
  rngLat <- range(dat$Latitude)*c(1-map_bufr,1+map_bufr)
  if (is.null(LAT_bottom)) LAT_bottom <- rngLat[1]
  if (is.null(LAT_top)) LAT_top <- rngLat[2]
  if (is.null(LON_left)) LON_left <- rngLon[1]
  if (is.null(LON_right)) LON_right <- rngLon[2]
  ## Make the underlyling leaflet map
  amap <- leaflet() %>%
    addProviderTiles(provider=map_type) %>%
    fitBounds(LON_left,LAT_bottom,LON_right,LAT_top)
  ## Possibly highlight a walk
  if (!is.null(walk)) {
  }
  ## Add all of the tracks
  for (i in unique(dat$trackID)) {
    tmp <- dplyr::filter(dat,.data$trackID==i)
    amap <- amap %>%
      addPolylines(data=tmp,
                   lng=~Longitude,lat=~Latitude,
                   color=iRetClr(tmp),opacity=0.8,
                   highlightOptions=highlightOptions(color="blue"),
                   label=~htmltools::htmlEscape(paste(trackID[1],sep="<br/>")),
                   popup=~paste0('<b>',trackID[1],'</b><br/>',
                                Primary[1],'<br/>',
                                ifelse(!is.na(From[1]),
                                       paste0("From: ",From[1],'<br/>'),""),
                                ifelse(!is.na(To[1]),
                                       paste0("To: ",To[1],'<br/>'),""),
                                "Surface: ",Type[1]))
  }
  ## Add box around the walk if one is shown
  if (!is.null(walk)) {
    rngLon <- range(walk$Longitude)
    rngLat <- range(walk$Latitude)
    amap <- amap %>%
      addRectangles(lng1=rngLon[1],lat1=rngLat[1],
                    lng2=rngLon[2],lat2=rngLat[2],
                    fill=FALSE,
                    highlightOptions=highlightOptions(opacity=1))
  }
  ## Show the map
  amap
}

#' @title Visualize tracks that form a walk.
#' 
#' @description Create a map or plot elevations for tracks in a data frame that form a contiguous \dQuote{walk}.
#' 
#' @param dat A data frame that contains tracks to map, contiguous tracks for \code{walkMap} that are usually made with \code{\link{walkMaker}} and all tracks for \code{allTracksMap}.
#' @param clrs A named vector of colors that will be applied to identify tracks by \code{Type} in \code{dat}.
#' @param LAT_bottom A latitude coordinate for the bottom of the bounding box for the map. Defaults to just below the minimum latitude found in \code{trkdata}.
#' @param LAT_top A latitude coordinate for the top of the bounding box for the map. Defaults to just above the maximum latitude found in \code{trkdata}.
#' @param LON_left A longitude coordinate for the left-side of the bounding box for the map. Defaults to just left of the minimum latitude found in \code{trkdata}.
#' @param LON_right A longitude coordinate for the right-side of the bounding box for the map. Defaults to just right the maximum latitude found in \code{trkdata}.
#' @param map_bufr A numeric that makes the map slightly larger than the space that the track paths require.
#' @param label_tracks A logical for whether (or not) the tracks should be labeled with a unique number in \code{walkMap} or the track ID in \code{allTracksMap}.
#' @param walk A ggplot2 object made with \code{walkMap} that will be used to highlight the \dQuote{walk} on the map of all tracks.
#' @param verbose A logical for whether progress messages should be displayed or not.
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
walkMap <- function(dat,clrs,
                    LAT_bottom=NULL,LAT_top=NULL,
                    LON_left=NULL,LON_right=NULL,map_bufr=0.00004,
                    label_tracks=TRUE,verbose=TRUE) {
  ## Build base map
  amap <- iBaseMap(dat)
  amap <- iBoundBaseMap(amap,dat,LAT_bottom,LAT_top,LON_left,LON_right,map_bufr)
  
  if (verbose) cli::cli_alert_success("Base map built.")
  ## Get summaries of tracks in the walk (for use below)
  walksum <- iWalkSumPts(dat)
  ## Map the walk
  if (verbose) cli::cli_progress_bar("Adding tracks",total=length(walksum$trknum))
  for (i in walksum$trknum) {
    tmp <- dplyr::filter(dat,.data$trknum==i)
    tmp2 <- dplyr::filter(walksum,.data$trknum==i)
    tmp_d <- formatC(tmp2$Distance[1],format="f",digits=2)
    amap <- amap |>
      addPolylines(data=tmp,
                   lng=~Longitude,lat=~Latitude,
                   color=iRetClr(tmp,clrs),opacity=0.8,
                   highlightOptions=highlightOptions(color="blue"),
                   label=~htmltools::htmlEscape(paste(trackID[1],sep="<br/>")),
                   popup=paste0('<b>',tmp2$trackID,'</b><br/>',
                                tmp2$Primary,'<br/>',
                                iMakeDescription(tmp2$Primary,tmp2$From,tmp2$To),
                                '<br/>',
                                'Distance: ',tmp_d,' miles<br/>',
                                'Cumulative: ',
                                formatC(tmp2$end_Dist,format="f",digits=2),
                                ' miles<br/>'))
    if (verbose) cli::cli_progress_update()
  }
  if (verbose) cli::cli_progress_done()
  if (verbose) cli::cli_alert_success("All tracks added to map.")
  ## Add labels if asked for
  if (label_tracks) {
    for (i in walksum$trknum) {
      tmp <- dplyr::filter(walksum,.data$trknum==i)
      amap <- amap  |>
        addLabelOnlyMarkers(data=tmp,
                            lng=~start_Lon,lat=~start_Lat,
                            label=~as.character(trknum),
                            labelOptions=labelOptions(noHide=TRUE,
                                                      direction=tmp$lbldir))
    }
    if (verbose) cli::cli_alert_success("All track labels added.")
  }
  amap
}

#' @rdname walkMap
#' @export
allTracksMap <- function(dat,clrs,
                         LAT_bottom=NULL,LAT_top=NULL,
                         LON_left=NULL,LON_right=NULL,map_bufr=0.00004,
                         walk=NULL,verbose=TRUE) {
  ## Build base map
  amap <- iBaseMap(dat)
  ## Add bounds to base map depending on whether a walk is given or not
  if (is.null(walk))
    amap <- iBoundBaseMap(amap,dat,LAT_bottom,LAT_top,LON_left,LON_right,map_bufr)
  else 
    amap <- iBoundBaseMap(amap,walk,LAT_bottom,LAT_top,LON_left,LON_right,map_bufr)
  if (verbose) cli::cli_alert_success("Base map built.")
  ## Add all of the tracks
  if (verbose) cli::cli_progress_bar("Adding tracks",total=length(unique(dat$trackID)))
  for (i in unique(dat$trackID)) {
    tmp <- dplyr::filter(dat,.data$trackID==i)
    tmp_d <- formatC(tmp$Distance[nrow(tmp)],format="f",digits=2)
    tmp_e <- round(tmp$Elevation[nrow(tmp)]-tmp$Elevation[1],0)
    amap <- amap |>
      addPolylines(data=tmp,
                   lng=~Longitude,lat=~Latitude,
                   color=iRetClr(tmp,clrs),opacity=0.8,
                   highlightOptions=highlightOptions(color="blue"),
                   label=~htmltools::htmlEscape(paste(trackID[1],sep="<br/>")),
                   popup=~paste0('<b>',trackID[1],'</b> - ',Primary[1],'<br/>',
                                 ifelse(!is.na(From[1]),
                                        paste0("From: ",From[1],'<br/>'),""),
                                 ifelse(!is.na(To[1]),
                                        paste0("To: ",To[1],'<br/>'),""),
                                 "Distance: ",tmp_d," miles<br/>",
                                 "Elevation Change: ",tmp_e," feet")
      )
    if (verbose) cli::cli_progress_update()
  }
  if (verbose) cli::cli_progress_done()
  if (verbose) cli::cli_alert_success("All tracks added to map.")
  ## Add box around the walk if one is shown
  if (!is.null(walk)) {
    rngLon <- range(walk$Longitude)
    rngLat <- range(walk$Latitude)
    amap <- amap |>
      addRectangles(lng1=rngLon[1],lat1=rngLat[1],
                    lng2=rngLon[2],lat2=rngLat[2],
                    fill=FALSE,
                    highlightOptions=highlightOptions(opacity=1))
    if (verbose) cli::cli_alert_success("Box added around the walk.")
  }
  ## Show the map
  amap
}

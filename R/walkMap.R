#' @title Visualize tracks that form a walk.
#' 
#' @description Create a map or plot elevations for tracks in a data frame that form a contiguous \dQuote{walk}.
#' 
#' @param dat A data frame that contains tracks to map, contiguous tracks for \code{walkMap} that are usually made with \code{\link{walkMaker}} and all tracks for \code{allTracksMap}.
#' @param title A string for the map title.
#' @param label_tracks A logical for whether (or not) the tracks should be labeled with a unique number.
#' @param OMap_type A string for the type of OpenStreet map to use under the track paths. Use \code{"none"} to not using an OpenStreet map.
#' @param OMap_bufr A numeric that makes the OpenStreet map slightly larger than the space that the track paths require.
#' @param LAT_bottom A latitude coordinate for the bottom of the bounding box for the map. Defaults to just below the minimum latitude found in \code{trkdata}.
#' @param LAT_top A latitude coordinate for the top of the bounding box for the map. Defaults to just above the maximum latitude found in \code{trkdata}.
#' @param LON_left A longitude coordinate for the left-side of the bounding box for the map. Defaults to just left of the minimum latitude found in \code{trkdata}.
#' @param LON_right A longitude coordinate for the right-side of the bounding box for the map. Defaults to just right the maximum latitude found in \code{trkdata}.
#' 
#' @details NONE YET
#' 
#' @return Returns a \code{ggplot2} object that is the map. The map will be displayed if the returned object is not assigned to name.
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
walkMap <- function(dat,title=NULL,label_tracks=TRUE,
                    OMap_type=c("esri-topo","esri","apple-iphoto","bing",
                                "stamen-terrain","none"),
                    OMap_bufr=0.00002,
                    LAT_bottom=NULL,LAT_top=NULL,
                    LON_left=NULL,LON_right=NULL) {
  ## Handle default arguments
  OMap_type <- match.arg(OMap_type)
  ## Possibly make underlyling OpenStreet map
  if (OMap_type!="none")
    omap <- iOMAP(dat,OMap_type,OMap_bufr,LAT_bottom,LAT_top,LON_left,LON_right)
  else 
    omap <- ggplot() +
      coord_sf(ylim=c(LAT_bottom,LAT_top),
               xlim=c(LON_left,LON_right))
  ## Get starting coordinates (for putting points on the plot)
  walksum <- iWalkSumPts(dat)
  ## Map the walk
  mapz <- omap +
    geom_path(data=dat,
              mapping=aes(x=.data$Longitude,y=.data$Latitude,
                          group=.data$trackID,color=.data$trknum),
              size=1.5) +
    geom_point(data=walksum,
               mapping=aes(x=.data$start_Lon,y=.data$start_Lat),
               pch=18,color="white")
  ## Add labels if asked for
  if (label_tracks) {
    mapz <- mapz +
      ggrepel::geom_label_repel(data=walksum,
                                mapping=aes(x=.data$midpt_Lon,
                                            y=.data$midpt_Lat,
                                            label=.data$trknum,
                                            color=.data$trknum),
                                label.padding=grid::unit(0.1,"lines"))
  }
  ## Add title if one is given
  if (!is.null(title)) mapz <- mapz + labs(title=title)
  ## Clean-up
  mapz <- mapz +
    theme_minimal() +
    theme(legend.position="none",axis.title=element_blank())
  ## Show the map
  mapz
}

#' @rdname walkMap
#' @export
allTracksMap <- function(dat,title=NULL,label_tracks=TRUE,
                         OMap_type=c("esri-topo","esri","apple-iphoto","bing",
                                     "stamen-terrain","none"),
                         OMap_bufr=0.00004,
                         LAT_bottom=NULL,LAT_top=NULL,
                         LON_left=NULL,LON_right=NULL) {
  ## Handle default arguments
  OMap_type <- match.arg(OMap_type)
  ## Possibly make underlyling OpenStreet map
  if (OMap_type!="none")
    omap <- iOMAP(dat,OMap_type,OMap_bufr,LAT_bottom,LAT_top,LON_left,LON_right)
  else omap <- ggplot() +
      coord_sf(ylim=c(LAT_bottom,LAT_top),
               xlim=c(LON_left,LON_right))
  ## Getting starting points of each segment
  walksum <- iWalkSumPts(dat)
  ## Add the track
  map <- omap +
    geom_path(data=dat,
              mapping=aes(x=.data$Longitude,y=.data$Latitude,
                          group=.data$trackID,color=.data$Type,
                          linetype=.data$Ownership,size=.data$Type)) +
    geom_point(data=walksum,
               mapping=aes(x=.data$start_Lon,y=.data$start_Lat,
                           color=.data$Type),
               pch=23,fill="white") +
    scale_color_manual(values=clrs) +
    scale_linetype_manual(values=ltyps) +
    scale_size_manual(values=szs) +
    theme_minimal() +
    theme(legend.position="none",axis.title=element_blank())
  ## Add title if one is given
  if (!is.null(title)) map <- map + labs(title=title)
  ## Include labels if asked to
  if (label_tracks) 
    map <- map +
    ggrepel::geom_text_repel(data=walksum,
                             mapping=aes(x=.data$start_Lon,y=.data$start_Lat,
                                         label=.data$trackID),
                             size=2)
  ## Show the map
  map
}


## INTERNAL function to make the possible underlying OpenStreets map
iOMAP <- function(dat,OMap_type,OMap_bufr,
                  LAT_bottom,LAT_top,LON_left,LON_right) {
  ### Set the bounding box for underlying map
  if(is.null(LAT_bottom)) LAT_bottom <- min(dat$Latitude)*(1-OMap_bufr)
  if(is.null(LAT_top)) LAT_top <- max(dat$Latitude)*(1+OMap_bufr)
  if(is.null(LON_left)) LON_left <- min(dat$Longitude)*(1+OMap_bufr)
  if(is.null(LON_right)) LON_right <- max(dat$Longitude)*(1-OMap_bufr)
  ### Get the underlying map and use Lat-Long rather than mercator projections
  omap <- OpenStreetMap::openmap(c(LAT_top,LON_left),c(LAT_bottom,LON_right),
                                 zoom=NULL,type=OMap_type,mergeTiles=TRUE) %>%
    OpenStreetMap::openproj(projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  OpenStreetMap::autoplot.OpenStreetMap(omap)
}
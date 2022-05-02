#' @title Map all tracks
#' 
#' @description Map all tracks in a data frame.
#' 
#' @param trkdata A data frame that contains coordinates and information about each track.
#' @param title A string for the map title.
#' @param inclLabels A logical for whether track labels should be shown or not.
#' @param OMap_type A string for the type of OpenStreet map to use under the track paths. Use \code{"none"} to not using an OpenStreet map.
#' @param OMap_bufr A numeric that makes the OpenStreet map slighly larger than the space that the track paths require.
#' @param LAT_bottom A latitude coordinate for the bottom of the bounding box for the map. Defaults to just below the minimum latitude found in \code{trkdata}.
#' @param LAT_top A latitude coordinate for the top of the bounding box for the map. Defaults to just above the maximum latitude found in \code{trkdata}.
#' @param LON_left A longitude coordinate for the left-side of the bounding box for the map. Defaults to just left of the minimum latitude found in \code{trkdata}.
#' @param LON_right A longitude coordinate for the right-side of the bounding box for the map. Defaults to just right the maximum latitude found in \code{trkdata}.
#' 
#' @details NONE YET
#' 
#' @return Returns a \code{ggplot2} object that is the map. The map will be displayed if the returned object is not assigned to name.
#' 
#' @author Derek H. Ogle
#' @keywords manip
#' 
#' @examples
#' ## None yet.
#' 
#' @export
mapAllTracks <- function(trkdata,title=NULL,inclLabels=TRUE,
                         OMap_type=c("esri-topo","esri","apple-iphoto","bing",
                                     "stamen-terrain","none"),
                         OMap_bufr=0.00004,
                         LAT_bottom=NULL,LAT_top=NULL,
                         LON_left=NULL,LON_right=NULL) {
  ## Handle default arguments
  OMap_type <- match.arg(OMap_type)
  ## Make possible underlying map
  if (OMap_type!="none") {
    ### Set the bounding box for underlying map
    if(is.null(LAT_bottom)) LAT_bottom <- min(trkdata$Latitude)*(1-OMap_bufr)
    if(is.null(LAT_top)) LAT_top <- max(trkdata$Latitude)*(1+OMap_bufr)
    if(is.null(LON_left)) LON_left <- min(trkdata$Longitude)*(1+OMap_bufr)
    if(is.null(LON_right)) LON_right <- max(trkdata$Longitude)*(1-OMap_bufr)
    zm <- NULL
    ### Get the underlying map and use Lat-Long rather than mercator projections
    omap <- OpenStreetMap::openmap(c(LAT_top,LON_left),c(LAT_bottom,LON_right),
                                   zoom=zm,type=OMap_type,mergeTiles=TRUE) %>%
      OpenStreetMap::openproj(projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    omap <- OpenStreetMap::autoplot.OpenStreetMap(omap)
  } else omap <- ggplot()
  ## Getting starting points of each segment
  walksum <- iWalkSumPts(trkdata)
  ## Add the segments
  map <- omap +
    geom_path(data=trkdata,
              mapping=aes(x=.data$Longitude,y=.data$Latitude,
                          group=.data$ID,color=.data$Type,
                          linetype=.data$Ownership,size=.data$Type)) +
    geom_point(data=walksum,
               mapping=aes(x=.data$start_Lon,y=.data$start_Lat,
                           color=.data$Type),
               pch=23,fill="white") +
    scale_color_manual(values=clrs) +
    scale_linetype_manual(values=ltyps) +
    scale_size_manual(values=szs) +
    coord_sf(ylim=c(LAT_bottom,LAT_top),
             xlim=c(LON_left,LON_right)) +
    theme_minimal() +
    theme(legend.position="none",axis.title=element_blank())
  ## Include labels if asked to
  if (inclLabels) 
    map <- map +
    ggrepel::geom_text_repel(data=walksum,
                             mapping=aes(x=.data$start_Lon,y=.data$start_Lat,
                                         label=.data$ID),
                             size=2)
  ## Show the map
  map
}

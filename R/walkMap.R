#' @title Visualize tracks that form a walk.
#' 
#' @description Create a map or plot elevations for tracks in a data frame that form a contiguous \dQuote{walk}.
#' 
#' @param walkdata A data frame that contains tracks that form a contiguous \dQuote{walk}. Usually made with \code{\link{walkMaker}}.
#' @param title A string for the map title.
#' @param label_tracks A logical for whether (or not) the tracks should be labeled with a unique number.
#' @param OMap_type A string for the type of OpenStreet map to use under the track paths. Use \code{"none"} to not using an OpenStreet map.
#' @param OMap_bufr A numeric that makes the OpenStreet map slightly larger than the space that the track paths require.
#' @param elev_bufr A numeric that makes a slight buffer around the elevation data.
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
#' @export 
walkMap <- function(walkdata,title=NULL,label_tracks=TRUE,
                    OMap_type=c("esri-topo","esri","apple-iphoto","bing",
                                "stamen-terrain","none"),
                    OMap_bufr=0.00002) {
  ## Handle default arguments
  OMap_type <- match.arg(OMap_type)

  ## Get starting coordinates (for putting points on the plot)
  walksum <- iWalkSumPts(walkdata)
  ## Make a title and subtitle (as appropriate)
  walklbl <- paste0(walkdata$ID[1]," to ",
                    walkdata$ID[nrow(walkdata)],": ",
                    formatC(max(walkdata$Distance),format="f",digits=2)," miles")
  if (is.null(title)) {
    title <- walklbl
    sttl <- NULL
  } else sttl <- walklbl
  ## Make possible underlying map
  if (OMap_type!="none") {
    ### Set the bounding box for underlying map
    LAT_bottom <- min(walkdata$Latitude)*(1-OMap_bufr)
    LAT_top <- max(walkdata$Latitude)*(1+OMap_bufr)
    LON_left <- min(walkdata$Longitude)*(1+OMap_bufr)
    LON_right <- max(walkdata$Longitude)*(1-OMap_bufr)
    zm <- NULL
    ### Get the underlying map and use Lat-Long rather than mercator projections
    omap <- OpenStreetMap::openmap(c(LAT_top,LON_left),c(LAT_bottom,LON_right),
                                   zoom=zm,type=OMap_type,mergeTiles=TRUE) %>%
      OpenStreetMap::openproj(projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    omap <- OpenStreetMap::autoplot.OpenStreetMap(omap)
  } else omap <- ggplot()
  ## Map the walk
  mapz <- omap +
    geom_path(data=walkdata,
              mapping=aes(x=.data$Longitude,y=.data$Latitude,
                          group=.data$ID,color=.data$Distance),
              size=1.5) +
    geom_point(data=walksum,
               mapping=aes(x=.data$start_Lon,y=.data$start_Lat),
               pch=18,color="white") +
    coord_sf()
  ## Add labels if asked for
  if (label_tracks) {
    mapz <- mapz +
      ggrepel::geom_label_repel(data=walksum,
                                mapping=aes(x=.data$midpt_Lon,y=.data$midpt_Lat,
                                            label=.data$trknum),
                                color="darkblue",label.padding=grid::unit(0.1,"lines"))
  }
  ## Clean-up
  mapz <- mapz +
    labs(title=title,subtitle=sttl) +
    theme_minimal() +
    theme(legend.position="none",axis.title=element_blank())
  ## Show the map
  mapz
}

#' @rdname walkMap
#' @export
walkElevation <- function(walkdata,title=NULL,elev_bufr=0.01) {
  ## Get starting distances and elevations (for putting points on the plot)
  walksum <- iWalkSumPts(walkdata)
  ## Find min and max elevations to set buffer around the plot
  min_elev <- min(walkdata$Elevation)*(1-elev_bufr)
  max_elev <- max(walkdata$Elevation)*(1+elev_bufr)
  ## Make the plot
  elev <- ggplot() +
    geom_ribbon(data=walkdata,
                mapping=aes(x=.data$Distance,ymin=min_elev,ymax=.data$Elevation),
                fill="gray80") +
    geom_line(data=walkdata,
              mapping=aes(x=.data$Distance,y=.data$Elevation)) +
    geom_point(data=walksum,
               mapping=aes(x=.data$start_Dist,y=.data$start_Elev),
               pch=23,color="red",fill="white") +
    scale_y_continuous(name="Elevation (m)",limits=c(min_elev,max_elev),
                       expand=expansion(mult=0)) +
    theme_minimal()
  elev
}
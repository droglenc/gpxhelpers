#' @title Visualize elevations for tracks that form a walk.
#' 
#' @description Plot elevations for tracks in a data frame that form a contiguous \dQuote{walk}.
#' 
#' @param walkdata A data frame that contains tracks that form a contiguous \dQuote{walk}. Usually made with \code{\link{walkMaker}}.
#' @param title A string for the map title.
#' @param elev_bufr A numeric that makes a slight buffer around the elevation data.
#' @param maval The number of points to include in the moving average. Use 1 to maintain raw data. Defaults to 10.
#' 
#' @details NONE YET
#' 
#' @return Returns a \code{ggplot2} object. The object will be displayed if the returned object is not assigned to name.
#' 
#' @seealso \code{\link{walkMap}} \code{\link{walkSummary}}
#' 
#' @author Derek H. Ogle
#' @keywords manip
#' 
#' @examples
#' ## None yet.
#' 
#' @export
walkElevation <- function(walkdata,title=NULL,elev_bufr=0.01,maval=10) {
  ## Add a moving average
  walkdata$maElevation <- stats::filter(walkdata$Elevation,rep(1/maval,maval))
  ## Get starting distances and elevations (for putting points on the plot)
  walksum <- iWalkSumPts(walkdata)
  ## Find min and max elevations to set buffer around the plot
  min_elev <- min(walkdata$Elevation)*(1-elev_bufr)
  max_elev <- max(walkdata$Elevation)*(1+elev_bufr)
  ## Make the plot?
  elev <- ggplot() +
    geom_ribbon(data=walkdata,
                mapping=aes(x=.data$Distance,ymin=min_elev,ymax=.data$maElevation),
                fill="gray80",na.rm=TRUE) +
    geom_line(data=walkdata,
              mapping=aes(x=.data$Distance,y=.data$maElevation),na.rm=TRUE) +
    geom_label(data=walksum,
               mapping=aes(x=.data$start_Dist,y=.data$start_Elev,
                           label=.data$trknum),
               nudge_y=5) +
    scale_x_continuous(name="Distance (miles)",expand=expansion(mult=0.01)) +
    scale_y_continuous(name="Elevation (ft)",limits=c(min_elev,max_elev),
                       expand=expansion(mult=0)) +
    theme_minimal()
  elev
}
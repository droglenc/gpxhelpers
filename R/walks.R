#' @title Extract track IDs and photos for a walk.
#' 
#' @description Extract track IDs and photos for a \dQuote{walk}.
#' 
#' @param d A data frame from either the \dQuote{Walks} or \dQuote{Photos} tab in the trail mapping info sheet.
#' @param whichWalk A numeric that identifies the code for the \dQuote{walk}.
#' @param walkIDs A vector of track name strings that represent a contiguous \dQuote{walk}. This may be returned from \code{\link{walkGetTrackIDs}}.
#' @param path A directory from the working directory that contains the photo image files.
#' 
#' @details NONE YET
#' 
#' @return \code{walkGetTrackIDs} returns a vector of track IDs for the walk and \code{walkGetPhotos} returns a data frame that contains the filename (with the \code{path} prepended) and caption for photos from the walk.
#' 
#' @seealso \code{\link{walkGetTrackIDs}}, \code{\link{walkMap}}, and \code{\link{walkSummary}}
#' 
#' @author Derek H. Ogle
#' @keywords manip
#' 
#' @examples
#' ## None yet.
#' 

#' @export
walkGetTrackIDs <- function(d,whichWalk) {
  unlist(strsplit(d$TrackIDs[d$Walk==whichWalk],split=", "))
}

#' @rdname walkGetTrackIDs
#' @export
walkGetPhotos <- function(d,walkIDs,path="Images") {
  d |>
    dplyr::filter(.data$trackID %in% walkIDs) |>
    dplyr::mutate(Photo=file.path(path,paste0(.data$Photo,".jpg")),
                  Caption=paste0(.data$trackID,": ",.data$Caption))
}



#' @title Concatenate tracks into a walk.
#' 
#' @description Concatenate a vector of track names that represent a contiguous \dQuote{walk} into a data frame.
#' 
#' @param trkdata A data frame that contains coordinates and information about each track.
#' @param trkinfo A data frame that contains information about each track.
#' @param walkIDs A vector of track name strings that can be appended into a contiguous \dQuote{walk}. Generally first two names should be the order of the first two tracks in the \dQuote{walk}.
#' @param startIDs A length two vector of track name strings that indicate the order of the first two tracks in the \dQuote{walk}. Defaults to the first two items in \code{walkIDs}.
#' @param findOrder A logical that indicates that the track names in \code{walkIDs} are NOT in walking order and an attempt should be made to place the tracks in a walking order based on the \code{From}  and \code{To} fields in \code{trkinfo}. See notes below.
#' @param basedate A string with a date that will serve as the base date for the dummy times in the returned data frame. Defaults to "2022-01-01".
#' @param verbose A logical for whether the progress of connecting tracks should be displayed or not.
#' 
#' @details NONE YET
#' 
#' @note If any of the track names in \code{walkIDs} are repeated because the \dQuote{walk} will include repeated walkings of some tracks then the user must put the track names in the order to be walked in \code{walkIDs} and \code{findOrder} must be \code{FALSE}.
#' 
#' @seealso \code{\link{walkMap}}
#' 
#' @return A date frame with the same variables as \code{trkdata}, but only with the tracks in \code{walkIDs} and in order of the \dQuote{walk}.
#' 
#' @author Derek H. Ogle
#' @keywords manip
#' 
#' @examples
#' ## None yet.

#' @export
walkMaker <- function(trkdata,trkinfo,walkIDs,startIDs=walkIDs[1:2],
                      findOrder=FALSE,basedate="2022-01-01",verbose=FALSE) {
  if (length(walkIDs)==1) {
    ## If only one track ID then just return that
    walkdat <- dplyr::filter(trkdata,.data$trackID==walkIDs[1])
  } else {
    ## Arrange walkIDs (from startIDs)
    if (findOrder) walkIDs <- iOrderWalk(walkIDs,trkinfo,startIDs)
    if (verbose) cat("Segment order:",paste(walkIDs,collapse=" -> "),"\n\n")
    
    ## Initiate walk data frame by connecting first two tracks
    if (verbose) cat("Connecting:",startIDs[1],"to",startIDs[2],"\n")
    seg1 <- dplyr::filter(trkdata,.data$trackID==startIDs[1]) |>
      dplyr::mutate(trknum=1)
    seg1_begpt <- seg1[1,c("Longitude","Latitude")]
    seg1_endpt <- seg1[nrow(seg1),c("Longitude","Latitude")]
    seg2 <- dplyr::filter(trkdata,.data$trackID==startIDs[2]) |>
      dplyr::mutate(trknum=2)
    seg2_begpt <- seg2[1,c("Longitude","Latitude")]
    seg2_endpt <- seg2[nrow(seg2),c("Longitude","Latitude")]
    dists <- c("beg2beg"=geosphere::distGeo(seg1_begpt,seg2_begpt),
               "beg2end"=geosphere::distGeo(seg1_begpt,seg2_endpt),
               "end2beg"=geosphere::distGeo(seg1_endpt,seg2_begpt),
               "end2end"=geosphere::distGeo(seg1_endpt,seg2_endpt))
    min_dist_pts <- names(dists)[which.min(dists)]
    if (min_dist_pts %in% c("beg2end","beg2beg")) {
      seg1 <- seg1[nrow(seg1):1,]
      seg1 <- iSwapFromTo(seg1)
    }
    if (min_dist_pts %in% c("beg2end","end2end")) {
      seg2 <- seg2[nrow(seg2):1,]
      seg2 <- iSwapFromTo(seg2)
    }
    walkdat <- rbind(seg1,seg2)
    
    ## Loop through other tracks and append
    if (length(walkIDs)>2) {
      for (i in 3:length(walkIDs)) {
        if (verbose) cat("Connecting:",walkIDs[i-1],"to",walkIDs[i],"\n")
        nextseg <- dplyr::filter(trkdata,.data$trackID==walkIDs[i]) |>
          dplyr::mutate(trknum=i)
        prevseg_endpt <- walkdat[nrow(walkdat),c("Longitude","Latitude")]
        nextseg_begpt <- nextseg[1,c("Longitude","Latitude")]
        nextseg_endpt <- nextseg[nrow(nextseg),c("Longitude","Latitude")]
        d_end2beg <- geosphere::distGeo(prevseg_endpt,nextseg_begpt)
        d_end2end <- geosphere::distGeo(prevseg_endpt,nextseg_endpt)
        if (d_end2end<d_end2beg) {
          nextseg <- nextseg[nrow(nextseg):1,]
          nextseg <- iSwapFromTo(nextseg)
        }
        walkdat <- rbind(walkdat,nextseg)
      }
    }
    ## Recalculate distance and time
    walkdat$Distance <- distAlongTrack(walkdat)
    walkdat$Time <- lubridate::ymd_hms(paste(basedate,"00:00:00 CDT")) +
      1:nrow(walkdat)    
  }
  ## Move "trknum" variable to first column
  walkdat <- dplyr::relocate(walkdat,.data$trknum)
  ## Return the walk data.frame
  walkdat
}



#' @title Summarize tracks that form a walk.
#' 
#' @description Create a table that summarizes tracks in a data frame that form a contiguous \dQuote{walk}.
#' 
#' @param walkdat A data frame that contains tracks that form a contiguous \dQuote{walk}. Usually made with \code{\link{walkMaker}}.
#' 
#' @details NONE YET
#' 
#' @return Returns a \code{kable} table object.
#' 
#' @seealso \code{\link{walkMap}}
#' 
#' @author Derek H. Ogle
#' @keywords manip
#' 
#' @examples
#' ## None yet.
#' 
#' @export 
walkSummary <- function(walkdat,dropType=FALSE,dropOwner=FALSE) {
  walksum <- iWalkSumPts(walkdat) |>
    ## need this group_by so that iMakeDescription works for each track
    dplyr::group_by(.data$trknum) |>
    dplyr::mutate(Description=iMakeDescription(.data$Primary,.data$From,.data$To),
                  Description=ifelse(.data$Primary==.data$Description,
                                     "--",.data$Description)) |>
    dplyr::rename(NUM=.data$trknum,Owner=.data$Ownership,
                  CumDist=.data$end_Dist) |>
    dplyr::select(.data$NUM,.data$trackID,.data$Primary,
                  .data$Description,.data$Type,.data$Owner,
                  .data$Distance,.data$CumDist,.data$DeltaElev)
  
  digs <- c(0,NA,NA,NA,NA,NA,2,2,0)

  if (dropType) {
    walksum <- dplyr::select(walksum,-.data$Type)
    digs <- digs[-2]
  }
  
  if (dropOwner) {
    walksum <- dplyr::select(walksum,-.data$Owner)
    digs <- digs[-2]
  }
  
  knitr::kable(walksum,digits=digs) |>
    kableExtra::kable_classic(html_font="Cambria",full_width=FALSE) |>
    kableExtra::kable_styling(bootstrap_options=c("hover","condensed"))
}



#' @title Visualize elevations for tracks that form a walk.
#' 
#' @description Plot elevations for tracks in a data frame that form a contiguous \dQuote{walk}.
#' 
#' @param walkdat A data frame that contains tracks that form a contiguous \dQuote{walk}. Usually made with \code{\link{walkMaker}}.
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
walkElevation <- function(walkdat,title=NULL,elev_bufr=0.01,maval=10) {
  ## Add a moving average
  walkdat$maElevation <- stats::filter(walkdat$Elevation,rep(1/maval,maval))
  ## Get starting distances and elevations (for putting points on the plot)
  walksum <- iWalkSumPts(walkdat)
  ## Find min and max elevations to set buffer around the plot
  min_elev <- min(walkdat$Elevation)*(1-elev_bufr)
  max_elev <- max(walkdat$Elevation)*(1+elev_bufr)
  ## Make the plot?
  elev <- ggplot() +
    geom_ribbon(data=walkdat,
                mapping=aes(x=.data$Distance,ymin=min_elev,ymax=.data$maElevation),
                fill="gray80",na.rm=TRUE) +
    geom_line(data=walkdat,
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


#' @title Write walk report HTML files.
#' 
#' @description Write \dQuote{walk} report HTML files.
#' 
#' @param walk A vector of walk codes.
#' @param project A project name.
#' @param datfile The name of the file in \code{pth} that contains all of the track information.
#' @param basedir A path string to where \code{datfile}, the folder with the images, and the folder in which to put the resultant HTML file reside.
#' @param tmplt A name for the template to use.
#' @param showFileInBrowser A logical for whether to open the resultant file in the broswer or not (default is to not).
#' @param quiet A logical for whether the progress of processing the markdown file should be shown (default is to not).
#' 
#' @details NONE YET
#' 
#' @return None, but an html file will be created.
#' 
#' @author Derek H. Ogle
#' @keywords manip
#' 
#' @examples
#' \dontrun{
#' project <- "Bayfield County"
#' basedir <- file.path("C:/aaaPersonal/MAPPING",project)
#' datfile <- paste0(project,".csv")
#' walk <- "FR4138051"
#' walkReports(walk,project,datfile,basedir,showFileInBrowser=TRUE)
#' }
#' 
#' @export 
walkReports <- function(walk,project,datfile,basedir,
                        tmplt="Walk_Template.Rmd",
                        showFileInBrowser=FALSE,quiet=TRUE) {
  tmplt <- file.path(system.file("templates",package="gpxhelpers"),tmplt)
  for (i in seq_along(walk)) {
    ofn <- paste0(walk[i],".html")
    cat("Processing '",ofn,"' to '",basedir,"' ...",sep="")
    rmarkdown::render(input=tmplt,
                      params=list(basedir=basedir,
                                  project=project,
                                  datfile=datfile,
                                  walk=walk[i]),
                      output_dir="Walks",
                      output_file=ofn,
                      quiet=quiet,
                      envir=new.env())
    cat("Done\n")
    if (showFileInBrowser)
      utils::browseURL(paste0('file://',
                              file.path(basedir,"Walks",ofn)))
  }
}

#' @title Concatenate tracks into a walk.
#' 
#' @description Concatenate a vector of track names that represent a contiguous \dQuote{walk} into a data frame.
#' 
#' @param trkdata A data frame that contains coordinates and information about each track.
#' @param trkinfo A data frame that contains information about each track.
#' @param walkIDs A vector of track name strings that can be appended into a contiguous \dQuote{walk}. Generally first two names should be the order of the first two tracks in the \dQuote{walk}.
#' @param startIDs A length two vector of track name strings that indicate the order of the first two tracks in the \dQuote{walk}. Defaults to the first two items in \code{walkIDs}.
#' @param findOrder A logical that indicates that the track names in \code{walkIDs} are NOT in walking order and an attempt should be made to place the tracks in a walking order based on the \code{connected} field in \code{trkinfo}. See notes below.
#' @param basedate A string with a date that will serve as the base date for the dummy times in the returned data frame. Defaults to "2022-01-01".
#' @param verbose A logical for whether the progress of connecting tracks should be displayed or not.
#' 
#' @details NONE YET
#' 
#' @note If any of the track names in \code{walkIDs} are repeated because the \dQuote{walk} will include repeated walkings of some segments then the user must put the track names in the order to be walked in \code{walkIDs} and \code{findOrder} must be \code{FALSE}.
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
  ## Arrange walkIDs (from startIDs)
  if (findOrder) walkIDs <- iOrderWalk(walkIDs,trkinfo,startIDs)
  if (verbose) cat("Segment order:",paste(walkIDs,collapse=" -> "),"\n\n")
  
  ## Initiate walk data frame by connecting first two tracks
  if (verbose) cat("Connecting:",startIDs[1],"to",startIDs[2],"\n")
  seg1 <- dplyr::filter(trkdata,.data$trackID==startIDs[1]) %>%
    dplyr::mutate(trknum=1)
  seg1_begpt <- seg1[1,c("Longitude","Latitude")]
  seg1_endpt <- seg1[nrow(seg1),c("Longitude","Latitude")]
  seg2 <- dplyr::filter(trkdata,.data$trackID==startIDs[2]) %>%
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
      nextseg <- dplyr::filter(trkdata,.data$trackID==walkIDs[i]) %>%
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
  ## Move "trknum" variable to first column
  walkdat <- dplyr::relocate(walkdat,.data$trknum)
  ## Return the walk data.frame
  walkdat
}


## INTERNAL function to arrange tracks to be chained together as a walk
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


## INTERNAL function to swap the From and To variables in d data.frame
iSwapFromTo <- function(d) {
  tmp <- d$From
  d$From <- d$To
  d$To <- tmp
  d
}
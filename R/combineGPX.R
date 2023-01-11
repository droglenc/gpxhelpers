#' @title Combine tracks into a single GPX file.
#' 
#' @description Combine given GPX track files into a single GPX file. If the single GPX file exists then tracks can be appended.
#' 
#' @param pin Path after the working directory that contains the original GPX files.
#' @param pout Path after the working directory to put the single resultant GPX file.
#' @param fnm A filename sans extension for the resultant file (a ".gpx" will be added as appropriate).
#' @param IDs a character vector of track IDs that should be combined into a single GPX file. If \code{NULL} then all GPX files in \code{pin} will be combined or if \code{fnm} exists in \code{pout} then all GPX files with modification dates after the modification date for \code{fnm} will be appended to \code{fnm}.
#' 
#' @details NONE YET
#' 
#' @return None, used for side effect of writing a file to the \code{pout} directory.
#' 
#' @author Derek H. Ogle
#' @keywords manip
#' 
#' @examples
#' ## None yet.
#' 
#' @export
combineTracks2GPX <- function(pin,pout,fnm,IDs=NULL) {
  # Determine if fnm has an extension, if not then add ".gpx"
  if(tools::file_ext(fnm)=="") fnm <- paste0(fnm,".gpx")
  
  # See if the output file exists or not in pout ...
  tmp <- list.files(pattern="gpx",path=pout)
  fnm_existed <- fnm %in% tmp 
  # Make the full name of output file
  fnm <- file.path(pout,fnm)
  if (fnm_existed) {
     ## if the output file existed then ... read the file ...
    res <- readLines(fnm)
    ## ... remove the last line to start the new output file
    res <- res[-length(res)]
    ## ... get the last modified date
    fnm_modtime <- file.info(fnm)$mtime
    ## Send message
    cli::cli_alert_info("The output file '{file.path(getwd(),fnm)}' already exists! Track files modified after {as.character(fnm_modtime)} will be appended to it.")
  }
  
  # Make list of IDs (i.e., gpx track files) to
  ## if IDs is not NULL then user gave specific IDs, use those
  if (!is.null(IDs)) paste0(IDs,".gpx")
  else {
  ## User did not supply IDs, so use all IDs in pin if fnm did not exist
  ##   or will append only those IDs in pin not already in fnm
    ## Get all IDs in pin
    IDs <- list.files(pattern="gpx",path=pin)
    ## If fnm existed then reduce to only those IDs modified since fnm was modified
    if (fnm_existed) {
      tmp <- file.info(file.path(pin,IDs))
      IDs <- IDs[which(tmp$mtime>fnm_modtime)]
      if (length(IDs)==0) {
        finish <- FALSE
        cli::cli_alert_warning("No tracks have been modified since {fnm} was last modified. There is nothing to add to the existing output file.")
      } else finish <- TRUE
    }
  }
  
  # Handle modified IDs that may already exist in fnm (as might happen if
  #   the IDs gpx was modified at a later date)
  if (fnm_existed & finish) {
    ## combine all IDs into a string that can be used in grep()
    tmp <- paste(tools::file_path_sans_ext(IDs),collapse="|")
    ## then grep to see if any of those IDs are in res (the existing output file)
    tmp <- grep(tmp,res)
    ## if any IDs existed in the output file then they must be removed
    if (length(tmp)!=0) {
      ## Send message
      tmp2 <- res[tmp]
      tmp2 <- substr(tmp2,9,100)         # remove <name> at beginning
      tmp2 <- substr(tmp2,nchar(tmp2)-7) # remove </name> at end
      cli::cli_alert_info("The following tracks existed in the output file but have been modified since that file was created. Thus, they will be removed and the newer track data will be appended to the output file.")
      cat("\n")
      cat(paste(tmp2,collapse=" "))
      cat("\n\n")
      ## grep above returned position of <name> tag, <trk> tag is right above it
      ##   this is now the position of <trk> for the IDs to remove
      tmp <- tmp-1
      ## get position of all <trk> in the output file
      trk_starts <- grep("<trk>",res)
      ## find which trk_starts correspond to IDs to remove ...
      trk_starts_remove <- which(trk_starts %in% tmp)
      ## ... convert to actual positions in res
      ##       the +1 will be used to find the </trk> to stop for the last IDs
      trk_starts_remove <- trk_starts[unique(c(trk_starts_remove,trk_starts_remove+1))]
      ## remove the <trk> to </trk> for each IDs to remove
      for (i in 1:(length(trk_starts_remove)-1))
        res <- res[-(trk_starts_remove[i]:(trk_starts_remove[i+1]-1))]
    }
  }
  
  # Cycle through IDs (gpx files) appending them to res
  if (finish) {
    for (i in seq_along(IDs)) {
      cli::cli_progress_bar("Adding GPX files",total=length(IDs))
      ## Read gpx file
      tmp <- readLines(file.path(pin,IDs[i]))
      ## If fnm had not existed then start res with first gpx (sans last line)
      if (i==1 & !fnm_existed) res <- tmp[-length(tmp)]
      else {
        ## Otherwise get from <trk> to </trk> and append to res
        trk_start <- which(grepl("<trk>",tmp))
        trk_end <- which(grepl("</trk>",tmp))
        res <- c(res,tmp[trk_start:trk_end])
      }
      cli::cli_progress_update()
    }
    ## Close out the <gpx> tag as the last line
    res <- c(res,"</gpx>")
    ## Write out the new file
    writeLines(res,fnm)
    ## Send completion message
    tmp <- ifelse(fnm_existed,"appended to","combined into")
    cli::cli_alert_success("{length(IDs) tracks {tmp} '{file.path(getwd(),fnm)}'")
  }
}

  
  
#' @title Write track information into a CSV file.
#' 
#' @description Combine track information with track GPX data into a single CSV file.
#' 
#' @param trkinfo A data frame that contains information about each track.
#' @param fnm A filename (with \dQuote{.gpx} extension) that contains the tracks to output to a CSV file with same base name.
#' 
#' @details NONE YET
#' 
#' @return Will invisibly return the data frame that is also written to the CSV file in the same directory.
#' 
#' @author Derek H. Ogle
#' @keywords manip
#' 
#' @examples
#' ## None yet.
#' 
#' @export
writeGPXnInfo2CSV <- function(trkinfo,fnm) {
  ## Create CSV output file name with same base name as GPX file
  fnmout <- paste0(tools::file_path_sans_ext(fnm),".csv")
  ## Read the GPX data in fnm
  cat("Reading ",fnm,", please be patient .... ",sep="")
  resgpx <- dplyr::bind_rows(gpx::read_gpx(fnm)$tracks,.id="trackID") %>%
    dplyr::rename(trknum=.data$`Segment ID`)
  cat("Done\n")
  ## Find distance of each track
  cat("Computing track distances and elevation changes .... ")
  resgpx$alldist <- distAlongTrack(resgpx)
  tmp <- resgpx %>%
    dplyr::group_by(.data$trackID) %>%
    dplyr::summarize(maxd=max(.data$alldist),mind=min(.data$alldist)) %>%
    dplyr::mutate(Distance=.data$maxd-.data$mind) %>%
    dplyr::select(.data$trackID,.data$Distance)
  tmp3 <- resgpx %>%
    dplyr::group_by(.data$trackID) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::select(.data$trackID,.data$Elevation)
  tmp4 <- resgpx %>%
    dplyr::group_by(.data$trackID) %>%
    dplyr::slice_tail(n=1) %>%
    dplyr::select(.data$trackID,.data$Elevation)
  tmp2 <- dplyr::left_join(tmp3,tmp4,by="trackID") %>%
    dplyr::mutate(dElevation=.data$Elevation.y-.data$Elevation.x) %>%
    dplyr::select(.data$trackID,.data$dElevation)
  resgpx <- dplyr::left_join(resgpx,tmp,by="trackID") %>%
    dplyr::left_join(tmp2,by="trackID")
  ## Append on other track information from trkinfo
  res <- dplyr::left_join(resgpx,trkinfo,by="trackID") %>%
    dplyr::select(.data$trknum,.data$trackID,.data$Primary,.data$From,.data$To,
                  .data$Type,.data$Ownership,.data$Latitude,.data$Longitude,
                  .data$Distance,.data$Elevation,.data$Time,.data$dElevation) %>%
    dplyr::mutate(Elevation=.data$Elevation*3.2808399,
                  dElevation=.data$dElevation*3.2808399)
  cat("Done\n")
  ## Write out the new CSV file
  cat("Writing data from",fnm,"to",fnmout,"... ")
  utils::write.csv(res,file=fnmout,row.names=FALSE)
  cat("Done\n")
  ## Return the data.frame
  invisible(res)
}

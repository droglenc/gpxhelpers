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
  # Set logical to finish or not (used instead of stop() later)
  finish <- TRUE
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
      }
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
    cli::cli_progress_bar("Adding GPX files",total=length(IDs))
    for (i in seq_along(IDs)) {
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
    cli::cli_progress_done()
    ## Close out the <gpx> tag as the last line
    res <- c(res,"</gpx>")
    ## Write out the new file
    writeLines(res,fnm)
    ## Send completion message
    tmp <- ifelse(fnm_existed,"appended to","combined into")
    cli::cli_alert_success("{length(IDs)} tracks {tmp} '{file.path(getwd(),fnm)}'")
  }
}

  
#' @title Write track information into a CSV file.
#' 
#' @description Combine track information with track GPX data into a single CSV file.
#' 
#' @param trkinfo A data frame that contains information about each track.
#' @param pin Path after the working directory that contains the original GPX files.
#' @param pout Path after the working directory to put the single resultant GPX file.
#' @param fnm A filename sans extension that contains the name for the output CSV file.
#' @param IDs a character vector of track IDs that should be combined into a single GPX file. If \code{NULL} then all GPX files in \code{pin} will be combined or if \code{fnm} exists in \code{pout} then all GPX files with modification dates after the modification date for \code{fnm} will be appended to \code{fnm}.
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
writeGPXnInfo2CSV <- function(trkinfo,pin,pout,fnm,IDs=NULL) {
  # Set logical to finish or not (used instead of stop() later)
  finish <- TRUE
  # Determine if fnm has an extension, if not then add ".csv"
  if(tools::file_ext(fnm)=="") fnm <- paste0(fnm,".csv")
  
  # See if the output file exists or not in pout ...
  tmp <- list.files(pattern="csv",path=pout)
  fnm_existed <- fnm %in% tmp 
  # Make the full name of output file
  fnm <- file.path(pout,fnm)
  if (fnm_existed) {
    ## if the output file existed then ... read the file ... and ...
    res <- utils::read.csv(fnm)
    ## ... get the last modified date
    fnm_modtime <- file.info(fnm)$mtime
    ## Send message
    cli::cli_alert_info("The output file '{file.path(getwd(),fnm)}' already exists! Track files modified after {as.character(fnm_modtime)} will be appended to it.")
  }
  
  # Make list of IDs (i.e., gpx track files)
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
      }
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
      cli::cli_alert_info("The following tracks existed in the output file but have been modified since that file was created. Thus, they will be removed and the newer track data will be appended to the output file.")
      cat("\n")
      cat(paste(unique(res$trackID[tmp]),collapse=" "))
      cat("\n\n")
      ## Remove IDs from res
      res <- res[-tmp,]
    }
  }

  # Cycle through IDs (gpx files) appending them to res
  if (finish) {
    cli::cli_progress_bar("Adding GPX files",total=length(IDs))
    for (i in seq_along(IDs)) {
      # Get trkinfo for just the current ID
      restrkinfo <- dplyr::filter(trkinfo,.data$trackID==IDs[i])
      ## Read gpx file, get just tracks object, list should have only 1 so get
      ##   just it, add trackID variable, change elevation to feet, rename the
      ##   Segment ID variable, and remove extensions variable
      resgpx <- gpx::read_gpx(file.path(pin,IDs[i]))$tracks[[1]] |>
        dplyr::mutate(trackID=IDs[i],
                      Elevation=.data$Elevation*3.2808399) |>
        dplyr::rename(trknum=.data$`Segment ID`) |>
        dplyr::select(-.data$extensions)
      ## Append cumulative distance at each point along the track
      resgpx$alldist <- distAlongTrack(resgpx)
      ## Add the total distance and elevation change (beginning to end) for track
#      resgpx <- resgpx |>
#        dplyr::mutate(Distance=max(alldist)-min(alldist),
#                      dElevation=Elevation[length(Elevation)]-Elevation[1])
      ## Append on track info
      tmp <- dplyr::left_join(resgpx,restrkinfo,by="trackID") |>
        dplyr::select(.data$trknum,.data$trackID,.data$Primary,.data$From,.data$To,
                      .data$Type,.data$Ownership,.data$Latitude,.data$Longitude,
                      .data$Elevation,.data$Time)
      ## Append to results
      if (fnm_existed | i>1) res <- rbind(res,tmp)
      else (res <- tmp)
      cli::cli_progress_update()
    }
    cli::cli_progress_done()
    ## Write out the new file
    utils::write.csv(res,file=fnm,row.names=FALSE)
    ## Send completion message
    tmp <- ifelse(fnm_existed,"appended to","combined into")
    cli::cli_alert_success("{length(IDs)} tracks {tmp} '{file.path(getwd(),fnm)}'")
  }
}

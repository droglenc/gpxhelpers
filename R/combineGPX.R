#' @title Combine tracks into a single GPX file.
#' 
#' @description Combine given GPX track files into a single GPX file.
#' 
#' @param pin Path after the working directory that contains the original GPX files.
#' @param pout Path after the working directory to put the single resultant GPX file.
#' @param fnm A filename sans extension for the resultant file (a ".gpx" will be added as appropriate).
#' @param IDs a character vector of track IDs that should be combined into a single GPX file. If \code{NULL} then all GPX files in \code{pin} will be combined.
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
  ## If IDs is NULL then put all GPX files in pin within current wd into IDS
  ## Else append .gpx to IDs given by user to make
  if (is.null(IDs)) IDs <- list.files(pattern="gpx",path=pin)
  else IDs <- paste0(IDs,".gpx")
  ## Cycle through files ... appending them to each other
  for (i in seq_along(IDs)) {
    ## Read GPX file
    tmp <- readLines(file.path(pin,IDs[i]))
    ## Handle first file differently
    if (i==1) res <- tmp[-length(tmp)]
    else {
      trk_start <- which(grepl("<trk>",tmp))
      trk_end <- which(grepl("</trk>",tmp))
      res <- c(res,tmp[trk_start:trk_end])
    }
  }
  ## Close out the <gpx> tag as the last line
  res <- c(res,"</gpx>")
  ## Write out the new file
  writeLines(res,file.path(pout,paste0(fnm,".gpx")))
  cat(length(IDs)," tracks combined into '",paste0(fnm,".gpx"),
      "' in ",file.path(getwd(),pout),"\n",sep="")
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
  resgpx$Distance <- distAlongTrack(resgpx)
  cat("Done\n")
  ## Append on other track information from trkinfo
  res <- dplyr::left_join(resgpx,trkinfo,by="trackID") %>%
    dplyr::select(.data$trknum,.data$trackID,.data$Primary,.data$From,.data$To,
                  .data$Type,.data$Ownership,.data$Latitude,.data$Longitude,
                  .data$Distance,.data$Elevation,.data$Time) %>%
    dplyr::mutate(Elevation=.data$Elevation*3.2808399)
  ## Write out the new CSV file
  utils::write.csv(res,file=fnmout,row.names=FALSE)
  cat("Data from",fnm,"written to",fnmout,"\n")
  ## Return the data.frame
  invisible(res)
}

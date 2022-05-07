#' @title Sanitize tracks.
#' 
#' @description Sanitize all GPX track files in a directory by removing the  \code{type} and  \code{extension} fields, adding a  \code{desc}ription field, and replacing  \code{time} with dummy times.
#' @param pin Path after the working directory that contains the original GPX files.
#' @param pout Path after the working directory to put the sanitized GPX files.
#' @param trkinfo A data frame that contains information about each track.
#' @param moddate A string with a date for which all GPX files in  \code{pin} after this date will be sanitized. Default to today's date.
#' @param basedate A string with a date that will serve as the base date for the dummy times in the sanitized GPX file. Defaults to "2022-01-01".
#' @param verbose A logical for whether to show progress or not of files being sanitized.
#' 
#' @details NONE YET
#' 
#' @return None, used for side effect of writing sanitized GPX tracks to the  \code{pout} directory.
#' 
#' @author Derek H. Ogle
#' @keywords manip
#' 
#' @examples
#' ## None yet.
#' 
#' @export
## MAIN function to sanitize all tracks in pin directory
sanitizeTracks <- function(pin,pout,trkinfo,
                           moddate=lubridate::today(),basedate="2022-01-01",
                           verbose=TRUE) {
  moddate <- as.Date(moddate)
  ## Find all GPX files in the pin directory within the current wd
  fns <- list.files(pattern="gpx",path=pin)
  ## Reduce to only those files modified since the given date
  finfo <- file.info(file.path(pin,fns))
  fns <- fns[which(as.Date(finfo$mtime)>moddate)]
  if (length(fns)==0) cat("\n\nNo new files to sanitize since",
                          format(moddate,format="%B %e, %Y"),
                          ".\n\n")
  else {
    ## Now sanitize those files
    for (i in fns) {
      trk <- tools::file_path_sans_ext(i)
      tmp <- trkinfo[trkinfo$trackID==trk,]
      desc <- iMakeDescription(tmp$Primary,tmp$From,tmp$To)
      if (verbose) cat("Sanitizing:",trk,"-",desc,"\n")
      iSanitizeTrack(f=i,pin=pin,pout=pout,desc=desc,basedate=basedate)
    }
  }
}

## INTERNAL function to sanitize a single track
iSanitizeTrack <- function(f,pin,pout,desc,basedate="2022-01-01") {
  ## Read GPX file
  h <- readLines(file.path(pin,f))
  ## Remove the type and extensions
  tmp <- c(which(grepl("<type>",h)),which(grepl("<extensions>",h)))
  h <- h[-tmp]
  ## Change the description
  h[which(grepl("<desc>",h))] <- paste0("  <desc>",desc,"</desc>")
  ## Find the time rows and replace them with a dummy date-time
  tmp <- which(grepl("<time>",h))
  tms <- lubridate::ymd_hms(paste(basedate,"00:00:00 CDT")) + 1:length(tmp)
  h[tmp] <- paste0("    <time>",basedate,"T",hms::as_hms(tms),"Z</time>")
  ## Write out the new file
  writeLines(h,file.path(pout,f))
}


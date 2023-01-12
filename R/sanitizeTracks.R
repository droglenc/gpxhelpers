#' @title Sanitize tracks.
#' 
#' @description Sanitize all GPX track files in a directory by removing the \code{type} and  \code{extension} fields, adding a \code{desc}ription field, and replacing \code{time} with dummy times.

#' @param trkinfo A data frame that contains information about each track.
#' @param pin Path after the working directory that contains the original GPX files.
#' @param pout Path after the working directory to put the sanitized GPX files.
#' @param basedate A string with a date that will serve as the base date for the dummy times in the sanitized GPX file. Defaults to "2022-01-01".
#' 
#' @details NONE YET
#' 
#' @return None, used for side effect of writing sanitized GPX tracks to the \code{pout} directory.
#' 
#' @author Derek H. Ogle
#' @keywords manip
#' 
#' @examples
#' ## None yet.
#' 
#' @export
## MAIN function
sanitizeTracks <- function(trkinfo,pin,pout,basedate="2022-01-01") {
  # Find all GPX files in the pin directory within the current wd
  fnins <- list.files(pattern="gpx",path=pin)
  fninsmtime <- file.info(file.path(pin,fnins))$mtime
  # Find all GPX files in the pout directory within the current wd
  fnouts <- list.files(pattern="gpx",path=pout)
  fnoutsmtime <- max(file.info(file.path(pout,fnouts))$mtime)
  # Which gpx files in pin have a modtime greater than the max modtime in pout
  fnneedsan <- fnins[fninsmtime>=fnoutsmtime]
  if (length(fnneedsan)==0) {
    cli::cli_alert_warning("No tracks have been modified since {as.character(fnoutsmtime)}. There are no files in {file.path(getwd(),pin)} to sanitize.")
  }
  else { ## Now sanitize those files
    for (i in fnneedsan) {
      trk <- tools::file_path_sans_ext(i)
      tmp <- trkinfo[trkinfo$trackID==trk,]
      if (nrow(tmp)<1)
        cli::cli_alert_warning("{trk} not found in info file; thus not sanitized!")
      else {
        desc <- iMakeDescription(tmp$Primary,tmp$From,tmp$To)
        cli::cli_alert_info("Sanitizing: {trk}-{desc}")
        iSanitizeTrack(f=i,pin=pin,pout=pout,desc=desc,basedate=basedate)
      }
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


#' @title Combine all tracks to a single GPX or CSV file.
#' 
#' @description Combine all GPX track files in a directory into a single GPX or CSV file.
#' 
#' @param pin Path after the working directory that contains the original GPX files.
#' @param pout Path after the working directory to put the sanitized GPX files.
#' @param trkinfo A data frame that contains information about each track.
#' @param type A string that indicates the type of file to create ... \code{gpx},  \code{csv}, or  \code{both}.
#' @param fnm A filename sans extension for the resultant file (a ".gpx" or ".csv" will be added as appropriate).
#' 
#' @details NONE YET
#' 
#' @return None, used for side effect of writing files to the  \code{pout} directory.
#' 
#' @author Derek H. Ogle
#' @keywords manip
#' 
#' @examples
#' ## None yet.
#' 
#' @export
## MAIN function
combineAllTracks <- function(pin,pout,trkinfo=NULL,
                             type=c("both","gpx","csv"),fnm="All Tracks") {
  type <- match.arg(type)
  if (type=="gpx"|type=="both")
    iCombineAllTracks2GPX(pin,pout,paste0(fnm,".gpx"))
  if (type=="csv"|type=="both")
    iCombineAllTracks2CSV(pin,pout,trkinfo,paste0(fnm,".csv"))
}

## INTERNAL function to write GPX file
iCombineAllTracks2GPX <- function(pin,pout,fnm) {
  ## Find all GPX files in the pin directory within the current wd
  fns <- list.files(pattern="gpx",path=pin)
  ## Cycle through files ... appending them
  for (i in seq_along(fns)) {
    ## Read GPX file
    tmp <- readLines(file.path(pin,fns[i]))
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
  writeLines(res,file.path(pout,fnm))
  cat("\n\nAll",length(fns),"tracks in",pin,
      "written to",file.path(pout,fnm),"\n\n")
}

## INTERNAL function to write CSV file
iCombineAllTracks2CSV <- function(pin,pout,trkinfo,fnm) {
  ## Find all GPX files in the pin directory within the current wd
  fns <- list.files(pattern="gpx",path=pin)
  nms <- tools::file_path_sans_ext(fns)
  ## Cycle through files ... appending them
  for (i in seq_along(fns)) {
    ## Read GPX file
    tmp <- gpx::read_gpx(file.path(pin,fns[i]))$tracks[[1]] %>%
      dplyr::select(-.data$extensions,-.data$`Segment ID`) %>%
      dplyr::mutate(ID=tools::file_path_sans_ext(fns[i]))
    tmp$trknum=i
    tmp$Distance=distAlongTrack(tmp)
    if (i==1) res <- tmp
    else res <- rbind(res,tmp)
  }
  ## Append on other segment information
  res <- dplyr::left_join(res,trkinfo,by="ID") %>%
    dplyr::select(.data$trknum,.data$ID,.data$Primary,.data$Description,
                  .data$Type,.data$Ownership,.data$Latitude,.data$Longitude,
                  .data$Distance,.data$Elevation,.data$Time) %>%
    dplyr::mutate(Elevation=.data$Elevation*3.2808399)
  ## Write out the new file
  utils::write.csv(res,file=file.path(pout,fnm),row.names=FALSE)
  cat("\n\nAll",length(fns),"tracks in",pin,
      "written to",file.path(pout,fnm),"\n\n")
}

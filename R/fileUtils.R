#' @title Compare file names in a directory to names in the information file.
#' 
#' @description Compare file names in a directory to names in the information file to see if any tracks are missing in either place.
#' 
#' @param pin Path to directory with track files.
#' @param trkinfo Database with track information.
#' 
#' @details NONE YET
#' 
#' @return A logical that indicates whether an issue between the GPX files and information file was identified or not.
#' 
#' @author Derek H. Ogle
#' @keywords manip
#' 
#' @examples
#' ## None yet.
#' 
#' @export
compareFiles2Info <- function(pin,trkinfo) {
  OK <- TRUE
  trksInWD <- tools::file_path_sans_ext(list.files(pattern="gpx",path=pin))
  trksInInfo <- trkinfo$trackID
  tmp <- !trksInWD %in% trksInInfo
  if (any(tmp)) {
    cli::cli_alert_danger("Tracks in '{pin}' not in info file: {paste(trksInWD[tmp],collapse=' ')}")
    OK <- FALSE
  } else cli::cli_alert_success("All tracks in '{pin}' are in the info file!")
  tmp <- !trksInInfo %in% trksInWD
  if (any(tmp)) {
    cli::cli_alert_danger("Tracks in info file not in '{pin}': {paste(trksInInfo[tmp],collapse=' ')}")
    OK <- FALSE
  } else cli::cli_alert_success("All tracks in info file are in '{pin}'!")
  OK
}


#' @title Determine track status.
#' 
#' @description Determine if the GPX files in \code{pin} are \dQuote{NEW} (i.e., never sanitized), \dQuote{MODIFIED} (i.e., GPX in \code{pin} is more recent than GPX in \code{pout}), or \dQuote{EXISTING} (i.e.,GPX is in \code{pin} and is not more recent than sanitized GPX in \code{pout}.

#' @param trkinfo A data frame that contains information about each track.
#' @param pin Path after the working directory that contains the original GPX files.
#' @param pout Path after the working directory to put the sanitized GPX files.
#' 
#' @details NONE YET
#' 
#' @return Returns \code{trkinfo} with new \code{modin} for modification time of GPX in \code{pin}, \code{modout} for modification time of GPX in \code{pout}, and \code{status} for \dQuote{status} of GPX file in \code{pin} (i.e., \dQuote{NEW}, \dQuote{MODIFIED}, or \dQuote{EXISTING}).
#' 
#' @author Derek H. Ogle
#' @keywords manip
#' 
#' @examples
#' ## None yet.
#' 
#' @export
findFileStatus <- function(trkinfo,pin,pout) {
  ## Get file modification times for all GPX files in pin
  fnins <- list.files(pattern="gpx",path=pin)
  dfins <- data.frame(trackID=tools::file_path_sans_ext(fnins),
                      modin=file.info(file.path(pin,fnins))$mtime)
  ## Get file modification times for all GPX files in pout
  fnouts <- list.files(pattern="gpx",path=pout)
  dfouts <- data.frame(trackID=tools::file_path_sans_ext(fnouts),
                       modout=file.info(file.path(pout,fnouts))$mtime)
  ## Add file modification times for pin and pout to trkinfo
  ## Create a status variable based on comparing original GPX info in pin to
  ##   sanitized GPX info in pout to determine if the sanitized file is ...
  ##     EXISTING ... in pout and mod time for file in pin & pout are same
  ##     NEW ... in pin but not in pout (so original exists, but not sanitized)
  ##     MODIFIED ... in pin & pout but pin has more recent mod time (so it has
  ##                  been modified since it was previously sanitized) 
  trkinfo <- trkinfo |>
    dplyr::left_join(dfins,by="trackID") |>
    dplyr::left_join(dfouts,by="trackID") |>
    dplyr::mutate(status=dplyr::case_when(
      is.na(modout) ~ "NEW",
      modin>modout ~ "MODIFIED",
      .default = "EXISTING"
    )) |>
    as.data.frame()
  ## Get vector of track IDs for NEW, MODIFIED, and REMOVED
  trks_new <- trkinfo$trackID[trkinfo$status=="NEW"]
  trks_mod <- trkinfo$trackID[trkinfo$status=="MODIFIED"]
  trks_rmvd <- tools::file_path_sans_ext(fnouts[!fnouts %in% fnins])
  ## return list with modified trkinfo and vectors of track IDs
  list(trkinfo=trkinfo,trks_new=trks_new,trks_mod=trks_mod,trks_rmvd=trks_rmvd)
}


#' @title Sanitize tracks.
#' 
#' @description Sanitize all GPX track files in a directory by removing the \code{type} and  \code{extension} fields, adding a \code{desc}ription field, and replacing \code{time} with dummy times.

#' @param trkinfo A data frame that contains information about each track.
#' @param pin Path after the working directory that contains the original GPX files.
#' @param pout Path after the working directory to put the sanitized GPX files.
#' @param basedate A string with a date that will serve as the base date for the dummy times in the sanitized GPX file. Defaults to "2022-01-01".
#' @param msgcutoff A numeric that indicates the maximum number of tracks to be sanitized before an overall progress bar will be given (rather than individual notices).
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
sanitizeTracks <- function(trkinfo,pin,pout,basedate=NULL,msgcutoff=100) {
  ## Get number of tracks to sanitize
  n <- nrow(trkinfo)
  ## Use a progress bar rather than individual messages if n>msgcutoff
  if (n>msgcutoff) cli::cli_progress_bar(paste("Sanitizing",n,"tracks"),total=n)
  ## Sanitize all tracks in the pin directory within the current wd that are in trkinfo
  for (i in seq_along(trkinfo$trackID)) {
    tmp <- trkinfo[i,]
    desc <- iMakeDescription(tmp$Primary,tmp$From,tmp$To)
    if (n>msgcutoff) cli::cli_progress_update()
    else cli::cli_alert_info("Sanitizing: {tmp$trackID}-{desc}")
    iSanitizeTrack(f=paste0(tmp$trackID,".gpx"),
                   pin=pin,pout=pout,desc=desc,basedate=basedate)
  }
  if (n>msgcutoff) cli::cli_progress_done()
}

## INTERNAL function to sanitize a single track
iSanitizeTrack <- function(f,pin,pout,desc,basedate=NULL) {
  ## Read GPX file
  h <- readLines(file.path(pin,f))
  ## Sometimes tracks have a "hidden" attribute that is unneeded and ultimately
  ##   causes problems. Determine if this is the case here and replace it
  ##   with a regular <trk> tag
  tmp <- grep("<trk hidden=\"hidden\">",h)
  if (length(tmp)>0) {
    h[tmp] <- "<trk>"
    cli::cli_alert_danger("'{f} has a 'hidden' attribute in '<trk>' tag.")
  }
  ## Remove the type and extensions
  tmp <- c(which(grepl("<type>",h)),which(grepl("<extensions>",h)))
  h <- h[-tmp]
  ## Change the description
  h[which(grepl("<desc>",h))] <- paste0("  <desc>",desc,"</desc>")
  ## Find the time rows ...
  tmp <- which(grepl("<time>",h))
  ## ... and, if no basedate is given, isolate the date in the track
  if (is.null(basedate)) {
    basedate <- substr(h[tmp[1]],11,1000)    ## removes first <time>
    basedate <- substr(basedate,1,unlist(gregexpr("T",basedate))[1]-1)
  }
  ## ... and replace them with that date and a dummy time
  tms <- lubridate::ymd_hms(paste(basedate,"00:00:00 CDT")) + 1:length(tmp)
  h[tmp] <- paste0("    <time>",basedate,"T",hms::as_hms(tms),"Z</time>")
  ## Write out the new file
  writeLines(h,file.path(pout,f))
}



#' @title Combine tracks into a single master GPX file.
#' 
#' @description Combine given GPX track files into a single master GPX file. If the single GPX file exists then tracks will be appended.
#' 
#' @param trkinfo A data frame that contains information about each track.
#' @param pin Path after the working directory that contains the original GPX files.
#' @param pout Path after the working directory to put the single resultant GPX file.
#' @param fnm A filename sans extension for the resultant file (a ".gpx" will be added as appropriate).
#' @param msgcutoff A numeric that indicates the maximum number of tracks to be sanitized before an overall progress bar will be given (rather than individual notices).
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
makeMasterGPX <- function(trkinfo,pin,pout,fnm,msgcutoff=100) {
  ## Determine if fnm has an extension, if not then add ".gpx"
  if(tools::file_ext(fnm)=="") fnm <- paste0(fnm,".gpx")
  ## Determine if the output file exists in pout ...
  fnm_existed <- fnm %in% list.files(pattern="gpx",path=pout)
  ## Make the full name of output file
  fnm <- file.path(pout,fnm)

  ## Get out immediately if there is nothing to create or modify
  tmp <- c(trkinfo$trks_rmvd,trkinfo$trks_mod,trkinfo$trks_new)
  if (fnm_existed & length(tmp)==0) {
    cli::cli_alert_warning("'{fnm}' exists, but no tracks to add, modify, or remove!")
  } else {
    ## There is something to do
    if (!fnm_existed) {
      ## Get all IDs in pin (as output GPX file did not exist)
      IDs2Add <- tools::file_path_sans_ext(list.files(pattern="gpx",path=pin))
      ## Initiate res (which will be the master GPX) with NULL
      res <- NULL
    } else {
      ## The output file existed so ... read the file ...
      res <- readLines(fnm)
      ## ... remove the last line to start the new output file
      res <- res[-length(res)]
      
      ## Remove IDs for GPX files that were removed from pin or have been modified
      IDs2Remove <- c(trkinfo$trks_rmvd,trkinfo$trks_mod)
      if (length(IDs2Remove>0)) {
        cli::cli_alert_info("'{fnm}' already exists and will be modified!")
        res <- iRemoveTracksFromMasterGPX(res,IDs2Remove)
      }
      
      ## Make IDs to add to the existing file (i.e., for all new & modified tracks)
      IDs2Add <- c(trkinfo$trks_new,trkinfo$trks_mod)
    }
    
    n <- length(IDs2Add) 
    if (n==0) 
      cli::cli_alert_warning("No new or modified tracks to combine to master GPX")
    else {
      if (is.null(res)) cli::cli_alert_info("'{fnm}' does not exist but will be created!")
      else if (!length(IDs2Remove>0)) cli::cli_alert_info("'{fnm}' already exists and will be modified!")
      if (n>msgcutoff)
        cli::cli_progress_bar(paste("Add",n,"tracks to master GPX"),total=n)
      ## Cycle through GPX files adding each one to res (which is initiated w NULL)
      for (i in seq_along(IDs2Add)) {
        res <- iAddTrack2GPX(res,pin,IDs2Add[i])
        if (n>msgcutoff) cli::cli_progress_update()
        else cli::cli_alert_info("Add {IDs2Add[i]} to master GPX")
      }
      ## Close out the <gpx> tag as the last line
      res <- c(res,"</gpx>")
      ## Write out the new file
      cli::cli_alert_info("Writing '{fnm}'")
      writeLines(res,fnm)
      if (n>msgcutoff) cli::cli_progress_done()
    }
  }
  ## return nothing
  invisible()
}


iAddTrack2GPX <- function(res,pin,ID) {
  ## Read gpx file
  tmp <- readLines(paste0(file.path(pin,ID),".gpx"))
  ## Add track to res (which is the master gpx)
  ## If res is NULL then start with full track gpx sans last line
  if (is.null(res)) res <- tmp[-length(tmp)]
  else {
    ## Otherwise get from <trk> to </trk> and append to res
    trk_start <- grep("<trk>",tmp)
    trk_end <- grep("</trk>",tmp)
    res <- c(res,tmp[trk_start:trk_end])
  }
  res
}

iRemoveTracksFromMasterGPX <- function(res,IDs2Remove) {
  ## combine all IDs into a string that can be used in grep()
  tmp <- paste(IDs2Remove,collapse="|")
  ## then grep to see if any of those IDs are in res (the existing output file)
  tmp <- grep(tmp,res)
  ## if any IDs existed in the output file then they must be removed
  if (length(tmp)!=0) {
    ## grep above returned position of <name> tag, <trk> tag is right above it
    ##   thus, this is now the position of the <trk> tag for the IDs to remove
    tmp <- tmp-1
    ## positions of all <trk> and </trk> tags in output file
    trk_starts <- grep("<trk>",res)
    trk_ends <- grep("</trk>",res)
    ## check for same length ... throw error if they are not
    if (length(trk_starts)!=length(trk_ends)) 
      cli::cli_abort(c("Unmatched <trk> and </trk> tags in existig master GPX file",
                       "i"="Check for special attributes (e.g., 'hidden') in <trk>"))
    ## which trk_starts correspond to IDs to remove ...
    trk_pos <- which(trk_starts %in% tmp)
    ## make vector of all rows in res to remove (i.e., between <trk> and </trk>
    ##   for all IDs in IDs2Remove)
    rows2remove <- NULL
    for (i in seq_along(trk_pos))
      rows2remove <- c(rows2remove,trk_starts[trk_pos[i]]:trk_ends[trk_pos[i]])
    ## Actually remove the <trk> to </trk> for each IDs to remove
    res <- res[-rows2remove]
    ## Send message
    cli::cli_alert_info("Tracks removed from existing master GPX file: {paste(IDs2Remove,collapse=', ')}")
  }
  res
}


#' @title Write all track information into a master CSV file.
#' 
#' @description Combine track information with track GPX data into a single CSV file.
#' 
#' @param trkinfo A data frame that contains information about each track.
#' @param pin Path after the working directory that contains the original GPX files.
#' @param pout Path after the working directory to put the single resultant GPX file.
#' @param fnm A filename sans extension that contains the name for the output CSV file.
#' @param msgcutoff A numeric that indicates the maximum number of tracks to be sanitized before an overall progress bar will be given (rather than individual notices).
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
makeMasterCSV <- function(trkinfo,pin,pout,fnm,msgcutoff=100) {
  ## Determine if fnm has an extension, if not then add ".gpx"
  if(tools::file_ext(fnm)=="") fnm <- paste0(fnm,".csv")
  ## Determine if the output file exists in pout ...
  fnm_existed <- fnm %in% list.files(pattern="csv",path=pout)
  ## Make the full name of output file
  fnm <- file.path(pout,fnm)
  
  ## Get out immediately if there is nothing to create or modify
  tmp <- c(trkinfo$trks_rmvd,trkinfo$trks_mod,trkinfo$trks_new)
  if (fnm_existed & length(tmp)==0) {
    cli::cli_alert_warning("'{fnm}' exists, but no tracks to add, modify, or remove!")
  } else {
    ## There is something to do
    if (!fnm_existed) {
      ## Get all IDs in pin (as output CSV file did not exist)
      IDs2Add <- tools::file_path_sans_ext(list.files(pattern="gpx",path=pin))
      ## Initiate res (which will be the master GPX) with NULL
      res <- NULL
    } else {
      ## The output file existed then ... read the file ...
      res <- as.data.frame(data.table::fread(fnm))

      ## Remove IDs for GPX files that were removed from pin or have been modified
      IDs2Remove <- c(trkinfo$trks_rmvd,trkinfo$trks_mod)
      if (length(IDs2Remove>0)) {
        cli::cli_alert_info("'{fnm}' already exists and will be modified!")
        res <- iRemoveTracksFromMasterCSV(res,IDs2Remove)
      }

      ## Make IDs to add to the existing file (i.e., for all new & modified tracks)
      IDs2Add <- c(trkinfo$trks_new,trkinfo$trks_mod)
    }
    
    n <- length(IDs2Add)
    if (n==0) 
      cli::cli_alert_warning("No new or modified tracks to combine to master CSV")
    else {
      if (is.null(res)) cli::cli_alert_info("'{fnm}' does not exist but will be created!")
      else if (!length(IDs2Remove>0)) cli::cli_alert_info("'{fnm}' already exists and will be modified!")
      if (n>msgcutoff) 
        cli::cli_progress_bar(paste("Add",n,"tracks to master CSV"),total=n)
      ## Cycle through GPX files adding each one to res
      for (i in seq_along(IDs2Add)) {
        res <- iAddTrack2MasterCSV(res,trkinfo,pin,IDs2Add[i])
        if (n>msgcutoff) cli::cli_progress_update()
        else cli::cli_alert_info("Add {IDs2Add[i]} to master CSV")
      }
      
      ## Write out the new file
      cli::cli_alert_info("Writing '{fnm}'")
      data.table::fwrite(res,file=fnm,row.names=FALSE)
      if (n>msgcutoff) cli::cli_progress_done()
    }
  }
  
  ## return nothing
  invisible()
}


iAddTrack2MasterCSV <- function(res,trkinfo,pin,ID) {
  # Get trkinfo for just the current ID
  restrkinfo <- dplyr::filter(trkinfo$trkinfo,.data$trackID==ID)
  ## Read gpx file, get just tracks object, list should have only 1 so get
  ##   just it, add trackID variable, change elevation to feet, rename the
  ##   Segment ID variable, and remove extensions variable
  resgpx <- gpx::read_gpx(paste0(file.path(pin,ID),".gpx"))$tracks[[1]] |>
    dplyr::mutate(trackID=ID,
                  Elevation=.data$Elevation*3.2808399,
                  Time=as.character(.data$Time)) |>
    dplyr::rename(trknum=.data$`Segment ID`) |>
    dplyr::select(-.data$extensions)
  ## Append cumulative distance at each point along the track
  resgpx$Distance <- distAlongTrack(resgpx)
  ## Append on track info
  tmp <- dplyr::left_join(resgpx,restrkinfo,by="trackID") |>
    dplyr::select(.data$trknum,.data$trackID,
                  .data$Primary,.data$From,.data$To,.data$Type,.data$Ownership,
                  .data$Time,.data$Latitude,.data$Longitude,
                  .data$Distance,.data$Elevation)
  res <- rbind(res,tmp)
}

iRemoveTracksFromMasterCSV <- function(res,IDs2Remove) {
  ## combine all IDs into a string that can be used in grep()
  tmp <- paste(IDs2Remove,collapse="|")
  ## then grep to see if any of those IDs are in res (the existing output file)
  tmp <- grepl(tmp,res$trackID)
  ## if any IDs existed in the output file then they must be removed
  if (any(tmp)) {
    ## Send message (first find track names that will be removed)
    cli::cli_alert_info("Tracks removed from existing master CSV file: {paste(IDs2Remove,collapse=', ')}")
    ## Remove IDs from res
    res <- res[!tmp,]
  }
  res
}



#' @title Rename tracks.
#' 
#' @description Rename gpx filenames but also, importantly, adjusting the track name in the \code{<name>} tag.

#' @param fold Name for \dQuote{old} GPX file.
#' @param fnew Name for \dQuote{new} GPX file.
#' @param pnew Path after the working directory to contain the \dQuote{new} GPX files.
#' 
#' @details NONE YET
#' 
#' @return None, used for side effect of writing new GPX files to the \code{pth} directory (and possibly deleting now old GPX files from the same directory).
#' 
#' @author Derek H. Ogle
#' @keywords manip
#' 
#' @examples
#' \dontrun{
#' ## Set working directory to where the files are
#' ## Single file
#' renameGPXfile("NCTBF01","NCTBF001")
#' 
#' ## Example of multiple files with a consistent file change pattern
#' olds <- c("NCTBF01","NCTBF07a")
#' news <- stringr::str_replace(olds,"NCTBF","NCTBF0")
#' cbind(olds,news)
#' renameGPXfile(olds,news)
#' 
#' ## Example of multiple files to get new numbers padded with zeroes
#' olds <- c("NCTBF01","NCTBF07a")
#' news <- paste0("NCTBF",stringr::str_pad(seq_along(olds),width=3,pad=0))
#' cbind(olds,news)
#' renameGPXfile(olds,news)
#' }
#' 
#' @export
renameGPXfile <- function(fold,fnew,pnew="renamed") {
  ## Determine if fold and fnew are same length
  if (length(fnew)!=length(fold))
    cli::cli_abort("'fold' and 'fnew' are different lengths.")
  ## Determine if any fold in current working directory
  pold <-getwd()
  tmp <- tools::file_path_sans_ext(list.files(pattern="gpx",path=pold))
  if (!any(tools::file_path_sans_ext(fold) %in% tmp))
    cli::cli_abort("No files in 'fold' are in the current working directory.")
  ## Create new folder for new files
  pnew <- file.path(pold,pnew)
  if (!file.exists(pnew)) dir.create(pnew)
  cli::cli_alert_info("Renamed files will be in {pnew}.")
  ## Cycle through files to change
  for (i in seq_along(fold))
    iRenameGPXfile(fold[i],fnew[i],pold,pnew)
}

iRenameGPXfile <- function(fold,fnew,pold,pnew) {
  ## Determine if fold has an extension; if so, remove and save name in nold;
  ##  if not then save fold into nold and add ".gpx" to fold
  if(tools::file_ext(fold)=="") {
    nold <- fold
    fold <- paste0(fold,".gpx")
  } else nold <- tools::file_path_sans_ext(fold)
  ## same for fnew/nnew
  if(tools::file_ext(fnew)=="") {
    nnew <- fnew
    fnew <- paste0(fnew,".gpx")
  } else nnew <- tools::file_path_sans_ext(fnew)
  ## Make full names for files
  fold <- file.path(pold,fold)
  fnew <- file.path(pnew,fnew)
  ## See if the old file exists or not in pold ... if so then read/process file
  ##   ... if not then send warning and do nothing
  fnm_existed <- fold %in% list.files(pattern="gpx",path=pold,full.names=TRUE)
  if (!fnm_existed) {
    cli::cli_alert_warning("{fold} not found in {pold}; thus, no renaming done!")
  } else {
    res <- readLines(fold)
    # find line with nold in it (this will be in <name></name>)
    tmp <- which(grepl(nold,res))
    # modify that line by replacing nold with new
    res[tmp] <- stringr::str_replace(res[tmp],nold,nnew)
    # Write out the new file
    writeLines(res,fnew)
    cli::cli_alert_success("'{nold}.gpx' --> '{nnew}.gpx'")
  }
} 

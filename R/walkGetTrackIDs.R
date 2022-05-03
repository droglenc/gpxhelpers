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
  d %>%
    dplyr::filter(.data$trackID %in% walkIDs) %>%
    dplyr::mutate(Photo=file.path(path,paste0(.data$Photo,".jpg")),
                  Caption=paste0(.data$trackID,": ",.data$Caption))
}
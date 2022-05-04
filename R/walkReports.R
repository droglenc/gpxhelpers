#' @title Write walk report HTML files.
#' 
#' @description Write \dQuote{walk} report HTML files.
#' 
#' @param walk A vector of walk codes.
#' @param project A project name.
#' @param datfile The name of the file in \code{pth} that contains all of the track information.
#' @param pth A path string to \code{datfile} and where the resultant HTML file will be written.
#' @param tmplt A name for the template to use.
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
#' pth <- "C:/aaaPersonal/Maps_GPS/Bayfield_County"
#' datfile <- "All Bayfield.csv"
#' project <- "Bayfield County"
#' walk <- c("FR4101")
#' walkReports(walk,project,datfile,pth)
#' }
#' 
#' @rdname walkMap
#' @export 
walkReports <- function(walk,project,datfile,pth="",tmplt="Walk_Template.Rmd") {
  tmplt <- file.path(system.file("templates",package="gpxhelpers"),tmplt)
  for (i in seq_along(walk)) {
    ofn <- paste0(walk[i],"_walk.html")
    message("Processing '",ofn,"'")
    rmarkdown::render(input=tmplt,
                      params=list(pth=pth,
                                  project=project,
                                  datfile=datfile,
                                  walk=walk[i]),
                      output_file=ofn,
                      output_dir=pth,
                      quiet=TRUE,
                      envir=new.env())
  }
}


#' @title Write walk report HTML files.
#' 
#' @description Write \dQuote{walk} report HTML files.
#' 
#' @param walk A vector of walk codes.
#' @param project A project name.
#' @param datfile The name of the file in \code{pth} that contains all of the track information.
#' @param basedir A path string to where \code{datfile}, the folder with the images, and the folder in which to put the resultant HTML file reside.
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
#' basedir <- "C:/aaaPersonal/Maps_GPS/Bayfield_County"
#' datfile <- "All Bayfield.csv"
#' project <- "Bayfield County"
#' walk <- c("FR4101","RYND1","JANN1","NDLTS1","MSKY2WLDN1","EAGLEBLOCK","OGLEBLOCK")[1]
#' walkReports(walk,project,datfile,basedir)
#' }
#' 
#' @rdname walkMap
#' @export 
walkReports <- function(walk,project,datfile,basedir,
                        tmplt="Walk_Template.Rmd") {
  tmplt <- file.path(system.file("templates",package="gpxhelpers"),tmplt)
  for (i in seq_along(walk)) {
    ofn <- paste0(walk[i],"_walk.html")
    message("Processing '",ofn,"'")
    rmarkdown::render(input=tmplt,
                      params=list(basedir=basedir,
                                  project=project,
                                  datfile=datfile,
                                  walk=walk[i]),
                      output_dir="Walks",
                      output_file=ofn,
                      quiet=TRUE,
                      envir=new.env())
  }
}


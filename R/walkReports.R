#' @title Write walk report HTML files.
#' 
#' @description Write \dQuote{walk} report HTML files.
#' 
#' @param walk A vector of walk codes.
#' @param project A project name.
#' @param datfile The name of the file in \code{pth} that contains all of the track information.
#' @param basedir A path string to where \code{datfile}, the folder with the images, and the folder in which to put the resultant HTML file reside.
#' @param tmplt A name for the template to use.
#' @param OMap_type Type of OpenStreetMap to use.
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
#' project <- "Bayfield County"
#' basedir <- file.path("C:/aaaPersonal/Maps_GPS",project)
#' datfile <- paste0(project,".csv")
#' walk <- "FR8131"
#' walkReports(walk,project,datfile,basedir)
#' }
#' 
#' @export 
walkReports <- function(walk,project,datfile,basedir,
                        tmplt="Walk_Template.Rmd",OMap_type="bing") {
  tmplt <- file.path(system.file("templates",package="gpxhelpers"),tmplt)
  for (i in seq_along(walk)) {
    ofn <- paste0(walk[i],"_walk.html")
    cat("Processing '",ofn,"' ...",sep="")
    rmarkdown::render(input=tmplt,
                      params=list(basedir=basedir,
                                  project=project,
                                  datfile=datfile,
                                  walk=walk[i],
                                  OMap_type=OMap_type),
                      output_dir="Walks",
                      output_file=ofn,
                      quiet=TRUE,
                      envir=new.env())
    cat("Done\n")
  }
}


#' @title Write walk report HTML files.
#' 
#' @description Write \dQuote{walk} report HTML files.
#' 
#' @param walk A vector of walk codes.
#' @param project A project name.
#' @param datfile The name of the file in \code{pth} that contains all of the track information.
#' @param basedir A path string to where \code{datfile}, the folder with the images, and the folder in which to put the resultant HTML file reside.
#' @param tmplt A name for the template to use.
#' @param map_type Type of OpenStreetMap to use.
#' @param showFileInBrowser A logical for whether to open the resultant file in the broswer or not (default is to not).
#' @param quiet A logical for whether the progress of processing the markdown file should be shown (default is to not).
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
#' basedir <- file.path("C:/aaaPersonal/MAPPING",project)
#' datfile <- paste0(project,".csv")
#' walk <- "FR4138051"
#' walkReports(walk,project,datfile,basedir,showFileInBrowser=TRUE)
#' }
#' 
#' @export 
walkReports <- function(walk,project,datfile,basedir,
                        tmplt="Walk_Template.Rmd",map_type="OpenTopoMap",
                        showFileInBrowser=FALSE,quiet=TRUE) {
  tmplt <- file.path(system.file("templates",package="gpxhelpers"),tmplt)
  for (i in seq_along(walk)) {
    ofn <- paste0(walk[i],".html")
    cat("Processing '",ofn,"' to '",basedir,"' ...",sep="")
    rmarkdown::render(input=tmplt,
                      params=list(basedir=basedir,
                                  project=project,
                                  datfile=datfile,
                                  walk=walk[i],
                                  map_type=map_type),
                      output_dir="Walks",
                      output_file=ofn,
                      quiet=quiet,
                      envir=new.env())
    cat("Done\n")
    if (showFileInBrowser)
      utils::browseURL(paste0('file://',
                              file.path(basedir,"Walks",ofn)))
  }
}


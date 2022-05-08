library(gpxhelpers)

project <- "Bayfield County"
basedir <- file.path("c:/aaaPersonal/Maps_GPS",project)
setwd(basedir)

## Get track information
fn <- file.path("Data","Trail Mapping Info.xlsx")
info <- readxl::read_excel(fn,sheet="Tracks") %>%
  dplyr::filter(Project==project)

## Sanitize the original gpx files (remove times, update descriptions, etc.)
##   that were created after the moddate
sanitizeTracks(pin=file.path("Tracks","aaaOriginals"),
               pout="Tracks",trkinfo=info,moddate="2022-05-07")
## Combine All Tracks into a single GPX file ... useful for GoogleEarth/Maps
combineTracks2GPX(pin="Tracks",pout="Data",fnm=project)
## Write all tracks to a single CSV
dat <- writeGPXnInfo2CSV(info,file.path("Data",paste0(project,".gpx")))

## TEST ... Map all tracks
allTracksMap(dat,OMap_type="bing")

## Try a "walk"
walks <- readxl::read_excel(fn,sheet="Walks") %>%
  dplyr::filter(Project==project)
( walkList <- unique(walks$Walk) )

walkIDs <- walkGetTrackIDs(walks,whichWalk=walkList[11])
awalk <- walkMaker(dat,info,walkIDs,findOrder=FALSE)
walkMap(awalk,OMap_type="bing")
walkElevation(awalk)
walkSummary(awalk)

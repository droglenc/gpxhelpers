library(gpxhelpers)
library(patchwork)

setwd("C:/aaaPersonal/Maps_GPS/Bayfield_County")

## Get segment information
### Run these if you need to update the local Excel file (from GoogleDrive)
# fn <- googledrive::as_id("1gxSPHK_R1XVzknsKrOeu4Oy8hWf6BjDnwk7-SXnbdrM")
# googledrive::drive_download(file=fn,overwrite=TRUE)
info <- readxl::read_excel("Trail Mapping Segment Info.xlsx",
                           sheet="Segments")

## Sanitize the original gpx files (remove times, update descriptions, etc.)
##   that were created after the moddate
sanitizeTracks(pin="Segments/aaaOriginals",
               pout="Segments",
               trkinfo=info,
               moddate="2022-05-01")
## Combine All Tracks into a single GPX file ... useful for GoogleEarth/Maps
## ... and to a single CSV file ... read in below for graphing in ggplot2
combineAllTracks(pin="Segments",pout="./",trkinfo=info)

## Read in all tracks data
dat <- read.csv(file.path("./","All Tracks.csv"))
### Map all tracks
mapAllTracks(dat)
mapAllTracks(dat,OMap_type="none")
mapAllTracks(dat,OMap_type="none",inclLabels=FALSE)
mapAllTracks(dat,inclLabels=FALSE,
             LAT_bottom=46.485,LAT_top=46.55,
             LON_left=-91.39,LON_right=-91.34)


## Try some "walks"
allTrackNames <- unique(dat$ID)
### A random order (after the first two)
walkIDs <- allTrackNames[grepl("MSKY",allTrackNames)]
( walkIDs <- c(walkIDs[1:2],walkIDs[sample(3:length(walkIDs))]) )
awalk <- walkMaker(dat,info,walkIDs)
walkMap(awalk)

### A more complicated one
walkIDs <- c("SPDR01","CTYH03","GPIT01","GPIT02","CTYH12","CTYH13","RUTH01")
awalk <- walkMaker(dat,info,walkIDs)
walkMap(awalk)
walkElevation(awalk)

### A more complicated one that uses grepl
( walkIDs <- c(allTrackNames[grepl("RYND",allTrackNames)],
               allTrackNames[grepl("BECK",allTrackNames)],
               allTrackNames[grepl("JANN",allTrackNames)]) )
awalk <- walkMaker(dat,info,walkIDs,startIDs=c("BECK01","BECK02"))
walkMap(awalk)
### ...  (and tests going backwards)
awalk <- walkMaker(dat,info,walkIDs,startIDs=c("RYND05","RYND04"))
walkMap(awalk)

## Try a route with a segment of overlap (must keep the given order)
walkIDs <- c("RYND05","RYND04","RYND03","RYND02","RYND01","RYND01",
             "WBGN01","WBGN01","RYND02","DRYW01","DRYW01",
             "RYND03","RYND04","RYND05")
awalk <- walkMaker(dat,info,walkIDs,findOrder=FALSE)
walkMap(awalk,title="Walk on 29-Jan-2022")
walkElevation(awalk,title="Walk on 29-Jan-2022")
walkSummary(awalk)

## A loop with some segment overlap
walkIDs <- c("JANN04","JANN03","JANN02","JANN01","JANN01",
             "FR39801","FR39301","FR39302","JANN04")
awalk <- walkMaker(dat,info,walkIDs,findOrder=FALSE)
p1 <- walkMap(awalk,title="Walk on 30-Jan-2022")
p2 <- walkElevation(awalk,title="Walk on 30-Jan-2022")
p1/p2  ## requires patchwork

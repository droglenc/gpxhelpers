library(gpxhelpers)

project <- "Bayfield County"
basedir <- file.path("c:/aaaPersonal/Maps_GPS",project)
setwd(basedir)

## Get track information
info <- readxl::read_excel(file.path("data",
                                     "Trail Mapping Info.xlsx"),
                                     sheet="Tracks") %>%
  dplyr::filter(Project==project)

## Sanitize the original gpx files (remove times, update descriptions, etc.)
##   that were created after the moddate
sanitizeTracks(pin=file.path("Tracks","aaaOriginals"),
               pout="Tracks",
               trkinfo=info,
               moddate="2022-05-05")
## Combine All Tracks into a single GPX file ... useful for GoogleEarth/Maps
combineTracks2GPX(pin="Tracks",pout="Data",fnm="All Bayfield")
## Write all tracks to a single CSV
dat <- writeGPXnInfo2CSV(info,file.path("Data","All Bayfield.gpx"))
## USE THIS IF NO NEW TRACKS .... Read in all tracks CSV data for use below
dat <- read.csv(file.path("Data","All Bayfield.csv"))


### Map all tracks
allTracksMap(dat)
allTracksMap(dat,OMap_type="none")
allTracksMap(dat,
             LAT_bottom=46.485,LAT_top=46.55,
             LON_left=-91.39,LON_right=-91.34)


## Try some "walks"
allTrackNames <- unique(dat$trackID)
### A random order (after the first two)
walkIDs <- allTrackNames[grepl("MSKY",allTrackNames)]
( walkIDs <- c(walkIDs[1:2],walkIDs[sample(3:length(walkIDs))]) )
awalk <- walkMaker(dat,info,walkIDs)
walkMap(awalk)
walkSummary(awalk)

### A more complicated one

walkIDs <- c("SPDR01","CTYH03","GPIT01","GPIT02","CTYH12","CTYH13","RUTH01")
awalk <- walkMaker(dat,info,walkIDs)
walkMap(awalk)
walkElevation(awalk)
walkSummary(awalk)

### A more complicated one that uses grepl
( walkIDs <- c(allTrackNames[grepl("RYND",allTrackNames)],
               allTrackNames[grepl("BECK",allTrackNames)],
               allTrackNames[grepl("JANN",allTrackNames)]) )
awalk <- walkMaker(dat,info,walkIDs,startIDs=c("BECK01","BECK02"))
walkMap(awalk)
walkSummary(awalk)
### ...  (and tests going backwards)
awalk <- walkMaker(dat,info,walkIDs,startIDs=c("RYND05","RYND04"))
walkMap(awalk)
walkSummary(awalk)

## Try a route with a track of overlap (must keep the given order)
walkIDs <- c("RYND05","RYND04","RYND03","RYND02","RYND01","RYND01",
             "WBGN01","WBGN01","RYND02","DRYW01","DRYW01",
             "RYND03","RYND04","RYND05")
awalk <- walkMaker(dat,info,walkIDs,findOrder=FALSE)
walkMap(awalk,title="Walk on 29-Jan-2022")
walkElevation(awalk,title="Walk on 29-Jan-2022")
walkSummary(awalk)

## A loop with some track overlap
walkIDs <- c("JANN04","JANN03","JANN02","JANN01","JANN01",
             "FR39801","FR39301","FR39302","JANN04")
awalk <- walkMaker(dat,info,walkIDs,findOrder=FALSE)
p1 <- walkMap(awalk,title="Walk on 30-Jan-2022")
p2 <- walkElevation(awalk,title="Walk on 30-Jan-2022")
p1/p2  ## requires patchwork
walkSummary(awalk)

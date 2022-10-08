#' Provides a list of all datasets for which IDs can be accessed with this package
#'
#'@export
datasetlist <- function(details = FALSE) {
  if (details == FALSE) {
    print(c("partyfacts","manifesto","parlgov","wikipedia","ches","clea","ejpr","vparty","wptags",
         "chisols","dpi","essprtc","essprtv","huber","kitschelt","polcon","ppmd",
         "whogov","wvs","afrelec","afro","ccdd","ccs","coppedge","cses","elecglob",
         "epac","hix","erdda","euandi","euned","gloelec","gpd","gps","ipod","janda",
         "jw","kurep","laeda","latino","laverhunt","leadglob","mackie","mapp","morgan",
         "mudde","nped","parlspeech","postyug","pip","poppa","ppdb","ppla","ppmdall","ray",
         "tap","populist","voteview","ees14","ppepe","thomas"))
  message("If you want to access a dataframe with more details about the available datasets type \"datasetlist(details = TRUE)\"")
  }
  if (details == TRUE) {
    if (file.exists(paste(find.package("paRtyids"),"/data/listofdatasets.RDATA",sep=""))) {
      load(paste(find.package("paRtyids"),"/data/listofdatasets.RDATA",sep=""), envir = .GlobalEnv)
    } else {
      message("Downloading list of datasets (this might take a few seconds)")
      listofdatasets <-
        dataverse::get_dataframe_by_name(
          filename  = "datasets.tab",
          dataset   = "10.7910/DVN/GM8LWQ",
          server    = "dataverse.harvard.edu"
        )
      if (!file.exists(paste(find.package("paRtyids"),"/data/",sep=""))) {
        dir.create(paste(find.package("paRtyids"),"/data",sep=""))
      }
      save(listofdatasets,file=paste(.libPaths()[1],"/paRtyids/data/listofdatasets.RDATA",sep=""))
    }
    print(listofdatasets, n=nrow(listofdatasets))
  }
}

datasetlist(details = TRUE)



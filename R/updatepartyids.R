#' Updates the datasets used in this package (or downloads them if that wasn't done yet)
#'
#'@export
updatepartyids <- function() {
  message("Downloading partyfacts dataset (this might take a few seconds)")
  partyfactsdataset <- read.csv("https://partyfacts.herokuapp.com/download/external-parties-csv/")
  dir.create(paste(find.package("paRtyids"),"/data",sep=""))
  save(partyfactsdataset,file=paste(find.package("paRtyids"),"/data/partyfactsdata.RDATA",sep=""))
  message("Downloading list of datasets (this might take a few seconds)")
  listofdatasets <-
    dataverse::get_dataframe_by_name(
        filename  = "datasets.tab",
        dataset   = "10.7910/DVN/GM8LWQ",
        server    = "dataverse.harvard.edu"
      )
  save(listofdatasets,file=paste(.libPaths()[1],"/paRtyids/data/listofdatasets.RDATA",sep=""))
}


updatepartyids()

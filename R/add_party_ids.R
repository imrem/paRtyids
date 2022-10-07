#' Add party ids from any of the data sets included in the partyfacts project if you have their partyfacts ids or vice versa
#'
#' Returns party IDs
#' @param ids A single id or vector of ids from the Party Facts data set.
#' @param dataset which dataset do you need IDs for/ have IDs from?
#' @param from_partyfacts logical: do you have partyfacts IDs and want to merge other IDs to it (TRUE, default)
#' or do you have other IDs and want to add partyfacts IDs (FALSE)?
#' @keywords parties partyfacts marpor cmp parlgov wikipedia ches clea ess
#' @export
#' @examples
#' not done yet
#'
#' @export
add_party_ids <- function(ids,
                          dataset = c("manifesto","parlgov","wikipedia","ches","clea","essprtc","essprtv"),
                          from_partyfacts = TRUE) {

  if (file.exists(paste(.libPaths()[1],"/paRtyids/data/partyfactsdata.RDATA",sep=""))) {
    load(paste(.libPaths()[1],"/paRtyids/data/partyfactsdata.RDATA",sep=""))
  } else {
    partyfactsdataset <-
      dataverse::get_dataframe_by_name(
        filename  = "external-parties.tab",
        dataset   = "10.7910/DVN/GM8LWQ",
        server    = "dataverse.harvard.edu"
      )
    dir.create(paste(.libPaths()[1],"/paRtyids/data",sep=""))
    save(partyfactsdataset,file=paste(.libPaths()[1],"/paRtyids/data/partyfactsdata.RDATA",sep=""))
  }
  partyids <- data.frame(as.vector(ids))

  if(from_partyfacts==TRUE) {
    names(partyids)[1] <- "partyfacts_id"
    returnvar <- "dataset_party_id"
  } else {
    names(partyids)[1] <- "dataset_party_id"
    returnvar <- "partyfacts_id"
  }

  partyfactsdataset_reduced <- partyfactsdataset[partyfactsdataset$dataset_key==dataset,]

  partyids$mergeorder <- as.integer(rownames(partyids))
  merged <- merge(partyids,partyfactsdataset_reduced,all.x=T)
  merged <- merged[order(merged$mergeorder),]
  merged$mergeorder <- NULL
  requestedvars <- merged[,returnvar]

  return(requestedvars)
}


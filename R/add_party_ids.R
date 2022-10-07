#' Add party ids from any of the data sets included in the partyfacts project if you have their partyfacts ids or vice versa
#'
#' Returns party IDs
#' @param ids A single id or vector of ids from the Party Facts data set.
#' @param dataset which dataset do you need IDs for/ have IDs from?
#' @param from_partyfacts logical: do you have partyfacts IDs and want to merge other IDs to it (TRUE, default)
#' or do you have other IDs and want to add partyfacts IDs (FALSE)?
#' @keywords parties partyfacts marpor cmp parlgov wikipedia ches clea ess
#' @examples
#' not done yet
#'
#'
#'@export
add_party_ids <- function(ids,
                          from = c("partyfacts","manifesto","parlgov","wikipedia","ches","clea","essprtc","essprtv"),
                          to = c("partyfacts","manifesto","parlgov","wikipedia","ches","clea","essprtc","essprtv")) {

  if (file.exists(paste(find.package("paRtyids"),"/data/partyfactsdata.RDATA",sep=""))) {
    load(paste(find.package("paRtyids"),"/data/partyfactsdata.RDATA",sep=""))
  } else {
    partyfactsdataset <-
      dataverse::get_dataframe_by_name(
        filename  = "external-parties.tab",
        dataset   = "10.7910/DVN/GM8LWQ",
        server    = "dataverse.harvard.edu"
      )
    dir.create(paste(find.package("paRtyids"),"/data",sep=""))
    save(partyfactsdataset,file=paste(find.package("paRtyids"),"/data/partyfactsdata.RDATA",sep=""))
  }
  partyids <- data.frame(as.vector(ids))
  partyids$mergeorder <- as.integer(rownames(partyids))

  if(missing(from) & missing(to)) {
    stop("You need to either set a value for 'from' or 'to' or both")
  }

  if(missing(from)) {
    from <- "partyfacts"
  }

  if(missing(to)) {
    to <- "partyfacts"
  }

  if(from==to) {
    stop("'from' and 'to' cannot be the same dataset")
  }


  if(from=="partyfacts") {
    names(partyids)[1] <- "partyfacts_id"
    returnvar <- "dataset_party_id"
    partyfactsdataset_reduced <- partyfactsdataset[partyfactsdataset$dataset_key==to,]
    merged <- merge(partyids,partyfactsdataset_reduced,all.x=T)
    merged <- merged[order(merged$mergeorder),]
    merged$mergeorder <- NULL
    requestedvars <- merged[,returnvar]
  }

  if(to=="partyfacts") {
    names(partyids)[1] <- "dataset_party_id"
    returnvar <- "partyfacts_id"
    partyfactsdataset_reduced <- partyfactsdataset[partyfactsdataset$dataset_key==from,]
    merged <- merge(partyids,partyfactsdataset_reduced,all.x=T)
    merged <- merged[order(merged$mergeorder),]
    merged$mergeorder <- NULL
    requestedvars <- merged[,returnvar]
  }

  if(from!="partyfacts" & to!="partyfacts") {
    names(partyids)[1] <- "dataset_party_id"
    returnvar <- "dataset_party_id"

    partyfactsdataset_reduced <- partyfactsdataset[partyfactsdataset$dataset_key==from,]
    merged <- merge(partyids,partyfactsdataset_reduced,all.x=T)
    partyids <- merged[order(merged$mergeorder),c("mergeorder","partyfacts_id")]

    partyfactsdataset_reduced <- partyfactsdataset[partyfactsdataset$dataset_key==to,]
    merged <- merge(partyids,partyfactsdataset_reduced,all.x=T)
    merged <- merged[order(merged$mergeorder),]
    merged$mergeorder <- NULL
    requestedvars <- merged[,returnvar]
    print("HERE SHOULD BE A WARNING")
  }

  requestedvars <- as.integer(requestedvars)
  return(requestedvars)
}

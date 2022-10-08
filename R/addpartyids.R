#' Add party ids from any of the data sets included in the partyfacts project if you have their partyfacts ids or vice versa
#'
#' Returns party IDs
#' @param ids A single id or vector of ids from the Party Facts data set.
#' @param from which dataset do you have IDs from?
#' @param to which dataset do you need IDs for/
#' @examples
#' not done yet
#'
#'
#'@export
addpartyids <- function(ids,
                          from = c("partyfacts","manifesto","parlgov","wikipedia","ches","clea","ejpr","vparty","wptags",
                                   "chisols","dpi","essprtc","essprtv","huber","kitschelt","polcon","ppmd",
                                   "whogov","wvs","afrelec","afro","ccdd","ccs","coppedge","cses","elecglob",
                                   "epac","hix","erdda","euandi","euned","gloelec","gpd","gps","ipod","janda",
                                   "jw","kurep","laeda","latino","laverhunt","leadglob","mackie","mapp","morgan",
                                   "mudde","nped","parlspeech","postyug","pip","poppa","ppdb","ppla","ppmdall","ray",
                                   "tap","populist","voteview","ees14","ppepe","thomas"),
                          to = c("partyfacts","manifesto","parlgov","wikipedia","ches","clea","ejpr","vparty","wptags",
                                   "chisols","dpi","essprtc","essprtv","huber","kitschelt","polcon","ppmd",
                                   "whogov","wvs","afrelec","afro","ccdd","ccs","coppedge","cses","elecglob",
                                   "epac","hix","erdda","euandi","euned","gloelec","gpd","gps","ipod","janda",
                                   "jw","kurep","laeda","latino","laverhunt","leadglob","mackie","mapp","morgan",
                                   "mudde","nped","parlspeech","postyug","pip","poppa","ppdb","ppla","ppmdall","ray",
                                   "tap","populist","voteview","ees14","ppepe","thomas")) {

  # check if partyfacts dataset is already downloaded, if not do so
  if (file.exists(paste(find.package("paRtyids"),"/data/partyfactsdata.RDATA",sep=""))) {
    load(paste(find.package("paRtyids"),"/data/partyfactsdata.RDATA",sep=""))
  } else {
    message("Downloading partyfacts dataset (this might take a few seconds)")
    partyfactsdataset <- read.csv("https://partyfacts.herokuapp.com/download/external-parties-csv/")
    dir.create(paste(find.package("paRtyids"),"/data",sep=""))
    save(partyfactsdataset,file=paste(find.package("paRtyids"),"/data/partyfactsdata.RDATA",sep=""))
  }
  partyids <- data.frame(as.vector(ids))
  partyids$mergeorder <- as.integer(rownames(partyids))

  if(missing(from) & missing(to)) {
    stop("You need to either set a value for 'from' or 'to' or both. If you only set a value for one of them, partyfacts is used as the other dataset")
  }
  if(!from %in% c("partyfacts","manifesto","parlgov","wikipedia","ches","clea","ejpr","vparty","wptags",
                  "chisols","dpi","essprtc","essprtv","huber","kitschelt","polcon","ppmd",
                  "whogov","wvs","afrelec","afro","ccdd","ccs","coppedge","cses","elecglob",
                  "epac","hix","erdda","euandi","euned","gloelec","gpd","gps","ipod","janda",
                  "jw","kurep","laeda","latino","laverhunt","leadglob","mackie","mapp","morgan",
                  "mudde","nped","parlspeech","postyug","pip","poppa","ppdb","ppla","ppmdall","ray",
                  "tap","populist","voteview","ees14","ppepe","thomas")) {

    stop(paste0("\"",from,"\" is not an available dataset. Call \"partyidsdata()\" for a list of available datasets. "))
  }
  if(!to %in% c("partyfacts","manifesto","parlgov","wikipedia","ches","clea","ejpr","vparty","wptags",
                  "chisols","dpi","essprtc","essprtv","huber","kitschelt","polcon","ppmd",
                  "whogov","wvs","afrelec","afro","ccdd","ccs","coppedge","cses","elecglob",
                  "epac","hix","erdda","euandi","euned","gloelec","gpd","gps","ipod","janda",
                  "jw","kurep","laeda","latino","laverhunt","leadglob","mackie","mapp","morgan",
                  "mudde","nped","parlspeech","postyug","pip","poppa","ppdb","ppla","ppmdall","ray",
                  "tap","populist","voteview","ees14","ppepe","thomas")) {

    stop(paste0("\"",to,"\" is not an available dataset. Call \"datasetlist()\" for a list of available datasets. "))
  }


  if(missing(from)) {
    from <- "partyfacts"
    message("No 'from' dataset was specified, so partyfacts was used as 'from' dataset")
  }

  if(missing(to)) {
    to <- "partyfacts"
    message("No 'to' dataset was specified, so partyfacts was used as 'to' dataset")
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
    message(paste0("WARNING: your request is a two-step process ("))
  }

  requestedvars <- as.integer(requestedvars)
  return(requestedvars)
}

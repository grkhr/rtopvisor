#' Get positions
#'
#' Function to get positions
#' @param user_id Your user ID, see \code{\link{https://topvisor.com/account/}}
#' @param token Your API token, see \code{\link{https://topvisor.com/account/}}
#' @param project_id Project ID
#' @param date1 Date from
#' @param date2 Date till
#' @export
#' @importFrom httr POST
#' @importFrom httr GET
#' @importFrom httr stop_for_status
#' @importFrom httr content
#' @importFrom httr add_headers
#' @importFrom rjson toJSON
#' @importFrom rjson fromJSON
#' @import data.table
#' @examples
#' TopVisorPos()


TopVisorPos <- function (user_id = NULL, token = NULL, project_id = NULL, date1 = NULL, date2 = NULL)
{
  library(data.table)
  token <- paste0("bearer ", token)
  date1 = as.character(date1)
  date2 = as.character(date2)
  
  body = toJSON(
    list(
      fields = c("id","name","site"),
      id = project_id,
      show_searchers_and_regions = "1"
    )   
  )
  add_head <- add_headers(.headers = c("Content-Type"="application/json","User-Id"=user_id,"Authorization"=token))
  answer <- POST("https://api.topvisor.com/v2/json/get/projects_2/projects",
                 body = body, add_head)
  dataRaw <- content(answer, "parsed", "application/json")
  result <- data.frame(stringsAsFactors = F)
  dataRaw <- dataRaw$result
  # if (dataRaw[[1]]$searchers[[1]]$searcher==0) {
  #   Ysearcher <- dataRaw[[1]]$searchers[[1]] 
  #   Gsearcher <- dataRaw[[1]]$searchers[[2]] 
  # } else {
  #   Ysearcher <- dataRaw[[1]]$searchers[[2]] 
  #   Gsearcher <- dataRaw[[1]]$searchers[[1]] 
  # }
  
  for (i in 1:length(dataRaw[[1]]$searchers))
    for (ii in 1:length(dataRaw[[1]]$searchers[[i]]$regions))
    {
      result <- rbind(result, c(dataRaw[[1]]$searchers[[i]]$searcher,dataRaw[[1]]$searchers[[i]]$name,dataRaw[[1]]$searchers[[i]]$regions[[ii]]$key,dataRaw[[1]]$searchers[[i]]$regions[[ii]]$name,dataRaw[[1]]$searchers[[i]]$regions[[ii]]$index), stringsAsFactors = F)
    }
  colnames(result) <- c("searcher_key","searcher_name","region_key","region_name","region_index")
  regions <- result
  regions_keys <- subset(as.data.frame(unique(as.data.table(result), by = "region_key")), select = c("region_key","region_name"))
  searchers <- subset(as.data.frame(unique(as.data.table(result), by = "searcher_key")), select = c("searcher_key","searcher_name"))
  
  datex = as.character(Sys.Date()-10000)
  datey = as.character(Sys.Date()-1)
  
  #offset = 0
  #ldr = 3
  list_of_regions <- list()
  for (i in 1:length(regions[[1]]))
  {
    list_of_regions[i] <- as.integer(regions$region_index[[i]])
  }
    body = toJSON(
      list(
        project_id = project_id,
        regions_indexes = list_of_regions,
        dates = list(datex,datey),
        show_exists_dates = "1"
      #  limit = 10000,
      #  offset = offset
      )   
    )
    
    add_head <- add_headers(.headers = c("Content-Type"="application/json","User-Id"=user_id,"Authorization"=token))
    answer <- POST("https://api.topvisor.com/v2/json/get/positions_2/history",
                   body = body, add_head)
    dataRaw <- content(answer, "parsed", "application/json")
    existDates <- dataRaw$result$existsDates
    
   
    
    if (as.integer(difftime(as.Date(existDates[[1]]),as.Date(date1))) < 0) 
    {
      exx <- as.data.frame(unlist(existDates))
      colnames(exx) <- c("name")
      existDates <- subset(existDates,as.integer(difftime(as.Date(exx[["name"]]),as.Date(date1))) >= 0)
    }
    
    
    if (length(existDates) == 0)
    {
      packageStartupMessage("DATES ARE NOT RIGHT OR NO MONEY FOR YESTERDAY", appendLF = T)
      return(NULL)
    } else {  
  result <- data.frame(stringsAsFactors = F)
  if (as.character(Sys.Date()) == existDates[[length(existDates)]]) existDates[[length(existDates)]] <- NULL
  packageStartupMessage("Processing", appendLF = F)
  for (i in 1:length(existDates))
  {
    offset = 0
    ldr = 3
  while (ldr==3)
  {    
    body = toJSON(
      list(
        fields = c("id","name","group_id","group_name"),
        project_id = project_id,
        regions_indexes = list_of_regions,
        dates = list(existDates[[i]],existDates[[i]]),
        type_range = 0,
        limit = 10000,
        offset = offset
      )   
    )
    add_head <- add_headers(.headers = c("Content-Type"="application/json","User-Id"=user_id,"Authorization"=token))
    answer <- POST("https://api.topvisor.com/v2/json/get/positions_2/history",
                   body = body, add_head)
    dataRaw <- content(answer, "parsed", "application/json")
    if (length(dataRaw)==3) offset <- dataRaw$nextOffset
    ldr <- length(dataRaw)
    dataRaw <- dataRaw$result$keywords
    for (ii in 1:length(dataRaw))
    {
      for (iii in 1:length(dataRaw[[ii]]$positionsData))
      {
        result <- unname(result)
        if (!is.null(names(dataRaw[[ii]]$positionsData[iii])))
        result <- rbind(result, c(existDates[[i]],dataRaw[[ii]]$id,dataRaw[[ii]]$name,dataRaw[[ii]]$group_id,dataRaw[[ii]]$group_name,gsub(".*:","",names(dataRaw[[ii]]$positionsData[iii])),dataRaw[[ii]]$positionsData[[iii]]),stringsAsFactors = F)
      }
    }
    packageStartupMessage(".", appendLF = F)
    
   }
  }
  colnames(result) <- c("date","id","name","group_id","group_name","region_index","position")
  
  
  # result$region_id <- as.integer(result$region_id)
  #  regions$region_key <- as.integer(regions$region_key)
  result <-merge.data.frame(result, regions, by = c("region_index"))
  result$position <- gsub("--",NA,result$position)
  packageStartupMessage(" Processed ",length(result$position)," rows", appendLF = T)
  return(result)
  
    }
}
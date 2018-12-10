#' Get volumes
#'
#' Function to get volumes
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
#' TopVisorVol()

TopVisorVol <- function (user_id = NULL, token = NULL, project_id = NULL)
{
  token <- paste0("bearer ", token)
  library(data.table)
  library(plyr)
#regions
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
  for (i in 1:length(dataRaw[[1]]$searchers))
    for (ii in 1:length(dataRaw[[1]]$searchers[[i]]$regions))
    {
      result <- rbind(result, c(dataRaw[[1]]$searchers[[i]]$searcher,dataRaw[[1]]$searchers[[i]]$name,dataRaw[[1]]$searchers[[i]]$regions[[ii]]$key,dataRaw[[1]]$searchers[[i]]$regions[[ii]]$name,dataRaw[[1]]$searchers[[i]]$regions[[ii]]$index), stringsAsFactors = F)
    }
  colnames(result) <- c("searcher_key","searcher_name","region_key","region_name","region_index")
  regions <- result
  regions_keys <- subset(as.data.frame(unique(as.data.table(result), by = "region_key")), select = c("region_key","region_name"))
  searchers <- subset(as.data.frame(unique(as.data.table(result), by = "searcher_key")), select = c("searcher_key","searcher_name"))
  upd <- data.frame(c("Yandex","Google"),c(6,3))
  colnames(upd) <- c("searcher_name","searcher_vol")
  searchers <- merge.data.frame(searchers, upd, by = c("searcher_name"))
  
#groups
  body = toJSON(
    list(
      fields = c("id","name"),
      # orders = list(
      #  name = 'date'#,
      # direction = 'DESC'
      #        ),
      # limit = 10
      
      project_id = project_id
    )   
  )
  add_head <- add_headers(.headers = c("Content-Type"="application/json","User-Id"=user_id,"Authorization"=token))
  answer <- POST("https://api.topvisor.com/v2/json/get/keywords_2/groups",
                 body = body, add_head)
  dataRaw <- content(answer, "parsed", "application/json")
  result <- data.frame(stringsAsFactors = F)
  dataRaw <- dataRaw$result
  if (length(dataRaw[1]) > 0)
  {
    column_names <- unlist(lapply(c(names(dataRaw[[1]])), 
                                  function(x) return(x)))
    
    rows <- lapply(dataRaw, function(x) return(x))
    for (rows_i in 1:length(rows)) {
      result <- rbind(result, unlist(rows[rows_i]), stringsAsFactors = F)
    }
    colnames(result) <- column_names
  }
  groups <- result
  groups_list <- as.list(unlist(groups$id))
  groups <- rbind(groups,c("666","All Groups"))
  
  packageStartupMessage("Processing", appendLF = F)
  
  #VOLUME
  result <- data.frame(stringsAsFactors = F)
  for (i in 1:length(regions_keys$region_key))
  {
    for (ii in 1: length(searchers$searcher_key))
    {
      #visibility
      offset = 0
      ldr = 3
      while (ldr==3)
      {
        
        body = toJSON(
          list(
            fields = c("id","name","group_id","group_name",paste0("volume:",regions_keys$region_key[[i]],":",searchers$searcher_key[[ii]],":",searchers$searcher_vol[[ii]])),
            project_id = project_id,
            limit = 10000,
            offset = offset
          )   
        )
        add_head <- add_headers(.headers = c("Content-Type"="application/json","User-Id"=user_id,"Authorization"=token))
        answer <- POST("https://api.topvisor.com/v2/json/get/keywords_2/keywords",
                       body = body, add_head)
        dataRaw <- content(answer, "parsed", "application/json")
        if (length(dataRaw)==3) offset <- dataRaw$nextOffset
        ldr <- length(dataRaw)
        dataRaw <- dataRaw$result
        if (length(dataRaw[1]) > 0)
        {
          
          
          rows <- lapply(dataRaw, function(x) return(x))
          for (rows_i in 1:length(rows)) {
            if (is.null(rows[[rows_i]][[length(rows[[rows_i]])]])) rows[[rows_i]][[length(rows[[rows_i]])]] <- NA
            result <- rbind(result, c(unlist(rows[rows_i]),regions_keys$region_name[[i]],searchers$searcher_name[[ii]]), stringsAsFactors = F)
          }
        }
        packageStartupMessage(".", appendLF = F)
        
      }
    }
  }
  
  
  
  
  column_names <- c("id","name","group_id","group_name","volume","region_name","searcher_name")
  colnames(result) <- column_names
  packageStartupMessage("",appendLF = T)
  packageStartupMessage(" Processed ",length(result$id)," rows", appendLF = T)
  
 return(result)
}

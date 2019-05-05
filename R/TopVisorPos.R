#' Get positions
#'
#' Function to get positions
#' @param user_id Your user ID, see \code{\link{https://topvisor.com/account/}}
#' @param token Your API token, see \code{\link{https://topvisor.com/account/}}
#' @param project_id Project ID
#' @param date1 Date from
#' @param date2 Date till
#' @export
#' @import data.table
#' @importFrom httr POST
#' @importFrom httr GET
#' @importFrom httr stop_for_status
#' @importFrom httr content
#' @importFrom httr add_headers
#' @importFrom rjson toJSON
#' @importFrom rjson fromJSON
#' @importFrom mgsub mgsub
#' @examples
#' TopVisorPos()

TopVisorPos <- function (user_id = NULL, token = NULL, project_id = NULL, date1 = NULL, date2 = NULL)
{
  st <- Sys.time()
  token <- paste0("bearer ", token)
  date1 = as.character(date1)
  date2 = as.character(date2)

  # geting searcher's and region's keys
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
    for (ii in 1:length(dataRaw[[1]]$searchers[[i]]$regions)) {
      result <- rbind(result, c(dataRaw[[1]]$searchers[[i]]$searcher,dataRaw[[1]]$searchers[[i]]$name,dataRaw[[1]]$searchers[[i]]$regions[[ii]]$key,dataRaw[[1]]$searchers[[i]]$regions[[ii]]$name,dataRaw[[1]]$searchers[[i]]$regions[[ii]]$index,dataRaw[[1]]$searchers[[i]]$regions[[ii]]$device), stringsAsFactors = F)
    }
  colnames(result) <- c("searcher_key","searcher_name","region_key","region_name","region_index","device")
  regions <- result
  regions_keys <- subset(as.data.frame(unique(as.data.table(result), by = "region_key")), select = c("region_key","region_name","device"))
  searchers <- subset(as.data.frame(unique(as.data.table(result), by = "searcher_key")), select = c("searcher_key","searcher_name"))

  # getting exist stat dates in project
  datex = as.character(Sys.Date() - 10000)
  datey = as.character(Sys.Date() - 1)

  list_of_regions <- list()
  for (i in 1:length(regions[[1]])) {
    list_of_regions[i] <- as.integer(regions$region_index[[i]])
  }
    body = toJSON(
      list(
        project_id = project_id,
        regions_indexes = list_of_regions,
        dates = list(datex,datey),
        show_exists_dates = "1"
      )
    )

    add_head <- add_headers(.headers = c("Content-Type"="application/json","User-Id"=user_id,"Authorization"=token))
    answer <- POST("https://api.topvisor.com/v2/json/get/positions_2/history",
                   body = body, add_head)
    dataRaw <- content(answer, "parsed", "application/json")
    existDates <- dataRaw$result$existsDates

    # check dates
    if (length(existDates) == 0) {
      packageStartupMessage('No stats for chosen period or some troubles with payment, check your account.')
      return(data.frame())
    }

    existDates <- lapply(existDates, as.Date)
    date1 = as.Date(date1)
    date2 = as.Date(date2)
    edsec <- seq(existDates[[1]], existDates[[length(existDates)]], by = "day")

    # cut list of existDates
    if ((date1 %in% edsec) & (date2 %in% edsec)) {
      existDates <- existDates[(existDates >= date1) & (existDates <= date2)]
    } else if (date1 %in% edsec) {
      existDates <- existDates[(existDates >= date1)]
    } else if (date2 %in% edsec) {
      existDates <- existDates[(existDates <= date2)]
    } else {
      packageStartupMessage('No stats for chosen period.')
      return(data.frame())
    }

  # check if existDates is empty
  if (length(existDates) == 0) {
    packageStartupMessage('No stats for chosen period.')
    return(data.frame())
  }
  existDates <- lapply(existDates, as.character)
  result <- data.frame(stringsAsFactors = F)
  if (length(existDates) == 0) return (result)
 # if (as.character(Sys.Date()) == existDates[[length(existDates)]]) existDates[[length(existDates)]] <- NULL
  packageStartupMessage("Ready to process the stats from ",existDates[[1]]," to ",existDates[[length(existDates)]]," for existing days.", appendLF = T)
  packageStartupMessage("Processing", appendLF = F)
  for (i in 1:length(existDates))
  {
    offset = 0
    ldr = 3
  while (ldr == 3)
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
    if (length(dataRaw) == 3) offset <- dataRaw$nextOffset
    ldr <- length(dataRaw)
    dataRaw <- dataRaw$result$keywords
    result1 <- data.frame()
    for (ii in 1:length(dataRaw))
    {
      for (iii in 1:length(dataRaw[[ii]]$positionsData))
      {
        result1 <- unname(result1)
        if (!is.null(names(dataRaw[[ii]]$positionsData[iii])))
        result1 <- rbind(result1, c(existDates[[i]],dataRaw[[ii]]$id,dataRaw[[ii]]$name,dataRaw[[ii]]$group_id,dataRaw[[ii]]$group_name, gsub(".*:", "", names(dataRaw[[ii]]$positionsData[iii])),dataRaw[[ii]]$positionsData[[iii]]),stringsAsFactors = F)
      }
    }
    colnames(result1) <- c("date","id","name","group_id","group_name","region_index","position")
    result <- rbind(result, result1)
    packageStartupMessage(".", appendLF = F)
   }
  }
  colnames(result) <- c("date","id","name","group_id","group_name","region_index","position")

  result <- merge.data.frame(result, regions, by = c("region_index"))
  result$device <- mgsub(result$device, list(0,1,2), list("Desktop","Tablet","Mobile"))
  result$position <- gsub("--", NA, result$position)
  packageStartupMessage("", appendLF = T)
  packageStartupMessage("Processed ", length(result$position), " rows for ", length(existDates), " existing days. ", round(as.numeric(Sys.time() - st), 2), " seconds elapsed.", appendLF = T)
  return(result)
}

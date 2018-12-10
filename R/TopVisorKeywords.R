#' Get all keywords
#'
#' Function to get keywords
#' @param user_id Your user ID, see \code{\link{https://topvisor.com/account/}}
#' @param token Your API token, see \code{\link{https://topvisor.com/account/}}
#' @param project_id Project ID
#' @export
#' @importFrom httr POST
#' @importFrom httr GET
#' @importFrom httr stop_for_status
#' @importFrom httr content
#' @importFrom httr add_headers
#' @importFrom rjson toJSON
#' @importFrom rjson fromJSON
#' @examples
#' TopVisorKeywords()


TopVisorKeywords <- function (user_id = NULL, token = NULL, project_id = NULL)
{
  token <- paste0("bearer ", token)
  offset = 0
  ldr = 3
  result <- data.frame(stringsAsFactors = F)
  packageStartupMessage("Processing", appendLF = F)
  
  while (ldr==3)
  {
    
    body = toJSON(
      list(
        fields = c("id","name","group_id","group_name"),
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
        result <- rbind(result, unlist(rows[rows_i]), stringsAsFactors = F)
      }
    }
    packageStartupMessage(".", appendLF = F)
    
  }
  column_names <- unlist(lapply(c(names(dataRaw[[1]])), 
                                function(x) return(x)))
  colnames(result) <- column_names
  packageStartupMessage(" Processed ",length(result$id)," rows", appendLF = T)
  return(result)
  
}
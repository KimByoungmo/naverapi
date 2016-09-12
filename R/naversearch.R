#' @title naverblog.search
#' @description Searching Naver blog with using Naver Open API
#' @author Kim BM(ksd2080@hotmail.com)
#' @docType package
#' @name naverblog.search
#' @export
#' @import httr
#' @importFrom XML xmlRoot xpathSApply htmlTreeParse xmlValue xmlToDataFrame
#' @details This is a package which use Naver Open API to search naver blog You must need Naver client ID and secret
#' @param keyword keyword you want to search
#' @param client_Id Naver Open API Client ID
#' @param client_secret Naver Open API Client secret
#' @param category category whcich you want to search (e.g : blog, news, region,.....)
#' @param display the number of search result
#' @param start start number of search result
#' @param sort sim : similarity, date : ordered by date
#' @param genre moive genre (when you wnat to search moives, it's a optional variable)
#' @param country  movie made in where? (when you wnat to search moives, it's a optional variable)
#' @param yearfrom movie searching period
#' @param yearto movie searching period
#' @examples naver.search(keyword = 'andong', client_Id = 'DpMY9iw4AztbeSxYXf2t', client_secret = '69Az0lv56T')

naver.search <- function(keyword = "", client_Id = "", client_secret = "", category = c('blog','news','region','encyclopedia','movie'),
  display = 100, start = 1, sort = "sim", genre = "", country = "", yearfrom = NULL, yearto = NULL)
  {
  cc <- match.arg(category, several.ok = F)
  # url setting
  url <- url_list[cc]
  v <- request_variable[request_variable$Category == cc,2]

    # start#
  if('header' %in% v) {
  h <- c('X-Naver-Client-Id' = client_Id, 'X-Naver-Client-Secret' = client_secret)  #GET header setting
  }

  if('query' %in% v) {
    qq<- c('query' = enc2utf8(keyword))

  }

  if('display' %in% v) {
  temp <- c('display' = display)
  try(qq <- append(qq ,temp), silent = T)
  }

  if('start' %in% v) {
    temp <- c('start' = start)
    try(qq <- append(qq, temp), silent = T)
  }

  if('genre' %in% v) {
    temp <- c('genre' = genre)
    try(qq <- append(qq,temp), silent = T)
  }

  if('country' %in% v) {
    temp <- c('country' = country)
    try(qq <- append(qq,temp), silent = T)
  }

  if('yearfrom' %in% v) {
      temp <- c('yearfrom' = yearfrom)
      try(qq <- append(qq,temp), silent = T)
    }


  if('yearto' %in% v) {
      temp <- c('yearto' = yearfrom)
      try(qq <- append(qq,temp), silent = T)
  }

  if('sort' %in% v) {
    temp <- c('sort' = sort)
    try(qq <- append(qq,temp), silent = T)
  }
  qq <- as.list(qq)

  result <- GET(url = url, add_headers(h), query = qq)

  # Checking http status code #
  if (result$status_code != 200)
  {
    if (result$status_code == 400)
    {
      stop("Check request variables \n at least, one variable is omitted.",
        call. = F)
    } else if (result$status_code == 401)
    {
      stop("Check Client ID or Client Secret.", call. = F)
    }
    else if (result$status_code == 500)
    {
      stop("System Error.", call. = F)
    }
  } else
  {
    result <- xmlRoot(xmlTreeParse(result, useInternalNodes = T,
      getDTD = F, encoding = "UTF-8"))
    result <- getNodeSet(doc = result,path = '//item')
    result <- xmlToDataFrame(doc = result, stringsAsFactors = F)
    for(i in 1:length(result)) {
    result[,i]<- gsub('<b>','',result[,i])
    result[,i] <- gsub('</b>','',result[,i])
}

    result
    }
  }


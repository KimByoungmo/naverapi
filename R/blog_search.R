#' @title naverblog.search
#' @description Searching Naver blog with using Naver Open API
#' @author Kim BM(ksd2080@hotmail.com)
#' @docType package
#' @name naverblog.search
#' @export
#' @import httr
#' @importFrom XML xmlRoot xpathSApply htmlTreeParse xmlValue
#' @details This is a package which use Naver Open API to search naver blog You must need Naver client ID and secret

naverblog.search <- function(keyword = '', client_Id = '', client_secret = '', display = 100, start = 2, sort = 'sim') {

   #checking function arguments
   args <- names(as.list(match.call(expand.dots = T)[-1]))

   if(!is.character(keyword)) stop("'keywords' is needed")
   if(!is.character(arg['client_Id'])) stop("Client_Id is needed\n you can get Client ID in https://developers.naver.com/main if you register")
   if(!is.character(arg['client_secret'])) stop('Client_secret is needed\n you can get Client ID in https://developers.naver.com/main if you register')

  url = 'https://openapi.naver.com/v1/search/blog.xml' #naver search api url

  set_config(config(ssl_verifypeer = 0L)) #SSL non-verification setting

  h <<- c('X-Naver-Client-Id'= client_Id, 'X-Naver-Client-Secret' = client_secret) #GET header setting
  q <- list (query = enc2utf8(keyword), display = as.numeric(display), start = as.numeric(start), sort = sort) #GET parameter setting

  result <- xmlRoot(htmlTreeParse(GET(url = url, add_headers(h), query = q), useInternalNodes = T, getDTD = F, encoding = 'UTF-8'))
  result <- xpathSApply(doc = result, path = '//description', xmlValue)

  result
  }


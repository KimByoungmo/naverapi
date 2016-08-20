#' @title naverblog.search
#' @description Searching Naver blog with using Naver Open API
#' @author Kim BM(ksd2080@hotmail.com)
#' @docType package
#' @name naverblog.search
#' @export
#' @import httr
#' @importFrom XML xmlRoot xpathSApply htmlTreeParse xmlValue
#' @details This is a package which use Naver Open API to search naver blog You must need Naver client ID and secret

naverblog.search <- function(keyword = "", client_Id = "", client_secret = "",
  display = 100, start = 1, sort = "sim") {

  # checking function arguments
  args <- names(as.list(match.call(expand.dots = T)[-1]))

  if ("keyword" %in% args) {
    if (!is.character(keyword) | length(keyword) != 1)
      stop("'keywords' is needed \n keyword must have one length",
        call. = F)
  }

  if ("cilent_Id" %in% args) {
    if (!is.character(client_Id) | length(client_Id) != 1)
      stop("Client_Id is needed\n you can get Client ID in https://developers.naver.com/main if you register",
        call. = F)
  }

  if ("cilent_secret" %in% args) {
    if (!is.character(client_secret) | length(client_secret !=
      1))
      stop("Client_secret is needed\n you can get Client ID in https://developers.naver.com/main if you register",
        call. = F)
  }

  if ("display" %in% args) {
    if (!is.numeric(display) | display > 100)
      stop("Cannot display more than 100 results", call. = F)
  }

  if ("sort" %in% args) {
    if (!(sort %in% c("sim", "date")))
      stop("sim = similarity \n date = ordered by date",
        call. = F)
  }

  # start#
  url = "https://openapi.naver.com/v1/search/blog.xml"  #naver search api url

  set_config(config(ssl_verifypeer = 0L))  #SSL non-verification setting

  h <- c(`X-Naver-Client-Id` = client_Id, `X-Naver-Client-Secret` = client_secret)  #GET header setting
  q <- list(query = enc2utf8(keyword), display = as.numeric(display),
    start = as.numeric(start), sort = sort)  #GET parameter setting
  result <- GET(url = url, add_headers(h), query = q)
  result <- xmlRoot(htmlTreeParse(result), useInternalNodes = T,
    getDTD = F, encoding = "UTF-8")
  result <- xpathSApply(doc = result, path = "//description",
    xmlValue)

  result[-1]
}


#' @title naverVoice
#' @description Make a voice synthesis using Naver Open API
#' @author Kim BM(ksd2080@hotmail.com)
#' @name naverVoice
#' @export
#' @import httr
#' @importFrom XML xmlRoot xpathSApply htmlTreeParse xmlValue xmlToDataFrame
#' @details It helps you to make a various voice synthesis using Naver Open API
#' @import tuneR
#' @param client_Id Naver Open API Client ID
#' @param client_secret Naver Open API Client secret
#' @param speaker speaker whose voice is composed'
#' refer to below
#' majin : Korean woman, jinhi : Korean Man, clara : American woman, matt : American man, yuri : Japanses woman, shinji : Japanese man, meimei : Chinese woman
#' @examples naverVoice('best','DpMY9iw4AztbeSxYXf2t','69Az0lv56T')

naverVoice <-
  function(text = '',
           client_Id = '',
           client_secret = '',
           speaker = c('mijin', 'jinho', 'clara', 'matt', 'yuri', 'shinji', 'meimei'),
           speed = 0) {
    #make POST request variables
    url = paste0('https://openapi.naver.com/v1/voice/tts.bin', '')
    h <-
      c('X-Naver-Client-Id' = client_Id,
        'X-Naver-Client-Secret' = client_secret)
    s <- match.arg(speaker, several.ok = F)
    q <- list (speaker = s,
               speed = speed,
               text = enc2utf8(text))
    result <-
      POST(
        url = url,
        body = q,
        config = add_headers(h),
        encode = 'form'
      )
    result <- content(result)
    play(.Call('C_do_read_mp3', result, PACKAGE = 'tuneR'))
  }

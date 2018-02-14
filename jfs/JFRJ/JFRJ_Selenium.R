library(RSelenium)
library(rvest)
library(dplyr)
library(stringr)

query <- "0188584-86.2017.4.02.5101"

busca <- function(query){
  # Digitando o num do processo
  webElem1 <- remDr$client$findElement(using = "name", value = "NumProc")
  webElem1$highlightElement()
  webElem1$sendKeysToElement(list(query))
  
  # Obtendo informaçoes sobre o captcha
  webElem2 <- remDr$client$findElement(using = "xpath", '//*[@id="ConsProc"]/table/tbody/tr[3]/td/table[1]/tbody/tr[12]/td/font/span/b[2]')
  captcha <- webElem2$getElementText() %>% unlist()
  webElem3 <- remDr$client$findElement(using = "xpath", '//*[@id="ConsProc"]/table/tbody/tr[3]/td/table[1]/tbody/tr[12]/td/font/span[text()[3]]')
  pergunta <- webElem3$getElementText()  %>% str_split(pattern = "\n") %>% unlist() %>% .[5]
  
  # Resolvendo captcha
  gabarito <- as.character(jfrj_captcha(pergunta, captcha))
  
  if(gabarito == "Nenhum"){
    webElem4 <- remDr$client$findElement(using = "name", 'nenhum')
    webElem4$clickElement()
  } else{
    webElem4 <- remDr$client$findElement(using = "name", 'captchacode')
    webElem4$highlightElement()
    webElem4$sendKeysToElement(list(gabarito))  
  }
  
  # Acessando a busca
  webElem5 <- remDr$client$findElement(using = "name", 'Pesquisar')
  webElem5$clickElement()
}


# EM ANDAMENTO

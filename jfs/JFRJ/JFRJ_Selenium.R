library(RSelenium)
library(rvest)
library(dplyr)
library(stringr)

query <- "0188584-86.2017.4.02.5101"

get_source <- function(name_element){
  WebElem <- remDr$client$findElement(using = 'name', name_element)
  WebElem$clickElement()
}

busca <- function(query){
  # Digitando o num do processo
  remDr <- RSelenium::rsDriver()
  remDr$client$navigate("http://procweb.jfrj.jus.br/portal/consulta/cons_procs.asp")
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
  
  #Obtendo informacoes do processo
  ## Dados basicos
  dados_basicos <- remDr$client$getPageSource()
  
  ## Movimento
  WebElem <- remDr$client$findElement(using = 'name', 'imMV')
  WebElem$clickElement()
  movimento <- remDr$client$getPageSource()
  
  ## Dados adicionais
  WebElem <- remDr$client$findElement(using = 'name', 'imCO')
  WebElem$clickElement()
  dados_adicionais <- remDr$client$getPageSource()
  
  ## Processos Vinculados
  WebElem <- remDr$client$findElement(using = 'name', "imPV")
  WebElem$clickElement()
  proc_vinculados <- remDr$client$getPageSource()
  
  ## Partes
  WebElem <- remDr$client$findElement(using = 'name', "imPT")
  WebElem$clickElement()
  partes <- remDr$client$getPageSource()
  
  ## Pecas
  WebElem <- remDr$client$findElement(using = 'name', "imPE")
  WebElem$clickElement()
  pecas <- remDr$client$getPageSource()
  
  ## Recursos
  WebElem <- remDr$client$findElement(using = 'name', "imRE")
  WebElem$clickElement()
  recursos <- remDr$client$getPageSource()
  
  ## Acessos
  WebElem <- remDr$client$findElement(using = 'name', "imAC")
  WebElem$clickElement()
  acessos <- remDr$client$getPageSource()
  
  ## Peticoes nao juntadas
  WebElem <- remDr$client$findElement(using = 'name', "imPJ")
  WebElem$clickElement()
  peticoes_n_juntadas <- remDr$client$getPageSource()
  
  info <- list(dados_adicionais,
               proc_vinculados,
               partes,
               pecas,
               recursos,
               acessos,
               peticoes_n_juntadas)
  return(info)
}


# Parseando
infos <- busca(query)

# Quebra captcha
library(stringr)
library(dplyr)

# Existem dois padroes para resolver esse captcha, o primeiro deles são as palavras "Quantos/Quais"
# e o segundo são as palavras números/consoantes/vogais.

# Quando o primeiro padrão é QUANTOS a resposta deve ser dada em algorismos
# Quando o primeiro padrão é QUAIS a resposta de ser dada com as letras que aparecem sem separação(ex. FT ou 54)
# Quando nenhum deles confere é preciso clicar em um botão presente no site

# O resultado da funcao devera ser colocado no 'captchacode' do site

jfrj_captcha <- function(pergunta, captcha){
  
  # Definindo as respostas
  consoantes <- LETTERS[!(LETTERS %in% c("A", "E", "I", "O", "U"))]
  
  vogais <- LETTERS[LETTERS %in% c("A", "E", "I", "O", "U")]
  
  algarismo <- as.character(0:9)
  
  # Padronização da pergunta
  perguntas_upper <- stringr::str_to_upper(pergunta)
  
  # Separando os elementos do captcha
  captcha_split <- captcha %>% 
    str_split(" ") %>%
    unlist()
  
  if(stringr::str_detect(perguntas_upper, "QUANTOS")==T){
    if(stringr::str_detect(perguntas_upper, "NÚMEROS")== T){
      
      quantos <- length(captcha_split[captcha_split %in% algarismo])
      if (quantos == 0){
        quantos <- "Nenhum"
      }
      return(quantos)
      
    } else if(stringr::str_detect(perguntas_upper, "CONSOANTES")== T){
      
      quantos <- length(captcha_split[captcha_split %in% consoantes])
      if (quantos == 0){
        quantos <- "Nenhum"
      }
      return(quantos)
      
    } else if(stringr::str_detect(perguntas_upper, "VOGAIS")== T){
      
      quantos <- length(captcha_split[captcha_split %in% vogais])
      if (quantos == 0){
        quantos <- "Nenhum"
      }
      return(quantos)
      
    } else {
      print("Nenhum")
    }
  } else if(stringr::str_detect(perguntas_upper, "QUAIS")==T){
    if(stringr::str_detect(perguntas_upper, "NÚMEROS")== T){
      
      quais <- captcha_split[which(captcha_split %in% algarismo)]
      quais <- str_c(quais, collapse = "")
      if(length(quais)==0){
        quais <- "Nenhum"
      }
      return(quais)
      
    } else if(stringr::str_detect(perguntas_upper, "CONSOANTES")== T){
      
      quais <- captcha_split[which(captcha_split %in% consoantes)]
      quais <- str_c(quais, collapse = "")
      if(length(quais)==0){
        quais <- "Nenhum"
      }
      return(quais)
      
    } else if(stringr::str_detect(perguntas_upper, "VOGAIS")== T){
      
      quais <- captcha_split[which(captcha_split %in% vogais)]
      quais <- str_c(quais, collapse = "")
      if(length(quais)==0){
        quais <- "Nenhum"
      }
      return(quais)
      
    } else {
      print("Nenhum")
    }
  }
}



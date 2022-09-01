#### webscraping da transcricao pelo Aos Fatos do debate presidencial da band 2022

library(tidyverse)
library(httr)
library(zoo)

get_data <- function(){
  # pagina original
  #'https://www.aosfatos.org/noticias/checamos-debate-presidencial-band-uol-folha-cultura/'
  
  #como encontrei o arquivo com os dados já estruturados,
  # vou pega-los direto nessa request, sem precisar fazer webscraping
  request <- httr::GET('https://docs.google.com/spreadsheets/d/1uYDq7bN--kxPImL2R1LWnkmj-f768gHJZGH_7s981ZY/gviz/tq?tqx=out:csv')
  
  #verificando status da request
  print(request$status_code)
  
  #pegando o content com apenas as colunas que importam nessa analise
  df_respostas <- 
    request %>% 
    content() %>% 
    select(1:5)
  
  #arrumando dataset
  df_respostas <- 
    df_respostas %>%
          #preenchedo os nan de forma que faca sentido na tabela
    mutate(pessoa = na.locf(pessoa),
           #padronizando nomes em uppercase
           pessoa = toupper(pessoa))
    
  
  #salvando df em arquivo
  df_respostas %>% 
    write_delim('data/dados_debate_presidencial_2022_08_28.csv',
                delim = ';')
  
  print('Sucesso na extracao dos dados ;)')
}

#chamando funcao principal
get_data()

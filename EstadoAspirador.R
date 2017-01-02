source("Estado.R")

## Classe e metodos para o problema do Aspirador de po 

Aspirador <- function(desc = NULL, pai = NULL){
  
  e <- environment() ##ambiente
  
  assign("posicao", posicao, envir = e) ##mostra qual quadrado do cenario eh
  assign("desc", desc, envir = e) ##sera o número de quadrados sujos no estado
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e) ##inicializa com zero
  assign("h", Inf, envir = e) ##inicializa com valor invalido = infinito
  assign("f", Inf, envir = e) ##inicializa com valor invalido = infinito
  
  class(e) <- c("Aspirador", "Estado")
  
  return(e)
}


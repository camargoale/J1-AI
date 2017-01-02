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

## Sobrecarregando o operador "==" para comparação entre estados
Ops.Aspirador = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica "print" do R
print.Aspirador <- function(obj) {
  cat("Posicao no cenario: (quadrado)", obj$posicao, "\n") ## posicao
  cat("Numero de quadrados sujos remanescentes: (", obj$desc, ")\n") 
  cat("G(n): ", obj$g, "(custo)\n") ## custo
  cat("H(n): ", obj$h, "(heuristica)\n") ##heuristica
  cat("F(n): ", obj$f, "(funcao de avaliacao)\n") ##resultado
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
## Será o número de quadrados sujos remanescentes limpando-se o quadrado atual (se estiver sujo)
heuristica.Aspirador <- function(atual){
  if (atual$desc == 0){
    return(0)
  }
  else{
    return (atual$desc - 1) ##considera que o quadrado atual (indicado pela acao) sera limpo
  }
}


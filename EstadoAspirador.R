source("Estado.R")

## Classe e metodos para o problema do Aspirador de po 

Aspirador <- function(desc = NULL, pai = NULL){
  
  e <- environment() ##ambiente
  
  assign("posicao", 0, envir = e) ##mostra qual quadrado do cenario eh
  assign("desc", 0, envir = e) ##sera o número de quadrados sujos no estado
  assign("pai", NULL, envir = e)
  assign("contexto", 0, envir = e) ##contexto atual para descobrir se o estado atual eh sujo ou nao
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
  cat("Contexto do estado: ", obj$contexto, "\n") ## contexto do estado 
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

geraFilhos.Aspirador <- function(obj) { ##obj para obter a posicao atual e atual para obter o cenario atual
  filhos <- list()
  if (obj$posicao == 1){ ##se estou na posicao 1
    if (is.null(obj$pai)){
      ##eh o estado inicial, nao tem estado pai 
      ##operacao "direita"
      filho <- Aspirador()
      filho$posicao <- (obj$posicao + 1)
      filho$desc <- obj$desc
      filho$pai <- obj
      filho$contexto <- obj$contexto
      filho$g <- obj$posicao
      filho$h <- heuristica(filho)
      filhos <- c(filhos, list(filho))
      ##operacao "baixo"
      filho <- Aspirador()
      filho$posicao <- (obj$posicao + 2)
      filho$desc <- obj$desc
      filho$pai <- obj
      filho$contexto <- obj$contexto
      filho$g <- 3
      filho$h <- heuristica(filho)
      filhos <- c(filhos, list(filho))
    }
    else if(obj$pai$posicao == 2){ 
      ##se veio da direita (2) então não pode voltar para la, nao gera este estado
      ##operacao "baixo"
      filho <- Aspirador()
      filho$posicao <- (obj$posicao + 2)
      filho$desc <- obj$desc
      filho$pai <- obj
      filho$contexto <- obj$contexto
      filho$g <- 3
      filho$h <- heuristica(filho)
      filhos <- c(filhos, list(filho))					
    }
    else{
      ##se veio de baixo (3) entao nao pode voltar, nao gera este estado
      ##operacao "direita"
      filho <- Aspirador()
      filho$posicao <- (obj$posicao + 1)
      filho$desc <- obj$desc
      filho$pai <- obj	
      filho$contexto <- obj$contexto
      filho$g <- 1
      filho$h <- heuristica(filho)
      filhos <- c(filhos, list(filho))				
    }
  }
  if (obj$posicao == 2){ ##se estou na posicao 2
    if (is.null(obj$pai)){
      ##eh o estado inicial, nao tem estado pai 
      ##operacao "esquerda"
      filho <- Aspirador()
      filho$posicao <- (obj$posicao - 1)
      filho$desc <- obj$desc
      filho$pai <- obj
      filho$contexto <- obj$contexto
      filho$g <- obj$posicao
      filho$h <- heuristica(filho)
      filhos <- c(filhos, list(filho))
      ##operacao "baixo"
      filho <- Aspirador()
      filho$posicao <- (obj$posicao + 2)
      filho$desc <- obj$desc
      filho$pai <- obj
      filho$contexto <- obj$contexto
      filho$g <- 3
      filho$h <- heuristica(filho)
      filhos <- c(filhos, list(filho))
    }
    else if(obj$pai$posicao == 1){ 
      ##se veio da esquerda (1) então não pode voltar para la, nao gera este estado
      ##operacao "baixo"
      filho <- Aspirador()
      filho$posicao <- (obj$posicao + 2)
      filho$desc <- obj$desc
      filho$pai <- obj
      filho$contexto <- obj$contexto
      filho$g <- 3
      filho$h <- heuristica(filho)
      filhos <- c(filhos, list(filho))					
    }
    else{
      ##se veio de baixo (4) entao nao pode voltar, nao gera este estado
      ##operacao "esquerda"
      filho <- Aspirador()
      filho$posicao <- (obj$posicao - 1)
      filho$desc <- obj$desc
      filho$pai <- obj	
      filho$contexto <- obj$contexto
      filho$g <- 1
      filho$h <- heuristica(filho)
      filhos <- c(filhos, list(filho))				
    }
  }
  if (obj$posicao == 3){ ##se estou na posicao 3
    if (is.null(obj$pai)){
      ##eh o estado inicial, nao tem estado pai 
      ##operacao "direita"
      filho <- Aspirador()
      filho$posicao <- (obj$posicao + 1)
      filho$desc <- obj$desc
      filho$pai <- obj
      filho$contexto <- obj$contexto
      filho$g <- obj$posicao
      filho$h <- heuristica(filho)
      filhos <- c(filhos, list(filho))
      ##operacao "cima"
      filho <- Aspirador()
      filho$posicao <- (obj$posicao - 2)
      filho$desc <- obj$desc
      filho$pai <- obj
      filho$contexto <- obj$contexto
      filho$g <- 3
      filho$h <- heuristica(filho)
      filhos <- c(filhos, list(filho))
    }
    else if(obj$pai$posicao == 4){ 
      ##se veio da direita (4) então não pode voltar para la, nao gera este estado
      ##operacao "cima"
      filho <- Aspirador()
      filho$posicao <- (obj$posicao - 2)
      filho$desc <- obj$desc
      filho$pai <- obj
      filho$contexto <- obj$contexto
      filho$g <- 3
      filho$h <- heuristica(filho)
      filhos <- c(filhos, list(filho))					
    }
    else{
      ##se veio de cima (1) entao nao pode voltar, nao gera este estado
      ##operacao "direita"
      filho <- Aspirador()
      filho$posicao <- (obj$posicao + 1)
      filho$desc <- obj$desc
      filho$pai <- obj
      filho$contexto <- obj$contexto
      filho$g <- 1
      filho$h <- heuristica(filho)
      filhos <- c(filhos, list(filho))				
    }
  }	
  if (obj$posicao == 4){ ##se estou na posicao 4
    if (is.null(obj$pai)){
      ##eh o estado inicial, nao tem estado pai 
      ##operacao "esquerda"
      filho <- Aspirador()
      filho$posicao <- (obj$posicao - 1)
      filho$desc <- obj$desc
      filho$pai <- obj
      filho$contexto <- obj$contexto
      filho$g <- obj$posicao
      filho$h <- heuristica(filho)
      filhos <- c(filhos, list(filho))
      ##operacao "cima"
      filho <- Aspirador()
      filho$posicao <- (obj$posicao - 2)
      filho$desc <- obj$desc
      filho$pai <- obj
      filho$contexto <- obj$contexto
      filho$g <- 3
      filho$h <- heuristica(filho)
      filhos <- c(filhos, list(filho))
    }
    else if(obj$pai$posicao == 3){ 
      ##se veio da esquerda (3) então não pode voltar para la, nao gera este estado
      ##operacao "cima"
      filho <- Aspirador()
      filho$posicao <- (obj$posicao - 2)
      filho$desc <- obj$desc
      filho$pai <- obj
      filho$contexto <- obj$contexto
      filho$g <- 3
      filho$h <- heuristica(filho)
      filhos <- c(filhos, list(filho))					
    }
    else{
      ##se veio de cima (2) entao nao pode voltar, nao gera este estado
      ##operacao "esquerda"
      filho <- Aspirador()
      filho$posicao <- (obj$posicao - 1)
      filho$desc <- obj$desc
      filho$pai <- obj
      filho$contexto <- obj$contexto
      filho$g <- 1
      filho$h <- heuristica(filho)
      filhos <- c(filhos, list(filho))				
    }
  }	
  if (obj$contexto[obj$posicao] == 1) {##se o quadrado esta sujo oferece o estado alcancado pela operacao limpar
    ##operacao "limpar"
    filho <- Aspirador()
    filho$posicao <- obj$posicao
    filho$desc <- obj$desc - 1
    filho$pai <- obj$pai
    filho$contexto <- obj$contexto
    filho$contexto[obj$posicao] <- 0
    filho$g <- (obj$g + 2)
    filho$h <- heuristica(filho)
    filhos <- c(filhos, list(filho))
  }
  return(filhos)
}

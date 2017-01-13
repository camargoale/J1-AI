source("Estado.R")

## Classe e metodos para o problema do aspirador de po 
Aspirador <- function(desc = NULL, pai = NULL){
  
  e <- environment()
  
  assign("desc", desc, envir = e) ##descricao
  assign("pai", pai, envir = e) ##estado de origem
  assign("g", 0, envir = e) ##custo
  assign("h", Inf, envir = e) ##heuristica
  assign("f", Inf, envir = e) ##funcao de avaliacao
  
  class(e) <- c("Aspirador", "Estado")
  
  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.Aspirador = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc[1:4] == obj2$desc[1:4]))
  }
}

## Sobrecarga da função genérica "print" do R
print.Aspirador <- function(obj) {
  cat("(q1,q2,q3,q4,P): (", obj$desc, ")\n") ## vetor representando a descricao do estado, com o estado de todos os quadrados e o estado atual na ultima posicao
  cat("G(n): ", obj$g, "(custo)\n") ## custo
  cat("H(n): ", obj$h, "(heuristica)\n") ## heuristica
  cat("F(n): ", obj$f, "(funcao de avaliacao)\n") ## resultado
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
## Será o número de quadrados sujos remanescentes 
heuristica.Aspirador <- function(atual){
  if(is.null(atual$desc)){
    return(Inf)
}
  return(sum(atual$desc[1:4])) ## heuristica = a soma de dos contextos de todos os quadrados 
}

geraFilhos.Aspirador <- function(obj) { 
	filhos <- list()
	if (obj$desc[5] == 1){ ##se estou na posicao 1
		##operacao "direita"
		filho <- Aspirador()
		filho$desc <- obj$desc
		filho$desc[5] <- 2
		filho$pai <- obj
		filho$g <- (obj$g + 1)
		filho$h <- heuristica.Aspirador(filho)
		filhos <- c(filhos, list(filho))
		##operacao "baixo"
		filho <- Aspirador()
		filho$desc <- obj$desc
		filho$desc[5] <- 3
		filho$pai <- obj
		filho$g <- (obj$g + 3)
		filho$h <- heuristica.Aspirador(filho)
		filhos <- c(filhos, list(filho))
	}
	else if (obj$desc[5] == 2){ ##se estou na posicao 2
		##operacao "esquerda"
		filho <- Aspirador()
		filho$desc <- obj$desc
		filho$desc[5] <- 1
		filho$pai <- obj
		filho$g <- (obj$g + 1)
		filho$h <- heuristica.Aspirador(filho)
		filhos <- c(filhos, list(filho))
		##operacao "baixo"
		filho <- Aspirador()
		filho$desc <- obj$desc
		filho$desc[5] <- 4
		filho$pai <- obj
		filho$g <- (obj$g + 3)
		filho$h <- heuristica.Aspirador(filho)
		filhos <- c(filhos, list(filho))
    }
	else if (obj$desc[5] == 3){ ##se estou na posicao 3
		##operacao "direita"
		filho <- Aspirador()
		filho$desc <- obj$desc
		filho$desc[5] <- 4
		filho$pai <- obj
		filho$g <- (obj$g + 1)
		filho$h <- heuristica.Aspirador(filho)
		filhos <- c(filhos, list(filho))
		##operacao "cima"
		filho <- Aspirador()
		filho$desc <- obj$desc
		filho$desc[5] <- 1
		filho$pai <- obj
		filho$g <- (obj$g + 3)
		filho$h <- heuristica.Aspirador(filho)
		filhos <- c(filhos, list(filho))
	}
	else { ##se estou na posicao 4
		##operacao "esquerda"
		filho <- Aspirador()
		filho$desc <- obj$desc
		filho$desc[5] <- 3 
		filho$pai <- obj
		filho$g <- (obj$g + 1)
		filho$h <- heuristica.Aspirador(filho)
		filhos <- c(filhos, list(filho))
		##operacao "cima"
		filho <- Aspirador()
		filho$desc <- obj$desc
		filho$desc[5] <- 2
		filho$pai <- obj
		filho$g <- (obj$g + 3)
		filho$h <- heuristica.Aspirador(filho)
		filhos <- c(filhos, list(filho))
	}	
	if (obj$desc[obj$desc[5]] == 1) {##se o quadrado esta sujo oferece o estado alcancado pela operacao limpar
    ##operacao "limpar"
    filho <- Aspirador()
    filho$desc <- obj$desc
	filho$desc[filho$desc[5]] <- 0
    filho$pai <- obj$pai
    filho$g <- (obj$g + 2)
    filho$h <- heuristica.Aspirador(filho)
    filhos <- c(filhos, list(filho))
	}
	return(filhos)
}

source("EstadoAspirador.R")
source("BuscaDesinformada.R")
source("BuscaInformada.R")

inicial <- Aspirador(desc = c(1,1,1,0,1)) #ex: 3 quadrados sujos

objetivo <- Aspirador(desc = c(0,0,0,0,1)) ##estado objetivo = todos os quadrados limpos

cat("====\tBusca em Largura\t=====\n")
print(unlist(buscaEmLargura(inicial, objetivo)))

cat("====\tBusca em Profundidade\t=====\n")
print(buscaEmProfundidade(inicial, objetivo))

cat("====\tBusca de Custo Uniforme\t=====\n")
print(buscaCustoUniforme(inicial, objetivo))

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))

cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))
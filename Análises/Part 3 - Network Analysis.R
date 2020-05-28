library(igraph)
library(psych)
library(qgraph)
library(tidyverse)

#Banco de dados
    network <- read.csv("network_carreira.csv", sep = ",")
    #Excluindo id e ACP
    network <- network %>% select(-X1, -ACP)
#Criando rede
    network <- as.matrix(network)
    networkCor <- cor(network)
    sds <- sqrt(diag(networkCor))
    networkMatrix <- cor2cov(networkCor, sds)

    networkGraph <- qgraph(networkMatrix, directed = FALSE, layout = "spring", 
                           graph = "glasso", sampleSize = 300)
    networkGraph

# Valores das relações dos itens
    networkMagnitudes <- getWmat(networkGraph)
    networkMagnitudes
    # Tabela das correlações dos itens
    plotMagnitudes <- cor.plot(networkMagnitudes, numbers = TRUE)
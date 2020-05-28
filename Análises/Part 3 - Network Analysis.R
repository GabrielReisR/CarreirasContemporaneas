# Abrindo a biblioteca
 library(igraph)
 library(psych)
 library(qgraph)
 library(tidyverse)

# Banco de dados completo
    network <- read.csv("https://raw.githubusercontent.com/GabrielReisR/CarreirasContemporaneas/master/Bancos/network%20analysis.csv", sep = ",")
    # Cada análise terá seu próprio banco, o banco acima contém todos escores desse arquivo
'''Criando rede 1 - escores específicos
    AEO 
    + MP (ACSF) + MF (ACSF) 
    + AG (ACP) + DV (ACP) 
    + AUTEN + BALAN + CRESC
    + EEC
    '''
    # lendo banco específico
    networkEsp <- read.csv
    networkEsp <- networkEsp %>% select(-X)
    # formando a matriz de correlações
     networkEsp <- as.matrix(networkEsp)
     networkCorEsp <- cor(networkEsp)
     sdsEsp <- sqrt(diag(networkCorEsp))
     networkMatrixEsp <- cor2cov(networkCorEsp, sdsEsp) # objeto final

    networkGraphEsp <- qgraph(networkMatrixEsp, directed = FALSE, layout = "spring", 
                           graph = "glasso", sampleSize = 300)
    networkGraphEsp # grafo de rede

 # Valores das relações dos itens
    networkMagnitudesEsp <- getWmat(networkGraphEsp)
    networkMagnitudesEsp
    # Tabela das correlações dos itens
    plotMagnitudesEsp <- cor.plot(networkMagnitudesEsp, numbers = TRUE)
 
 # Medidas de centralidade
    centrality1 <- centrality_auto(networkGraph1)

    nodeCentrality1 <- centralityItens_1$node.centrality
    edge1 <- centralityItens_1$edge.betweenness.centrality
    centralityPlot(networkGraph1, labels = colnames(network1))

'''Criando rede 2 - escores gerais
    AEO 
    + ACSF
    + ACP
    + AUTEN + BALAN + CRESC
    + EEC
    '''
    # removendo o que não entra na análise
    network2 <- network %>% select(-X, -MP, -MF, -AG, -DV)
    # formando a matriz de correlações
     network1 <- as.matrix(network1)
     networkCor1 <- cor(network1)
     sds1 <- sqrt(diag(networkCor1))
     networkMatrix1 <- cor2cov(networkCor1, sds1) # objeto final

    networkGraph1 <- qgraph(networkMatrix1, directed = FALSE, layout = "spring", 
                           graph = "glasso", sampleSize = 300)
    networkGraph1 # grafo de rede

 # Valores das relações dos itens
    networkMagnitudes1 <- getWmat(networkGraph1)
    networkMagnitudes1
    # Tabela das correlações dos itens
    plotMagnitudes1 <- cor.plot(networkMagnitudes1, numbers = TRUE)
 
 # Medidas de centralidade
    centrality1 <- centrality_auto(networkGraph1)

    nodeCentrality1 <- centralityItens_1$node.centrality
    edge1 <- centralityItens_1$edge.betweenness.centrality
    centralityPlot(networkGraph1, labels = colnames(network1))
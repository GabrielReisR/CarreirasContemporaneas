# Abrindo a biblioteca
 library(igraph)
 library(psych)
 library(qgraph)
 library(tidyverse)

# Banco de dados completo
    network <- read.csv("https://raw.githubusercontent.com/GabrielReisR/CarreirasContemporaneas/master/Bancos/network%20analysis.csv", sep = ",")
    # Cada análise terá seu próprio banco, o banco acima contém todos escores desse arquivo

# Criando rede 1 - escores específicos
 #   AEO 
 #   + MP (ACSF) + MF (ACSF) 
 #   + AG (ACP) + DV (ACP) 
 #   + AUTEN + BALAN + CRESC
 #   + EEC

 # lendo banco específico
    networkEsp <- read.csv("https://raw.githubusercontent.com/GabrielReisR/CarreirasContemporaneas/master/Bancos/network%20analysis%20-%20especifico.csv", sep = ",")
    networkEsp <- networkEsp %>% select(-X)

 # formando a matriz de correlações
     networkEsp <- as.matrix(networkEsp)
     networkCorEsp <- cor(networkEsp)
     sdsEsp <- sqrt(diag(networkCorEsp))
     networkMatrixEsp <- cor2cov(networkCorEsp, sdsEsp) # objeto final

    networkGraphEsp <- qgraph(networkMatrixEsp, directed = FALSE, layout = "spring", 
                           graph = "glasso", sampleSize = 300)
    networkGraphEsp # grafo de rede

 # Valores das correlações dos itens
    networkMagnitudesEsp <- getWmat(networkGraphEsp)
    networkMagnitudesEsp
    # Tabela das correlações dos itens
    plotMagnitudesEsp <- cor.plot(networkMagnitudesEsp, numbers = TRUE)
 
 # Medidas de centralidade
    centralityEsp <- centrality_auto(networkGraphEsp)

    nodeCentralityEsp <- centralityEsp$node.centrality
    edgeEsp <- centralityEsp$edge.betweenness.centrality
    centralityPlot(networkGraphEsp, labels = colnames(networkEsp))

# Criando rede 2 - escores gerais
 #   AEO 
 #   + ACSF
 #   + ACP
 #   + AUTEN + BALAN + CRESC
 #   + EEC

 # lendo banco específico
    networkGeral <- read.csv("https://raw.githubusercontent.com/GabrielReisR/CarreirasContemporaneas/master/Bancos/network%20analysis%20-%20geral.csv", sep = ",")
    networkGeral <- networkGeral %>% select(-X)

 # formando a matriz de correlações
     networkGeral <- as.matrix(networkGeral)
     networkCorGeral <- cor(networkGeral)
     sdsGeral <- sqrt(diag(networkCorGeral))
     networkMatrixGeral <- cor2cov(networkCorGeral, sdsGeral) # objeto final

   # grupos
   grupos <- list("Autoeficácia Ocupacional" = c(1), "Carreira Sem-Fronteiras" = c(2),
                  "Carreira Proteana" = c(3), "Autenticidade" = c(4), "Balanço" = c(5),
                  "Crescimento" = c(6), "Engajamento na Carreira" = c(7))
   names(networkGeral)
   networkGraphGeral <- qgraph(networkMatrixGeral, directed = FALSE, 
                                layout = "minimal", graph = "glasso", 
                                sampleSize = 300, groups = grupos, 
                                labels = FALSE)
   networkGraphGeral # grafo de rede

 # Valores das relações dos itens
    networkMagnitudesGeral <- getWmat(networkGraphGeral)
    networkMagnitudesGeral
    # Tabela das correlações dos itens
    plotMagnitudesGeral <- cor.plot(networkMagnitudesGeral, 
                                    numbers = TRUE)
 
 # Medidas de centralidade
    centralityPlot <- centralityPlot(networkGraphGeral, 
                                     include = c("Strength", 
                                                 "Closeness", 
                                                 "Betweenness"), 
                                     orderBy = "Strength")

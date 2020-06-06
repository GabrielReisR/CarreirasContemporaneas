# Abrindo a biblioteca
 library(igraph)
 library(psych)
 library(qgraph)
 library(tidyverse)
# Na Parte 3 foi apresentado um modelo mais conservador
# Esse modelo calculava os escores individuais a partir dos factor loadings
# Apesar deste ser o mais correto, resolvemos recalcular as redes
# Dessa vez com as médias dos escores brutos
# Permitindo mais variação e erro, colaborando para análise exploratória desses dados

# Criando rede com médias
 #   AEO 
 #   + ACSF
 #   + ACP
 #   + AUTEN + BALAN + CRESC
 #   + EEC

 # lendo banco específico
    networkGeral <- read.csv("https://raw.githubusercontent.com/GabrielReisR/CarreirasContemporaneas/master/Bancos/carreira%20fatoriais.csv", sep = ",")
    networkGeral <- networkGeral %>% 
        select(AEO_total, ACSF_total, ACP_total,
               PPC_AUTEN, PPC_BALAN, PPC_CRESC, EEC_total)

 # formando a matriz de correlações
     networkGeral <- as.matrix(networkGeral)
     networkCorGeral <- cor(networkGeral)
     sdsGeral <- sqrt(diag(networkCorGeral))
     networkMatrixGeral <- cor2cov(networkCorGeral, sdsGeral) # objeto final

   # grupos
   grupos <- list("Autoeficácia Ocupacional" = c(1), "Carreira Sem-Fronteiras" = c(2),
                  "Carreira Proteana" = c(3), "Autenticidade" = c(4), "Balanço" = c(5),
                  "Crescimento" = c(6), "Engajamento na Carreira" = c(7))

    networkGraphGeral <- qgraph(networkMatrixGeral, directed = FALSE, layout = "spring", 
                           graph = "glasso", sampleSize = 300, groups = grupos,
                           palette = "colorblind",
                           labels = c("AEO", "CSF", "CP", "Auten", "Balan", "Cresc", "Engaj"))
    networkGraphGeral # grafo de rede

 # Valores das relações dos itens
    networkMagnitudesGeral <- getWmat(networkGraphGeral)
    networkMagnitudesGeral
    # Tabela das correlações dos itens
    plotMagnitudesGeral <- cor.plot(networkMagnitudesGeral, numbers = TRUE)
 
 # Medidas de centralidade
    centralityPlot(networkGraphGeral, include = c("Strength", "Closeness", "Betweenness"),
               orderBy = "Strength")

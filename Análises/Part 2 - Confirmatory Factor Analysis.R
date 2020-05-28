#Abrindo a biblioteca
 library(psych)
 library(lavaan)
 library(semPlot)
 library(skimr)
 library(tidyverse)

#Banco de dados
  carreira <- read.csv("https://raw.githubusercontent.com/GabrielReisR/CarreirasContemporaneas/master/Bancos/carreira.csv", sep = ",")
  #Excluindo X
  carreira <- carreira[, -1]
  View(carreira) #tudo ok!
#Análises Fatoriais Confirmatórias
#AEO = Autoeficácia Ocupacional
  model_AEO <- ' AEO =~ AEO01 + AEO02 + AEO03 + AEO04 + AEO05 + AEO06 '
  fit_AEO <- cfa(model = model_AEO, data = carreira, 
                 ordered = TRUE, estimator = "WLSMV")
  summary(fit_AEO, fit.measures = TRUE, standardized = TRUE)
  AEO_loadings <- inspect(fit_AEO, what = "std")[["lambda"]]
  #Factor loadings entre .535 e .727
  #CFI = .982, TLI = .970
  #RMSEA = .062 (entre .023 e .1)
  #SRMR = .035

#ACSF = Atitude de Carreira sem Fronteiras
  model_ACSF <- '
  MP =~ ACSF05 + ACSF06 + ACSF03 + ACSF04 + ACSF07 + ACSF08
  MF =~ ACSF12 + ACSF13 + ACSF11 + ACSF10
  '
  fit_ACSF <- cfa(model = model_ACSF, data = carreira, 
                  ordered = TRUE, estimator = "WLSMV")
  summary(fit_ACSF, fit.measures = TRUE, standardized = TRUE)
  ACSF_loadings <- inspect(fit_ACSF, what = "std")[["lambda"]]
  #Factor loadings entre .508 e .823
  #CFI = .929, TLI = .958
  #RMSEA = .106 (entre .089 e .124)
  #SRMR = .082

  #modelo geral da ACSF
  model_ACSF_geral <- ' 
  ACSF =~ ACSF05 + ACSF06 + ACSF03 + ACSF04 + ACSF07 + ACSF08 + ACSF12 + ACSF13 + ACSF11 + ACSF10
  '
  fit_ACSF_geral <- cfa(model = model_ACSF_geral, data = carreira, 
                        ordered = TRUE, estimator = "WLSMV")
  summary(fit_ACSF_geral, fit.measures = TRUE, standardized = TRUE)
  ACSF_loadings_geral <- inspect(fit_ACSF_geral, what = "std")[["lambda"]]
  #Factor loadings entre .444 e .707
  #CFI = .520, TLI = .383
  #RMSEA = .273 (entre .257 e .290)
  #SRMR = .222
#ACP  = Atitude de Carreira Proteana
  model_ACP <- '
  AG =~ ACP05 + ACP03 + ACP06 + ACP04 + ACP02 + ACP07
  DV =~ ACP11 + ACP09 + ACP14 + ACP12
  ACP12 ~~ ACP14
  '
  #Retirado o item 01, de acordo com "http://pepsic.bvsalud.org/pdf/gerais/v2n2/v2n2a11.pdf"
  fit_ACP <- cfa(model = model_ACP, data = carreira, 
                 ordered = TRUE, estimator = "WLSMV")
  summary(fit_ACP, fit.measures = TRUE, standardized = TRUE, modindices = TRUE)
  ACP_loadings <- inspect(fit_ACP, what = "std")[["lambda"]]
  #Factor loadings entre .465 e .839
  #CFI = .933, TLI = .909
  #RMSEA = 0.098 (entre .080 e .116)
  #SRMR = .069

  #Caso se queira um modelo com ACP geral
  model_ACP_geral <- '
  ACP =~ ACP05 + ACP03 + ACP06 + ACP04 + ACP02 + ACP07 + ACP13 + ACP11 + ACP09 + ACP12
  ACP12 ~~ ACP13
  '
  #Retirado o item 01, de acordo com "http://pepsic.bvsalud.org/pdf/gerais/v2n2/v2n2a11.pdf"
  #Retirado item 14 por apresentar factor loading = .261
  fit_ACP_geral <- cfa(model = model_ACP_geral, data = carreira, 
                       ordered = TRUE, estimator = "WLSMV")
  summary(fit_ACP_geral, fit.measures = TRUE, standardized = TRUE, modindices = TRUE)
  ACP_loadings_geral <- inspect(fit_ACP_geral, what = "std")[["lambda"]]
  #Factor loadings entre .305 e .838
  #CFI = .881, TLI = .842
  #RMSEA = .130 (entre .113 e .148)
  #SRMR = .087

#PPC  = Escala de Parâmetros de Carreira Caleidoscópica
  model_PPC <- '
  AUTEN =~ PPC01 + PPC02 + PPC05 + PPC07 + PPC10 + PPC17 + PPC19
  BALAN =~ PPC04 + PPC06 + PPC08 + PPC11 + PPC15 + PPC18
  CRESC =~ PPC03 + PPC09 + PPC14 + PPC12 + PPC13
  PPC14 ~~ PPC12
  '
  #Retirado item 16 por apresentar factor loading = .222
  fit_PPC <- cfa(model = model_PPC, data = carreira, 
                 ordered = TRUE, estimator = "WLSMV")
  summary(fit_PPC, fit.measures = TRUE, standardized = TRUE, modindices = TRUE)
  PPC_loadings <- inspect(fit_PPC, what = "std")[["lambda"]]
  #Factor loadings entre .222 e .803
  #CFI = .903, TLI = .886
  #RMSEA = 0.70 (entre .060 e .079)
  #SRMR = .066

#EEC  = Engajamento com a Carreira
  model_EEC <- ' EEC =~ EEC01 + EEC02 + EEC03 + EEC04 + EEC05 + EEC06 + EEC07 + EEC08 + EEC09 '
  fit_EEC <- cfa(model = model_EEC, data = carreira, 
                 ordered = TRUE, estimator = "WLSMV")
  summary(fit_EEC, fit.measures = TRUE, standardized = TRUE, modindices = TRUE)
  EEC_loadings <- inspect(fit_EEC, what = "std")[["lambda"]]
  #Factor loadings entre .527 e .821
  #CFI = .920, TLI = .894
  #RMSEA entre .140 e .178
  #SRMR = .067

# Criação dos escores z por item de cada participante
  #AEO
    AEO_predict <- predict(fit_AEO)
    carreira <- cbind(carreira, AEO_predict)
  #ACSF
    ACSF_predict <- predict(fit_ACSF)
    carreira <- cbind(carreira, ACSF_predict)
  #ACSF_geral
    ACSF_geral_predict <- predict(fit_ACSF_geral)
    carreira <- cbind(carreira, ACSF_geral_predict)
  #ACP
    ACP_predict <- predict(fit_ACP)
    carreira <- cbind(carreira, ACP_predict)
  #ACP_geral
    ACP_geral_predict <- predict(fit_ACP_geral)
    carreira <- cbind(carreira, ACP_geral_predict)
  #PPC
    PPC_predict <- predict(fit_PPC)
    carreira <- cbind(carreira, PPC_predict)
  #EEC
    EEC_predict <- predict(fit_EEC)
    carreira <- cbind(carreira, EEC_predict)

#Criando escores das médias e DPs para análises descritivas
  #AEO
  carreira <- carreira %>% rowwise() 
    %>% mutate(AEO_total = mean(AEO01:AEO06, na.rm = T))
  #ACSF
  carreira <- carreira %>% rowwise() 
    %>% mutate(ACSF_total = mean(c(ACSF03:ACSF08, ACSF10:ACSF13), na.rm = T))
  #ACSF_MP
  carreira <- carreira %>% rowwise() %>%
    mutate(ACSF_MP = mean(c(ACSF03:ACSF08), na.rm = T))
  #ACSF_MF
  carreira <- carreira %>% rowwise() %>%
    mutate(ACSF_MF = mean(c(ACSF10:ACSF13), na.rm = T))
  #ACP
  carreira <- carreira %>% rowwise() %>%
    mutate(ACP_total = mean(c(ACP02:ACP13), na.rm = T))
  #ACP_AG
  carreira <- carreira %>% rowwise() %>%
    mutate(ACP_AG = mean(c(ACP02:ACP07), na.rm = T))
  #ACP_DV
  carreira <- carreira %>% rowwise() %>%
    mutate(ACP_DV = mean(c(ACP09, ACP11, ACP12, ACP14), na.rm = T))
  #PPC
  carreira <- carreira %>% rowwise() %>%
    mutate(PPC_total = mean(c(PPC01:PPC15, PPC17:PPC19), na.rm = T))
  #AUTEN
  carreira <- carreira %>% rowwise() %>%
    mutate(PPC_AUTEN = mean(c(PPC01, PPC02, PPC05, PPC07, PPC10, PPC17, PPC19), na.rm = T))
  #BALAN
  carreira <- carreira %>% rowwise() %>%
    mutate(PPC_BALAN = mean(c(PPC04, PPC06, PPC08, PPC11, PPC15, PPC18), na.rm = T))
  #CRESC
  carreira <- carreira %>% rowwise() %>%
    mutate(PPC_CRESC = mean(c(PPC03, PPC09, PPC12:PPC14), na.rm = T))
  #EEC
 carreira <- carreira %>% rowwise() %>%
  mutate(EEC_total = mean(c(EEC01:EEC09), na.rm = T))
#Uma olhada em como o banco está
glimpse(carreira) #tudo ok

#Novo banco com todas alterações anteriores
  write.csv(carreira, "carreira fatoriais.csv")

#Novo banco apenas para a análise de redes - escores z
  network <- carreira %>% select(AEO:EEC)
  write.csv(network, "network analysis.csv")

#Abrindo a biblioteca
 library(psych)
 library(lavaan)
 library(semPlot)

#Cleaning Dataset.R:
#Abrindo a biblioteca
  library(haven)
  library(psych)
  library(skimr)
  library(tidyverse)

#Entendendo o banco de dados
  carreira <- read_sav("C:\\Users\\Marco2\\Desktop\\Gabriel\\Mestrado\\Artigos e capítulos\\Em andamento\\Artigo - Carreiras contemporâneas\\bancosintegrados.sav")
  glimpse(carreira)

  #Nome das variáveis
    #AEO  = Autoeficácia Ocupacional
    #ACSF = Atitude de Carreira sem Fronteiras
    #ACP  = Atitude de Carreira Proteana
    #PPC  = Escala de Parâmetros de Carreira Caleidoscópica
    #EEC  = Engajamento com a Carreira

  #A variável ID parece estranha, é melhor olhá-la de perto
    carreira <- carreira %>% arrange(ID)
    carreira

    #ID está significando "Concordo Discordo" ao invés de identificar os participantes
    #Criando uma nova ID
      id <- 1:300
      carreira <- cbind(carreira, id)
      glimpse(carreira)
    #Pronto! Agora vou selecionar "id" (em minúsculo) e prosseguir

#Selecionando apenas as sete variáveis principais, já convertendo para números
  carreira <- carreira %>%
    select(id, Sexo, AEO01:EEC09)

#Análises descritivas iniciais
  skim(carreira)

#Análises Fatoriais Confirmatórias
#AEO = Autoeficácia Ocupacional
  model_AEO <- ' AEO =~ AEO01 + AEO02 + AEO03 + AEO04 + AEO05 + AEO06 '
  fit_AEO <- cfa(model = model_AEO, data = carreira, 
                ordered = TRUE, estimator = "WLSMV")
  summary(fit_AEO, fit.measures = TRUE, standardized = TRUE)
  AEO_loadings <- inspect(fit_AEO, what = "std")[["lambda"]]
  #Factor loadings entre .535 e .727
  #CFI = .982, TLI = .970
  #RMSEA entre .023 e .1
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
  #RMSEA entre .089 e .124
  #SRMR = .082

#ACP  = Atitude de Carreira Proteana
  model_ACP <- '
  AG =~ ACP05 + ACP03 + ACP06 + ACP04 + ACP02 + ACP07
  DV =~ ACP01 + ACP13 + ACP11 + ACP09 + ACP14 + ACP12
  '
  fit_ACP <- cfa(model = model_ACP, data = carreira, 
                  ordered = TRUE, estimator = "WLSMV")
  summary(fit_ACP, fit.measures = TRUE, standardized = TRUE)
  ACP_loadings <- inspect(fit_ACP, what = "std")[["lambda"]]
  #Factor loadings entre .343 e .840
  #CFI = .856, TLI = .820
  #RMSEA entre .106 e .133
  #SRMR = .094

  #Caso se queira um modelo com ACP geral
  model_ACP_geral <- '
  ACP =~ ACP05 + ACP03 + ACP06 + ACP04 + ACP02 + ACP07 + ACP01 + ACP13 + ACP11 + ACP09 + ACP14 + ACP12
  '
  fit_ACP_geral <- cfa(model = model_ACP_geral, data = carreira, 
                  ordered = TRUE, estimator = "WLSMV")
  summary(fit_ACP_geral, fit.measures = TRUE, standardized = TRUE)
  ACP_loadings_geral <- inspect(fit_ACP_geral, what = "std")[["lambda"]]
  #Factor loadings entre .328 e .817
  #CFI = .772, TLI = .722
  #RMSEA entre .135 e .162
  #SRMR = .113

#PPC  = Escala de Parâmetros de Carreira Caleidoscópica
  model_PPC <- '
  AUTEN =~ PPC01 + PPC02 + PPC05 + PPC07 + PPC10 + PPC16 + PPC17 + PPC19
  BALAN =~ PPC04 + PPC08 + PPC06 + PPC11 + PPC15 + PPC18
  CRESC =~ PPC03 + PPC09 + PPC14 + PPC12 + PPC13
  '
  fit_PPC <- cfa(model = model_PPC, data = carreira, 
                  ordered = TRUE, estimator = "WLSMV")
  summary(fit_PPC, fit.measures = TRUE, standardized = TRUE)
  PPC_loadings <- inspect(fit_PPC, what = "std")[["lambda"]]
  #Factor loadings entre .222 e .803
  #CFI = .877, TLI = .859
  #RMSEA entre .065 e .083
  #SRMR = .070

#EEC  = Engajamento com a Carreira
  model_EEC <- ' EEC =~ EEC01 + EEC02 + EEC03 + EEC04 + EEC05 + EEC06 + EEC07 + EEC08 + EEC09 '
  fit_EEC <- cfa(model = model_EEC, data = carreira, 
                  ordered = TRUE, estimator = "WLSMV")
  summary(fit_EEC, fit.measures = TRUE, standardized = TRUE)
  EEC_loadings <- inspect(fit_EEC, what = "std")[["lambda"]]
  EEC_loadings

  #Factor loadings entre .527 e .821
  #CFI = .920, TLI = .894
  #RMSEA entre .140 e .178
  #SRMR = .067




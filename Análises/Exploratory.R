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
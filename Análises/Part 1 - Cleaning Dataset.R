#Abrindo a biblioteca
  library(psych)
  library(readr)
  library(skimr)
  library(tidyverse)

#Entendendo o banco de dados
  banco_inicial <- read_csv("https://raw.githubusercontent.com/GabrielReisR/CarreirasContemporaneas/master/Bancos/bancosintegrados.csv")
  View(banco_inicial) #visualização do banco

  #Nome das variáveis
    #AEO  = Autoeficácia Ocupacional
    #ACSF = Atitude de Carreira sem Fronteiras
    #ACP  = Atitude de Carreira Proteana
    #PPC  = Escala de Parâmetros de Carreira Caleidoscópica
    #EEC  = Engajamento com a Carreira

  #A variável ID parece estranha, é melhor olhá-la de perto
    banco_inicial <- banco_inicial %>% arrange(ID)
    banco_inicial

    #ID está significando "Concordo/Discordo" ao invés de identificar os participantes
    #Criando uma nova ID
     id <- 1:300
     banco_inicial <- cbind(banco_inicial, id)
     glimpse(banco_inicial)
    #Pronto! Agora vou selecionar "id" (em minúsculo) e prosseguir
    
#Selecionando apenas as sete variáveis principais, já convertendo para números
  banco_inicial <- banco_inicial %>%
    select(id, Sexo, AEO01:EEC09)

#Análises descritivas iniciais
  skim(banco_inicial) #algumas respostas que deveriam ser inteiras estão fracionadas

  #Transformando em números inteiros
  banco_inicial <- round(banco_inicial)
  skim(banco_inicial) #tudo ok!

#Novo banco limpo - prosseguir para análises de carreira
write.csv(banco_inicial, "carreira.csv")
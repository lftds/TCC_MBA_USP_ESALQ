install.packages("geobr")
install.packages("tidyverse")
install.packages("tmap")
install.packages("sf")
install.packages("dismo")
install.packages("deldir")
install.packages("readxl")
install.packages("writexl")
library(geobr)
library(tidyverse)
library(tmap)
library(readxl)
library(writexl)
library(sf)
library(dismo)
library(deldir)

#verificando os pacotes instalados
(.packages())

setwd("/Users/roykapeniak/Desktop/TCC_USP/basededados")
getwd()

####################################### ETAPA 1 ################################################### 
### CRIANDO O DATAFRAME COM OS DADOS DOS PRINCIPAIS HOSPITAIS DE VILA VELHA - ES ###
## FONTE DADOS HOSPITAIS PÚBLICOS MUNICIPAIS:
# https://www.vilavelha.es.gov.br/paginas/saude-unidades-de-saude-por-regioes-administrativas ##
# Demais hospitais: Pesquisa de campo  #


idhospital   <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
tipohospital <- 
  c(
    'Hospital Estadual',
    'Hospital Estadual',
    'Hospital Estadual',
    'Hospital Estadual',
    'Hospital Privado',
    'Hospital Privado',
    'Hospital Privado',
    'Hospital Privado',
    'Unidade Básica',
    'Unidade Básica',
    'Unidade Básica',
    'Unidade Básica',
    'Unidade Básica',
    'Unidade Básica',
    'Unidade Básica',
    'Unidade Básica',
    'Unidade Básica',
    'Unidade Básica',
    'Unidade de Atenção',
    'Unidade de Cuidados Específicos',
    'Unidade de Estratégia de Saúde',
    'Unidade de Estratégia de Saúde',
    'Unidade de Estratégia de Saúde',
    'Unidade de Estratégia de Saúde',
    'Unidade de Estratégia de Saúde',
    'Unidade de Estratégia de Saúde',
    'Unidade de Estratégia de Saúde',
    'Unidade de Estratégia de Saúde',
    'Unidade de Estratégia de Saúde',
    'Unidade de Pronto Atendimento'
  )
nomehospital <-
  c(
    'Centro de Reabilitação Física do Espírito Santo (CREFES)',
    'Hospital Estadual Antonio Bezerra de Faria',
    'Hospital Estadual Dr Nilton de Barros',
    'Hospital Estadual Infantil e Maternidade  Alzir Bernardo Alves',
    'Hospital Praia da Costa',
    'Hospital Evangélico de Vila Velha',
    'Hospital Santa Mônica',
    'Hospital e Maternidade São Luis',
    'Unidade Básica de Saúde de Coqueiral de Itaparica',
    'Unidade Básica de Saúde Jaburuna',
    'Unidade Básica de Saúde de Ataíde',
    'Unidade Básica de Saúde Santa Rita',
    'Unidade Básica de Saúde Dom João Batista',
    'Unidade Básica de Saúde Vila Garrido',
    'Unidade Básica de Saúde Provisória de Paul',
    'Unidade Básica de Saúde Vale Encantado',
    'Unidade Básica de Saúde Jardim Marilândia',
    'Unidade Básica de Saúde São Torquato',
    'Unidade de Atenção Primária à Saúde de Vila Batista',
    'Unidade de Cuidados Específicos da Prainha',
    'Unidade de Estratégia de Saúde da Família Vila Nova',
    'Unidade de Saúde da Família Araçás',
    'Unidade de Estratégia de Saúde da Família Ibes',
    'Unidade de Estratégia de Saúde da Família Jardim Colorado',
    'Unidade de Estratégia de Saúde da Família Barra do Jucu',
    'Unidade de Estratégia de Saúde da Família Ponta Fruta',
    'Unidade de Estratégia de Saúde da Família Ulisses Guimarães',
    'Unidade de Estratégia de Saúde da Família Terra Vermelha',
    'Unidade de Estratégia de Saúde da Família Barramares',
    'UPA Zilda Arns'
  )

longitude <- 
  c(
    -40.27404,
    -40.29898,
    -40.35120,
    -40.30875,
    -40.28761,
    -40.34314,
    -40.29590,
    -40.31492,
    -40.30441,
    -40.30153,
    -40.32433,
    -40.33709,
    -40.31886,
    -40.33958,
    -40.33778,
    -40.34104,
    -40.35337,
    -40.34822,
    -40.33545,
    -40.29146,
    -40.31377,
    -40.32569,
    -40.31559,
    -40.31545,
    -40.32597,
    -40.35897,
    -40.33941,
    -40.35197,
    -40.34681,
    -40.33132
  )

latitude <-
  c (
    -20.32782,
    -20.33275,
    -20.32939,
    -20.34678,
    -20.33423,
    -20.34605,
    -20.36012,
    -20.35165,
    -20.36334,
    -20.33653,
    -20.34724,
    -20.34946,
    -20.33776,
    -20.33907,
    -20.33089,
    -20.44449,
    -20.36221,
    -20.33125,
    -20.33803,
    -20.33257,
    -20.36464,
    -20.38711,
    -20.35042,
    -20.36018,
    -20.42551,
    -20.49864,
    -20.44458,
    -20.44412,
    -20.43770,
    -20.42644
  )

dfhospitais <- data.frame(idhospital,tipohospital,nomehospital,longitude,latitude)



################################ ETAPA 2 ##########################################


#CRIAÇÃO DO POLIGONO DE VORONOI




#Criando um objeto do tipo sf dos pontos de hospitais, a partir do  data frame de hospitais:
sf_HospitaisVV <- st_as_sf(x = dfhospitais, 
                           coords = c("longitude", "latitude"), 
                           crs = 4326)


#Criação das área de influência com base na base de dados de hospitais de Vila Velha - ES

pontosvoronoi<- dfhospitais

coordinates(pontosvoronoi) <- c("longitude", "latitude") 
proj4string(pontosvoronoi) <- CRS("+proj=longlat +datum=WGS84")

AreadeInfluenciaDosHospitais <- voronoi(pontosvoronoi)

class(vv)                              # [1] "sf"         "data.frame"
class (AreadeInfluenciaDosHospitais)    #[1] "sp SpatialPolygonsDataFrame"

# Transformando o arquivo do município de Vila Velha em arquivo espacial

spdfvv <- as_Spatial(vv)
spdfvv <- as(vv, "Spatial")
as(spdfvv, "sf")

class(spdfvv)                           #[1] "SpatialPolygonsDataFrame"
class (AreadeInfluenciaDosHospitais)    #[1] "sp SpatialPolygonsDataFrame"


#fazendo o intersect entre o limite de Vila Velha e o setor censitário 

intersectsetorvv <- intersect(spdfvv, AreadeInfluenciaDosHospitais)

# Figura 2  - Áreas de influência
tmap_mode("view")
tm_shape(shp =intersectsetorvv) +
tm_borders(alpha = 0.8)

####################################### ETAPA 3 ################################################### 



##Informações do Pacote GeoBR##

# Criando um dataframe geobrbases para explorar o conteúdo das informações no pacote geobr,
# temos informaçoes de ano referência, informações no nivel regional, municipal e 
# e de setor censitário de acordo com o censo do IBGE, com base no ano de referência das informações

# Para este estudo utilizaremos abaixo as informações geográficas de censo demográfico de 2010 
# disponibilizada pelo IBGE

geobrbases <- list_geobr() # Dataframe com todas as informações gerais do pacote geobr

br <- read_country(year ='2010') #Setando o nível de informações geográficas do Brasil  2010

estados <-  read_state (code_state = "all")           #Selecionando todos os Estados do Brasil em 2010
sudeste <- filter(estados, name_region == 'Sudeste')  #filtrando dados da região Sudeste em 2010

es <- read_state (code_state = "ES")           #filtrando dados do Estado do Espírito Santo em 2010.
brmuni <- read_municipality()
munies <- filter(brmuni, abbrev_state == 'ES') #filtrando o mapa com o limite de município de Vila Velha - ES
vv <- read_municipality (code_muni = 3205200)  #filtrando o município de Vila Velha - ES 

#carregando o shapefile de bairros de Vila Velha - ES
#fonte: 
#https://www.vilavelha.es.gov.br/paginas/desenvolvimento-urbano-e-mobilidade-arquivos-de-georreferenciamento

shapebairrovv <- read_sf(dsn = ".", layer = "BAIRROS")

# Gerando a malha de setores censitários do IBGE de 2010 de Vila Velha - ES
setoresESVilaVelha <- 
  
  read_census_tract(
    code_tract = 3205200,
    year = 2010,
    zone = "urban",
    simplified = TRUE,
    showProgress = TRUE
  )



# Figura 3 - Setores censitários
tmap_mode("view")
tm_shape(shp = setoresESVilaVelha) +
tm_borders(alpha = 0.8)



################################ ETAPA 4 e SAÍDAS ##########################################


#Coletando informaçoes de Populaçao, Idade e Renda
#nível: setor censitário IBGE - 2010 


#DADOS DE POPULACAO TOTAL

# https://censo2010.ibge.gov.br/sinopseporsetores/?nivel=st
# importando a tabela de Pessoas Residentes ( Total de Pessoas ) do Município de Vila Velha - ES
# Pessoa03_ES - 6.8 Arquivo Cor ou Raça, idade e gênero (planilha Pessoa03_UF.xls ou Pessoa03_UF.csv)
# V001 - Pessoas Residentes

# Disponível em: 
#https://www.ibge.gov.br/estatisticas/sociais/saude/9662-censo-demografico-2010.html?edicao=10410&t=resultados Censo 2010


Pessoa03_ES <- read_excel("Pessoa03_ES.xls", range = "A1:H6382", 
                          sheet = "Pessoa03_ES", col_types = c("text", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric",
                                                               "numeric"))


setoresVVPessoas_fenotipo_merged <-
  setoresESVilaVelha  %>%
  left_join(Pessoa03_ES,by=c('code_tract'='Cod_setor'))





#MAPA DE POPULACAO TOTAL x ÁREAS DE INFLUÊNCIAS DE VORONOI DOS HOSPITAIS DE VILA VELHA - ES


#mapas temáticos - Pessoas Residente e Fenótipos por setor censitário

tmap_mode("view")
tm_shape(shp = setoresVVPessoas_fenotipo_merged)+
  tm_fill("V001",   
          #V001 - Total 
        
          alpha = 0.8,
          title="População Total : 414.584 - Censo 2010 - IBGE",
          tyle = "equal", 
          textNA = "sem dados no arquivo de Censo",
          breaks = c(0, 10, 100, 250, 750, 1000,  1500, 2000, 2500, 2750),
          palette = c("white" ,
                      "blue" ,
                      "green" ,
                      "yellow",
                      "salmon",
                      "brown" ,
                      "pink"   ,
                      "violet" ,
                      "purple" ,
                      "red"   )              
  ) +
  tm_borders(alpha = 0.1)  +
  
  tm_shape(shp = sf_HospitaisVV) +
  tm_dots(palette = "Set2", 
          col = "nomehospital",
          id = "nomehospital", # bold in popup
          popup.vars = c("Tipo:" = "tipohospital", "Hospital:" = "nomehospital"), # light in popup
          title="Principais Hospitais de Vila Velha",
          size = 0.1)+
  
  tm_shape(shp =intersectsetorvv) +
  tm_borders(alpha = 0.8)

#SAÍDA 2
#MAPA DE RENDA DO RESPONSÁVEL x ÁREAS DE INFLUÊNCIAS DE VORONOI DOS HOSPITAIS DE VILA VELHA - ES

#DADOS DE RENDA DO RESPONSÁVEL

# https://censo2010.ibge.gov.br/sinopseporsetores/?nivel=st
# importando a tabela de Renda do Responsável do Município de Vila Velha - ES
# ResponsavelRenda_ES - 6.8 Arquivo Cor ou Raça, idade e gênero (planilha Pessoa03_UF.xls ou Pessoa03_UF.csv)


RendaDoResponsavelES <- read_excel("ResponsavelRenda_ES.xls", range = "A1:Z6382", 
                                   sheet = "Desidentifica_ResponsavelRendaU",col_types = c("text", 
                                                                                           "numeric", "numeric", "numeric", 
                                                                                           "numeric", "numeric", "numeric",
                                                                                           "numeric", "numeric", "numeric", 
                                                                                           "numeric", "numeric", "numeric",
                                                                                           "numeric", "numeric", "numeric", 
                                                                                           "numeric", "numeric", "numeric",
                                                                                           "numeric", "numeric", "numeric", 
                                                                                           "numeric", "numeric", "numeric", 
                                                                                           "numeric"))

setoresVVRendaTotalResponsavel_merged <-
  setoresESVilaVelha  %>%
  left_join(RendaDoResponsavelES,by=c('code_tract'='Cod_setor'))



#mapas temáticos - Renda Total do Responsável por setor censitário


tmap_mode("view")
tm_shape(shp = setoresVVRendaTotalResponsavel_merged)+
  tm_fill("V022",   
          alpha = 0.8,
          title="Total do rendimento nominal mensal dos responsáveis pelo domicílio em R$",
          tyle = "equal", 
          textNA = "sem dados no arquivo de Censo",
          breaks = c(0, 5000, 10000, 50000, 100000, 500000, 1000000, 2000000, 3000000,3500000),
          palette = c("white",
                      "green",
                      "blue",    
                      "cyan",
                      "yellow",
                      "pink",
                      "violet",
                      "purple",
                      "red",
                      "black")              
  ) +
  tm_borders(alpha = 0.1)+
  
  
  tm_shape(shp = sf_HospitaisVV) +
  tm_dots(palette = "Set2", 
          col = "nomehospital",
          id = "nomehospital", # bold in popup
          popup.vars = c("Tipo:" = "tipohospital", "Hospital:" = "nomehospital"), # light in popup
          title="Principais Hospitais de Vila Velha",
          size = 0.2)+
  
  tm_shape(shp =intersectsetorvv) +
  tm_borders(alpha = 0.8)




#SAÍDA 3
#MAPA DE FAIXA DE IDADE x ÁREAS DE INFLUÊNCIAS DE VORONOI DOS HOSPITAIS DE VILA VELHA - ES

#DADOS DE IDADE 

# https://censo2010.ibge.gov.br/sinopseporsetores/?nivel=st
# importando a tabela de dados de idade total por setor censitário do Município de Vila Velha - ES
# Pessoa13_ES  - 6.16 Arquivo Idade, total (planilha Pessoa13_UF.xls ou Pessoa13_UF.csv)

#idade


# V014                                                        Avôs ou avós em domicílios particulares
# V022	V035	V036	V037	V038	V039                        	1_a_5_anos
# V040	V041	V042	V043	V044	V045	V046	V047	V048	V049	6_a_15_anos	
# V050	V051	V052	V053	V054	V055	V056	V057	V058	V059	16_a_25_anos
# V060	V061	V062	V063	V064	V065	V066	V067	V068	V069	17_a_35_anos
# V070	V071	V072	V073	V074	V075	V076	V077	V078	V079	36_a_45_anos
# V080	V081	V082	V083	V084	V085	V086	V087	V088	V089	46_a_55_anos
# V090	V091	V092	V093	V094	V095	V096	V097	V098	V099	56_a_65_anos
# V100	V101	V102	V103	V104	V105	V106	V107	V108	V109	66_a_75_anos
# V110	V111	V112	V113	V114	V115	V116	V117	V118	V119	76_a_85_anos
# V120	V121	V122	V123	V124	V125	V126	V127	V128	V129	86_a_ 95_anos
# V130	V131	V132	V133	V134	                              Mais_que_96 anos 



Pessoa13_ES <- read_excel("Pessoa13_ES.xls", 
                          sheet = "Pessoa13_ES", col_types = c("text", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric",  
                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric",  
                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric",  
                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric"))

#Criação de Faixa etária com base na idade total

names(Pessoa13_ES)

Pessoa13_ES$Faixa_1_a_5_anos <- Pessoa13_ES$V022+Pessoa13_ES$V035+Pessoa13_ES$V036+Pessoa13_ES$V037+Pessoa13_ES$V038+Pessoa13_ES$V039

Pessoa13_ES$Faixa_6_a_15_anos	<- Pessoa13_ES$V040+Pessoa13_ES$V041+Pessoa13_ES$V042+Pessoa13_ES$V043+Pessoa13_ES$V044+Pessoa13_ES$V045+Pessoa13_ES$V046+Pessoa13_ES$V047+Pessoa13_ES$V048+Pessoa13_ES$V049

Pessoa13_ES$Faixa_16_a_25_anos <- Pessoa13_ES$V050+Pessoa13_ES$V051+Pessoa13_ES$V052+Pessoa13_ES$V053+Pessoa13_ES$V054+Pessoa13_ES$V055+Pessoa13_ES$V056+Pessoa13_ES$V057+Pessoa13_ES$V058+Pessoa13_ES$V059

Pessoa13_ES$Faixa_17_a_35_anos <- Pessoa13_ES$V060+Pessoa13_ES$V061+Pessoa13_ES$V062+Pessoa13_ES$V063+Pessoa13_ES$V064+Pessoa13_ES$V065+Pessoa13_ES$V066+Pessoa13_ES$V067+Pessoa13_ES$V068+Pessoa13_ES$V069

Pessoa13_ES$Faixa_36_a_45_anos <-  Pessoa13_ES$V070+Pessoa13_ES$V071+Pessoa13_ES$V072+Pessoa13_ES$V073+Pessoa13_ES$V074+Pessoa13_ES$V075+Pessoa13_ES$V076+Pessoa13_ES$V077+Pessoa13_ES$V078+Pessoa13_ES$V079

Pessoa13_ES$Faixa_46_a_55_anos <-  Pessoa13_ES$V080+Pessoa13_ES$V081+Pessoa13_ES$V082+Pessoa13_ES$V083+Pessoa13_ES$V084+Pessoa13_ES$V085+Pessoa13_ES$V086+Pessoa13_ES$V087+Pessoa13_ES$V088+Pessoa13_ES$V089

Pessoa13_ES$Faixa_56_a_65_anos <-  Pessoa13_ES$V090+Pessoa13_ES$V091+Pessoa13_ES$V092+Pessoa13_ES$V093+Pessoa13_ES$V094+Pessoa13_ES$V095+Pessoa13_ES$V096+Pessoa13_ES$V097+Pessoa13_ES$V098+Pessoa13_ES$V099

Pessoa13_ES$Faixa_66_a_75_anos <-  Pessoa13_ES$V100+Pessoa13_ES$V101+Pessoa13_ES$V102+Pessoa13_ES$V103+Pessoa13_ES$V104+Pessoa13_ES$V105+Pessoa13_ES$V106+Pessoa13_ES$V107+Pessoa13_ES$V108+Pessoa13_ES$V109

Pessoa13_ES$Faixa_76_a_85_anos <-  Pessoa13_ES$V110+Pessoa13_ES$V111+Pessoa13_ES$V112+Pessoa13_ES$V113+Pessoa13_ES$V114+Pessoa13_ES$V115+Pessoa13_ES$V116+Pessoa13_ES$V117+Pessoa13_ES$V118+Pessoa13_ES$V119

Pessoa13_ES$Faixa_86_a_95_anos <-  Pessoa13_ES$V120+Pessoa13_ES$V121+Pessoa13_ES$V122+Pessoa13_ES$V123+Pessoa13_ES$V124+Pessoa13_ES$V125+Pessoa13_ES$V126+Pessoa13_ES$V127+Pessoa13_ES$V128+Pessoa13_ES$V129

Pessoa13_ES$Faixa_Mais_que_96_anos <-  Pessoa13_ES$V130+Pessoa13_ES$V131+Pessoa13_ES$V132+Pessoa13_ES$V133+Pessoa13_ES$V134

Pessoa13_ES$Faixa_Mais_que_65_anos <-
  Pessoa13_ES$Faixa_66_a_75_anos +
  Pessoa13_ES$Faixa_76_a_85_anos +
  Pessoa13_ES$Faixa_86_a_95_anos +
  Pessoa13_ES$Faixa_Mais_que_96_anos

Pessoa13_ES$Faixa_Mais_que_45_anos <-
  Pessoa13_ES$Faixa_46_a_55_anos +
  Pessoa13_ES$Faixa_56_a_65_anos +
  Pessoa13_ES$Faixa_66_a_75_anos +
  Pessoa13_ES$Faixa_76_a_85_anos +
  Pessoa13_ES$Faixa_86_a_95_anos +
  Pessoa13_ES$Faixa_Mais_que_96_anos


Pessoa13_ES$Faixa_1_a_35_anos <- 
Pessoa13_ES$Faixa_17_a_35_anos +
Pessoa13_ES$Faixa_16_a_25_anos  +
Pessoa13_ES$Faixa_6_a_15_anos +
Pessoa13_ES$Faixa_1_a_5_anos 

# V022	V035	V036	V037	V038	V039	1_a_5_anos
# V040	V041	V042	V043	V044	V045	V046	V047	V048	V049	6_a_15_anos	
# V050	V051	V052	V053	V054	V055	V056	V057	V058	V059	16_a_25_anos
# V060	V061	V062	V063	V064	V065	V066	V067	V068	V069	17_a_35_anos
# V070	V071	V072	V073	V074	V075	V076	V077	V078	V079	36_a_45_anos
# V080	V081	V082	V083	V084	V085	V086	V087	V088	V089	46_a_55_anos
# V090	V091	V092	V093	V094	V095	V096	V097	V098	V099	56_a_65_anos
# V100	V101	V102	V103	V104	V105	V106	V107	V108	V109	66_a_75_anos
# V110	V111	V112	V113	V114	V115	V116	V117	V118	V119	76_a_85_anos
# V120	V121	V122	V123	V124	V125	V126	V127	V128	V129	86_a_ 95_anos


setoresVVFaixa_de_idade <-
  setoresESVilaVelha  %>%
  left_join(Pessoa13_ES,by=c('code_tract'='Cod_setor'))

setoresVV_idade_maiorque65 <-
  setoresESVilaVelha  %>%
  left_join(Pessoa13_ES,by=c('code_tract'='Cod_setor'))

setoresVV_idade_1_a_35 <-
  setoresESVilaVelha  %>%
  left_join(Pessoa13_ES,by=c('code_tract'='Cod_setor'))



setoresVV_idade_maiorque45 <-
  setoresESVilaVelha  %>%
  left_join(Pessoa13_ES,by=c('code_tract'='Cod_setor'))



#mapas temáticos - Faixa de idade por setor censitário




tmap_mode("view")
tm_shape(shp = setoresVV_idade_maiorque65)+
  tm_fill("Faixa_Mais_que_65_anos",   
          alpha = 0.8,
          title="Pessoas com idade maior que 65  anos de idade por setor censitário",
          tyle = "equal", 
          textNA = "sem dados no arquivo de Censo",
          breaks = c(0,15, 35, 50, 75, 100,  150),
          palette = c("white",
                             "green",
                             "cyan",
                             "yellow",
                             "pink",
                             "violet",
                             "red")                      
  ) +
  tm_borders(alpha = 0.1)+
  # tm_compass(type = "4star", size = 2, position = c("left", "top"))
  
  tm_shape(shp = vv) +
  tm_borders(alpha = 0.8)+
  
  tm_shape(shp = sf_HospitaisVV) +
  tm_dots(palette = "Set2", 
          col = "nomehospital",
          id = "nomehospital", # bold in popup
          popup.vars = c("Tipo:" = "tipohospital", "Hospital:" = "nomehospital"), # light in popup
          title="Principais Hospitais de Vila Velha",
          size = 0.2)+
  
  tm_shape(shp =intersectsetorvv) +
  tm_borders(alpha = 0.8)



tmap_mode("view")
tm_shape(shp = setoresVV_idade_1_a_35)+
  tm_fill("Faixa_1_a_35_anos",   
          alpha = 0.8,
          title="pessoas com idade entre 1 a 35 anos",
          tyle = "equal", 
          textNA = "sem dados no arquivo de Censo",
          breaks = c(0,50, 100, 200, 500, 1000,  1800),
          palette = c("white",
                             "green",
                             "cyan",
                             "yellow",
                             "pink",
                             "violet",
                             "red")                      
  ) +
  tm_borders(alpha = 0.1)+
  # tm_compass(type = "4star", size = 2, position = c("left", "top"))
  
  tm_shape(shp = vv) +
  tm_borders(alpha = 0.8)+
  
  tm_shape(shp = sf_HospitaisVV) +
  tm_dots(palette = "Set2", 
          col = "nomehospital",
          id = "nomehospital", # bold in popup
          popup.vars = c("Tipo:" = "tipohospital", "Hospital:" = "nomehospital"), # light in popup
          title="Principais Hospitais de Vila Velha",
          size = 0.2)+
  
  tm_shape(shp =intersectsetorvv) +
  tm_borders(alpha = 0.8)






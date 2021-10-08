library(readr)
library(tidyverse)
library(taxize)

### EDITANDO CADA ALUNO ###

## CARLOS
CARLOS <- read_delim("atividade1_carlos_filgueira.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
# Renomear colunas e formatar coordenadas
colnames(CARLOS) <- c("pdf", "amostra", "site", "lat", "lon", "date", "Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
CARLOS$lat <- sapply(CARLOS$lat, gsub, pattern = "\\.", replacement= "")
CARLOS$lat <- as.numeric(CARLOS$lat)
CARLOS$lon <- sapply(CARLOS$lon, gsub, pattern = "\\.", replacement= "")
CARLOS$lon <- as.numeric(CARLOS$lat)
CARLOS$lat <- format(CARLOS$lat , digits=8,
                      big.mark=".",small.mark=".", big.interval=6)
CARLOS$lon <- format(CARLOS$lon , digits=8,
                     big.mark=".",small.mark=".", big.interval=6)

## CAROLINA
CAROLINA <- read_delim("atividade1_CAROLINA-OLIVEIRA-VALE.csv", 
                                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
# Renomear colunas e formatar coordenadas e data
colnames(CAROLINA) <- c("pdf", "amostra", "Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "site", "lat", "lon", "date")
CAROLINA$Species <- sapply(CAROLINA$Species, gsub, pattern = "_", replacement= " ")
CAROLINA$lat <- sapply(CAROLINA$lat, gsub, pattern = "\\.", replacement= "")
CAROLINA$lat <- as.numeric(CAROLINA$lat)
CAROLINA$lon <- sapply(CAROLINA$lon, gsub, pattern = "\\.", replacement= "")
CAROLINA$lon <- as.numeric(CAROLINA$lat)
CAROLINA$lat <- format(CAROLINA$lat , digits=8,
                     big.mark=".",small.mark=".", big.interval=6)
CAROLINA$lon <- format(CAROLINA$lon , digits=8,
                     big.mark=".",small.mark=".", big.interval=6)
CAROLINA$date <- factor(CAROLINA$date) %>% 
  as.Date(format = "%Y-%m-%d") %>% 
  format("%d/%m/%Y")


## ISABELLA
ISABELLA <- read_delim("atividade1_ISABELLA-FERREIRA.csv", 
                                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
# Renomear colunas
colnames(ISABELLA) <- c("pdf", "amostra", "site", "lat", "lon", "date", "Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

#MARINA
MARINA <- read_delim("atividade1_marinasissini.csv", 
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
# Renomear colunas e formatar data
colnames(MARINA) <- c("pdf", "amostra", "site", "lat", "lon", "date", "Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
MARINA$date <- factor(MARINA$date) %>% 
               as.Date(format = "%Y-%m-%d") %>% 
               format("%d/%m/%Y")


#NILSON DATA
NILSON <- read_delim("atividade1_NILSON-BERRIEL.csv", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
# Renomear colunas , formatar nome da especie e data
colnames(NILSON) <- c("pdf", "amostra", "Species","site", "lat", "lon", "date", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
NILSON$Species <- sapply(NILSON$Species, gsub, pattern = "_", replacement= " ")
NILSON$date <- factor(NILSON$date) %>% 
  as.Date(format = "%Y-%m-%d") %>% 
  format("%d/%m/%Y")

## GUSTAVO
GUSTAVO <- read_delim("atividade1_GUSTAVO_VIANA.csv", 
                                       delim = ";", escape_double = FALSE, col_types = cols(Latitude = col_number(), Longitude = col_number(), `Sepal length (cm)` = col_character(), 
                                                                                            `Sepal width (cm)` = col_character(), `Petal length (cm)` = col_character()), trim_ws = TRUE)
# Renomear colunas, formatar especie, coordenadas, substituir virgula por ponto
colnames(GUSTAVO) <- c("pdf", "amostra", "site", "lat", "lon", "date", "Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
GUSTAVO$Species <- sapply(GUSTAVO$Species, gsub, pattern = "_", replacement= " ")
GUSTAVO$lat <- format(GUSTAVO$lat , digits=8,
         big.mark=".",small.mark=".", big.interval=6)
GUSTAVO$lon <- format(GUSTAVO$lon , digits=8,
                           big.mark=".",small.mark=".", big.interval=6)
GUSTAVO$Sepal.Length <- sapply(GUSTAVO$Sepal.Length, gsub, pattern = ",", replacement= ".")
GUSTAVO$Sepal.Width <- sapply(GUSTAVO$Sepal.Width, gsub, pattern = ",", replacement= ".")
GUSTAVO$Petal.Length <- sapply(GUSTAVO$Petal.Length, gsub, pattern = ",", replacement= ".")
GUSTAVO$Petal.Width <- sapply(GUSTAVO$Petal.Width, gsub, pattern = ",", replacement= ".")

## MARCOS
MARCOS <- read_delim("atividade1_marcosdelucena.csv", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
colnames(MARCOS) <- c("pdf", "amostra", "site", "lat", "lon", "date", "Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
MARCOS$lat <- format(MARCOS$lat , digits=8,
                      big.mark=".",small.mark=".", big.interval=6)
MARCOS$lon <- format(MARCOS$lon , digits=8,
                      big.mark=".",small.mark=".", big.interval=6)

## GABRIEL
GABRIEL <- read_delim("atividade1_GABRIEL-DEPIANTTI.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
colnames(GABRIEL) <- c("amostra", "site", "Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "lat", "lon", "date")
GABRIEL$pdf <- "NA"
GABRIEL$lat <- sapply(GABRIEL$lat, gsub, pattern = "\\.", replacement= "")
GABRIEL$lat <- as.numeric(GABRIEL$lat)
GABRIEL$lon <- sapply(GABRIEL$lon, gsub, pattern = "\\.", replacement= "")
GABRIEL$lon <- as.numeric(GABRIEL$lat)
GABRIEL$lat <- format(GABRIEL$lat , digits=8,
                     big.mark=".",small.mark=".", big.interval=6)
GABRIEL$lon <- format(GABRIEL$lon , digits=8,
                     big.mark=".",small.mark=".", big.interval=6)

## VANESSA
VANESSA <- read_delim("atividade1_Vanessa Xavier.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
colnames(VANESSA) <- c("Species", "pdf" , "amostra", "site", "lat", "lon", "date", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
VANESSA$lat <- format(VANESSA$lat , digits=8,
                     big.mark=".",small.mark=".", big.interval=6)
VANESSA$lon <- format(VANESSA$lon , digits=8,
                     big.mark=".",small.mark=".", big.interval=6)

## PEDRO
PEDRO <- read_delim("atividade1_pedrozau.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

colnames(PEDRO) <- c("pdf", "amostra", "Species", "site", "lat", "lon", "date",  "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
PEDRO$Species <- sapply(PEDRO$Species, gsub, pattern = "_", replacement= " ")

## Tabela final
iris <-rbind(CARLOS, CAROLINA, GABRIEL, GUSTAVO, ISABELLA,MARCOS, MARINA, NILSON, VANESSA, PEDRO) %>% 
  drop_na(amostra)

lapply(iris, unique)

## check taxa
species <- iris %>% 
  distinct(Species) %>% 
  pull() %>% 
  get_tsn() %>% 
  data.frame() %>% 
  bind_cols(iris %>% 
              distinct(Species))

iris_1 <- iris %>% 
  dplyr::mutate(eventID = paste(site, date, sep = "_"), # create indexing fields 
                occurrenceID = paste(site, date, amostra, sep = "_")) %>% 
  left_join(species %>% 
              select(Species, uri)) %>% # add species unique identifier
  dplyr::rename(decimalLongitude = lon, # rename fields according to DwC 
                decimalLatitude = lat,
                eventDate = date,
                scientificName = Species,
                scientificNameID = uri) %>% 
  mutate(geodeticDatum = "WGS84", # and add complimentary fields
         verbatimCoordinateSystem = "decimal degrees",
         georeferenceProtocol = "Random coordinates obtained from Google Earth",
         locality = "Gaspe Peninsula",
         recordedBy = "Edgar Anderson",
         taxonRank = "Species",
         organismQuantityType = "individuals",
         basisOfRecord = "Human observation")

## create eventCore
eventCore <- iris_1 %>% 
  select(eventID, eventDate, decimalLongitude, decimalLatitude, locality, site,
         geodeticDatum, verbatimCoordinateSystem, georeferenceProtocol) %>% 
  distinct()

## create occurrence
occurrences <- iris_1 %>% 
  select(eventID, occurrenceID, scientificName, scientificNameID,
         recordedBy, taxonRank, organismQuantityType, basisOfRecord) %>%
  distinct() 

## create measurementsOrFacts
eMOF <- iris_1 %>% 
  select(eventID, occurrenceID, recordedBy, Sepal.Length:Petal.Width) %>%  
  pivot_longer(cols = Sepal.Length:Petal.Width,
               names_to = "measurementType",
               values_to = "measurementValue") %>% 
  mutate(measurementUnit = "cm",
         measurementType = plyr::mapvalues(measurementType,
                                           from = c("Sepal.Length", "Sepal.Width", "Petal.Width", "Petal.Length"), 
                                           to = c("sepal length", "sepal width", "petal width", "petal length")))

# check if all eventID matches
setdiff(eventCore$eventID, occurrences$eventID)
setdiff(eventCore$eventID, eMOF$eventID)
setdiff(occurrences$eventID, eMOF$eventID)

# check NA values
eMOF %>%
  filter(is.na(eventID))

occurrences %>%
  filter(is.na(eventID))


rm(list = setdiff(ls(), c("eventCore", "occurrences", "eMOF")))

files <- list(eventCore, occurrences, eMOF) 
data_names <- c("DF_eventCore","DF_occ","DF_eMOF")
dir.create("Dwc_Files")


for(i in 1:length(files)) {
  path <- paste0(getwd(), "/", "DwC_Files")
  write.csv(files[[i]], paste0(path, "/", data_names[i], ".csv"))
}
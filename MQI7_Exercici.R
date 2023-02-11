# 1. CREATE NEW PROJECT!!!!!    ---->>>>

# 2. LOAD FILES
library(readxl)
library(dplyr)
library(janitor)

# 3. DOWNLOAD DATA

#-- Choose 3.1 OR 3.2

## 3.1. MANUAL DOWNLOADING
#Go to: https://infoelectoral.interior.gob.es/opencms/es/elecciones-celebradas/area-de-descargas/
# Otras Descargas -> Datos de Municipios -> Congreso 2019 10 de Noviembre -> Descargar

## 3.2. AUTOMATIC (it might not work)
download.file("https://infoelectoral.interior.gob.es/estaticos/docxl/02_201911_1.zip",
              "02_201911_1.zip")
unzip("02_201911_1.zip")

## 3.3. READ THE FILE (it should be in your working directory)
elecc19 <- read_xlsx("02_201911_1.xlsx", skip = 5) |> 
  clean_names()

# 4. GLIMPSE DATA
elecc19
glimpse(elecc19)


# 5. EXERCISE
# Student:
# Question: 
# Code (at least 3 dplyr functions):
elecc19

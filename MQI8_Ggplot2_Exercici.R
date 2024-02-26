#EXERCICI GGPLOT2 (2024)

# 1. ACTIVITAT: L'activitat consisteix a fer un plot amb ggplot2:
#- El plot es crea a partir d'un filtre o bé d'un resum previ de dades a través de les funcions group_by() i summarize().
#- El plot ha de mostrar com a mínim dues variables.
#- L'estudiant ha de fer una captura de pantalla.
#- Es valora la relació entre pregunta i gràfic, l'originalitat del tema i l'ús de capes extres.



# 2. PUNTUACIÓ: Puntuació de l'exercici (sobre 1.0):
# 0.2 Compleix normes (captura, nom, pregunta, codi vist a l'aula, consola)
# 0.2 El plot conté un filtre o un group_by() i summarize()
# 0.1 El plot dels inclou dues (o més) variables
# 0.2 El gràfic permet donar resposta amb suficient nitidesa a la pregunta
# 0.2 El tema és original
# 0.1 S'utilitza alguna capa 'extra'


# 3. DATASETS
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(foreign)
library(haven)


# 2. IDESCAT
## Nota: Els csv d'Idescat normalment es descarreguen amb read_csv2 i eliminant les primeres línies (argument skip = 5 o indicar el nombre de línies fins que concordi)

# Renda familiar disponible:
# https://www.idescat.cat/pub/?id=rfdbc&n=13301&by=mun


# Generació de residus per càpita
# https://www.idescat.cat/pub/?id=resmc&n=6997&by=mun


# Codis de municipis i comarques
# https://www.idescat.cat/codis/?id=50&n=9


#3. CENTRE D'ESTUDIS D'OPINIÓ

#Enquesta CEO febrer 2024
#https://ceo.gencat.cat/ca/estudis/registre-estudis-dopinio/estudis-dopinio-ceo/societat/detall/index.html?id=9088

#Al qüestionari veiem les variables del marc de dades
#https://upceo.ceo.gencat.cat/wsceop/9088/Qüestionari_1078.pdf

#Descarregar dades CEO per PC
ceo1078 <- read_csv2("https://upceo.ceo.gencat.cat/wsceop/9088/Microdades_anonimitzades_1078.csv")

#Descarregar dades CEO per Mac
ceo1078 <- read_csv2("https://upceo.ceo.gencat.cat/wsceop/9088/Microdades_anonimitzades_1078.csv",
                     locale = locale(encoding = "ISO-8859-1"))



# 4. ESTATS

#Quality of Government Institute: https://www.gu.se/en/quality-government/qog-data/data-downloads/basic-dataset
qog_cs <- read_csv("https://www.qogdata.pol.gu.se/data/qog_bas_cs_jan23.csv") #cross-section
qog_ts <- read_csv("https://www.qogdata.pol.gu.se/data/qog_bas_ts_jan23.csv") #time-series


#GAME OF THRONES
#https://github.com/jeffreylancaster/game-of-thrones
#https://github.com/aljrico/gameofthrones
got <- read_csv("https://raw.githubusercontent.com/Kevogich/Game-Of-Thrones/master/5kings_battles_v1.csv")

#TITANIC
#https://github.com/datasciencedojo/datasets/blob/master/titanic.csv
titanic <- read_csv("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")



#MUSIC
#https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-21/readme.md
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
rs <- read_csv("https://raw.githubusercontent.com/jmoro0408/Spotify_Analysis_2.0/main/Data/rollingstones500.csv")
count(rs, Year, sort = T) |> 
  ggplot(aes(x = Year, y = n)) +
  geom_line()

#OTHERS
# Busqueu el CSV i entreu a l'arxiu -> Download -> Copieu l'adreça de la pàgina i importeu-la amb read_csv

# Food consumption and CO2 emissions: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-02-18/readme.md
# Tour de France winners: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-07/readme.md
# Human Rights Violations: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-21/readme.md
# Nobel Prize Winners: https://github.com/rfordatascience/tidytuesday/blob/master/data/2019/2019-05-14/nobel_winners.csv
# Anime: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-04-23
# Board Games: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-03-12
# IMdb: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-01-08
imdb <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")
#Comic characters: https://github.com/rfordatascience/tidytuesday/tree/master/data/2018/2018-05-29
comic <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2018/2018-05-29/week9_comic_characters.csv")
# Friends: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-09-08/readme.md
# Hollywood age gaps: https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-02-14/readme.md
# Freedom in the World: 
freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')
# Eurovision: https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-05-17/readme.md
# Stranger Things dialogues: https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-10-18
stranger_things <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv")


dplyr::starwars
ggplot2::diamonds
ggplot2::presidential
tidyr::world_bank_pop






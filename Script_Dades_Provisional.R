library(dplyr)
library(ggplot2)
library(readxl)
library(foreign)
library(haven)


#IDESCAT

# Renda familiar disponible:
# https://www.idescat.cat/pub/?id=rfdbc&n=13301&by=mun


# Generació de residus per càpita
# https://www.idescat.cat/pub/?id=resmc&n=6997&by=mun


# Codis de municipis i comarques
# https://www.idescat.cat/codis/?id=50&n=9


# 3. ESTATS

#https://www.gu.se/en/quality-government/qog-data/data-downloads/basic-dataset
read_csv("https://www.qogdata.pol.gu.se/data/qog_bas_cs_jan23.csv") #cross-section
read_csv("https://www.qogdata.pol.gu.se/data/qog_bas_ts_jan23.csv") #time-series


#GAME OF THRONES
#https://github.com/jeffreylancaster/game-of-thrones
#https://github.com/aljrico/gameofthrones
got <- read_csv("https://raw.githubusercontent.com/Kevogich/Game-Of-Thrones/master/5kings_battles_v1.csv")

#Titanic
#https://github.com/datasciencedojo/datasets/blob/master/titanic.csv
Titanic
read_csv("data/titanic.csv")
titanic <- read_csv("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")



#SPOTIFY
#https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-21/readme.md
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
rs <- read_csv("https://raw.githubusercontent.com/jmoro0408/Spotify_Analysis_2.0/main/Data/rollingstones500.csv")
count(rs, Year, sort = T) |> 
  ggplot(aes(x = Year, y = n)) +
  geom_line()

# Food consumption and CO2 emissions: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-02-18/readme.md
# Tour de France winners: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-07/readme.md
# Human Rights Violations: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-21/readme.md
# Nobel Prize Winners: https://github.com/rfordatascience/tidytuesday/blob/master/data/2019/2019-05-14/nobel_winners.csv
# Anime: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-04-23
# Board Games: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-03-12
# IMdb: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-01-08
imdb <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")
#Comic characters
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



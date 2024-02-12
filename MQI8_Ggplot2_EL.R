#####################################################################
########################## GGPLOT2 EL ###############################
#####################################################################



#####################################################################
# PART 0. LEARNING CURVE
#####################################################################

# R learning curve
library(dplyr)
rlc <- tibble(class = 1:8,
              knowledge = (1:8)^3)
lesson <- 7
rlc |> 
  ggplot(aes(x = class, y = knowledge)) +
  geom_line() +
  geom_point(data = rlc |> 
               filter(class == lesson), col = "red", size = 3)



#####################################################################
# PART 1. SUMMARY
#####################################################################

# The dplyr package is THE R PACKAGE
# So let's load it
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(forcats)
library(readr)
library(lubridate)
theme_set(theme_minimal())
cens_gc <- readRDS("data/cens_gc.rds")
rendacs <- readRDS("data/rendacs.rds")
accidents <- readRDS("data/accidents.rds")
municipi <- readRDS("data/municipi.rds")
lloguer_any <- readRDS("data/lloguer_any.rds")
festivals <- readRDS("data/festivals.rds")
elecc19 <- readRDS("data/elecc19.rds")


# Let's load the datasets

accidents #UO: Accident
contractes_menors #UO: contracte
festivals #UO: festival
municipi #UO: municipi
lloguer_any #UO: ??
rendacs #UO: ??
cens_gc #UO: individu
elecc19 # UO: municipi



#####################################################################
# PART 2. OTHER GGPLOT2 LAYERS
#####################################################################


# 2.1. FACET

## 2.1.1. COLS
# facet_wrap(facets = vars( ), ncol = 1)

elecc19 |> 
  filter(nombre_de_comunidad == "Cataluña") |> 
  mutate(pp_per = pp / total_votantes * 100) |> 
  ggplot(aes(x = pp_per)) +
  geom_histogram()

  
## 2.1.2. ROWS
# facet_wrap(facets = vars( ), nrow = 1)

elecc19 |> 
  filter(nombre_de_comunidad == "Cataluña") |> 
  mutate(erc_per = erc_sobiranistes / total_votantes * 100,
         psoe_per = psoe / total_votantes * 100,
         hab = if_else(poblacion > 15000, "Ciutat", "Poble")) |> 
  ggplot(aes(x = erc_per, y = psoe_per, col = hab)) +
  geom_point()


## 2.1.3. NONE
#facet_wrap(facets=vars( ))

library(lubridate)
covid <- readRDS("data/covid.rds")
covid |> 
  filter(NOM != "Sense especificar") |> 
  mutate(DATA = as.Date(DATA, format = "%d/%m/%Y"),
         MONTH = month(DATA),
         YEAR = year(DATA)) |> 
  group_by(NOM, MONTH, YEAR) |> 
  summarize(casos = sum(CASOS_CONFIRMAT)) |> 
  mutate(DATE = as.Date(paste0("01/", MONTH, "/", YEAR), format = "%d/%m/%Y")) |> 
  ggplot(aes(x = DATE, y = casos)) +
  geom_line() +
  facet_wrap(facets = vars(NOM), scales = "free")


## 2.1.4. FACET GRID
#facet_grid(rows = vars(), cols = vars())

accidents |>
  ggplot(aes(x = edat)) +
  geom_histogram()


# 2.2. COORDINATES

# coord_flip()
elecc19 |>
  filter(nombre_de_comunidad == "Andalucía") |> 
  mutate(cs_per = cs / poblacion * 100) |> 
  ggplot(aes(x = fct_reorder(nombre_de_provincia, cs_per), y = cs_per)) +
  geom_violin() +
  coord_flip()

  
# coord_cartesian(xlim = c( , ), ylim = c( , ))
rendacs |> 
  ggplot(aes(x = import_euros, y = index_gini, col = nom_districte)) +
  geom_point() +
  coord_cartesian(ylim = c(30, 40))


# 2.3. SCALES


## 2.3.1. NUMERIC: scale_x_continuous()
rendacs |> 
  ggplot(aes(x = import_euros, y = index_gini, col = nom_districte)) +
  geom_point() +
  scale_x_continuous(
    breaks = c(20000, 50000, 80000), 
    labels = c("20k", "50k", "80k"),
    name = "Mitjana ingrés (€)"
  )


## 2.3.2. CATEGORICAL: scale_x_discrete()
elecc19 |> 
  filter(nombre_de_comunidad == "Cataluña") |> 
  ggplot(aes(x = nombre_de_provincia, y = numero_de_mesas)) +
  geom_col() +
  scale_x_discrete(labels = c("BCN", "GIR", "LLEI", "TGN"),
                   name = "Província")


## 2.3.3. PERCENTAGE: scale_y_continuous(labels = scales::label_percent())
elecc19 |> 
  filter(nombre_de_comunidad == "Cataluña") |> 
  mutate(erc_per = erc_sobiranistes / total_votantes * 100,
         psoe_per = psoe / total_votantes * 100,
         hab = if_else(poblacion > 15000, "Ciutat", "Poble")) |> 
  ggplot(aes(x = erc_per, y = psoe_per, col = hab)) +
  geom_point()


## 2.3.4. COL/FILL

#scale_fill_brewer(palette = 1, direction = 1)
elecc19 |> 
  filter(nombre_de_comunidad == "Cataluña") |> 
  ggplot(aes(x = nombre_de_provincia, y = numero_de_mesas,
             fill = nombre_de_provincia)) +
  geom_col() 


#scale_color_gradient(low = "white", high = "black")
elecc19 |> 
  filter(nombre_de_comunidad == "Cataluña") |> 
  mutate(erc_per = erc_sobiranistes / total_votantes,
         psoe_per = psoe / total_votantes,
         pp_per = pp / total_votantes,
         hab = if_else(poblacion > 15000, "Ciutat", "Poble")) |> 
  ggplot(aes(x = erc_per, y = psoe_per, col = pp_per)) +
  geom_point()


#scale_fill_manual(values, labels)
elecc19 |> 
  filter(nombre_de_comunidad == "Cataluña") |> 
  mutate(erc_per = erc_sobiranistes / total_votantes,
         psoe_per = psoe / total_votantes,
         hab = if_else(poblacion > 15000, "Ciutat", "Poble")) |> 
  ggplot(aes(x = erc_per, y = psoe_per, col = hab)) +
  geom_point() +
  scale_color_manual(values = c("orange", "darkgreen"),
                     labels = c("Ciutat", "Poble"))


# 2.4. LABELS AND THEMES

## Labels: labs(title, subtitle, caption, x, y)

## Themes: theme_classic(), theme_minimal()

## Ggthemes: scale_color_wsj
library(ggthemes)
rendacs |> 
  ggplot(aes(x = import_euros, y = index_gini, col = nom_districte)) +
  geom_point()



#####################################################################
# PART 3. EXERCISES
#####################################################################



# PREGUNTES

#1. A Euskadi, com està distribuït per municipis el vot a Bildu segons cada província?
  

#2. En quants municipis de cada provincia d'Espanya, els vots al PACMA van superar el 2% dels vots?


#3. En quin municipi de cada CCAA hi ha hagut menys abstenció?


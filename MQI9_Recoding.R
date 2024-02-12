#####################################################################
############################## DPLYR ################################
#####################################################################



#####################################################################
# PART 0. LEARNING CURVE
#####################################################################

# R learning curve
library(dplyr)
library(ggplot2)
rlc <- tibble(class = 1:8,
              knowledge = (1:8)^3)
lesson <- 8
rlc |> 
  ggplot(aes(x = class, y = knowledge)) +
  geom_line(size = 1) +
  geom_point(data = rlc |> 
               filter(class == lesson), col = "red", size = 4) +
  annotate("text", x = 4.5, y = 375, label = "You are here!") +
  geom_curve(aes(x = 5, xend = 7.8, y = 400, yend = 515),
             arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
             color = "gray40", curvature = -0.3) +
  theme_minimal() +
  labs(x = "Class number", y = "Knowledge", title = "The R Knowledge curve")



#####################################################################
# PART 1. SUMMARY
#####################################################################

#Load packages
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(forcats)
library(readxl)



#####################################################################
# PART 2. BOOLEAN OPERATORS
#####################################################################


## 2.1. BOOLEAN OPERATORS

#    - AND: & (totes les condicions presents)
#    - OR: | (qualsevol condició present)
#    - NOT: ! (retorna el contrari de la condició)

ctr_pov <- tibble(country = c("Armenia", "Austria", "Benin", "Bolivia",
                              "Brazil", "Colombia", "El Salvador",
                              "Ethiopia", "Honduras", "Indonesia"),
                  continent = c("ASI", "EUR", "AFR", "AME", "AME", "AME", 
                                "AME", "AFR", "AME", "ASI"),
                  poverty = c(1.90, 0.7, 49.6, 6.4, 3.4, 4.5, 
                              1.9, 26.7, 16.2, 7.2))

# 1. The operator AND is more restrictive than the operator OR

# African countries AND poverty line above 10%  
ctr_pov |> 
  filter(continent == "AFR" & poverty > 10)

# African countries OR poverty line above 10%
ctr_pov |> 
  filter(continent == "AFR" | poverty > 10)



# 2. The operator NOT negates the logical condition

# American / European countries AND poverty line below 30%
ctr_pov |> 
  filter(continent %in% c("AFR", "ASI") & poverty < 30)

# NOT American / European countries AND poverty line below 30%
ctr_pov |> 
  filter(!continent %in% c("AFR", "ASI") & poverty < 30)



#####################################################################
# PART 3. RELABEL FUNCTIONS
#####################################################################


## 3.1. IF_ELSE (dplyr)
#    - if_else(condition, if TRUE, if FALSE)


#Ens hem adonat que no sabem si els accidents es van produir o no en cap de setmana
accidents |> 
  glimpse()

unique(accidents$descripcio_dia_setmana)
accidents$edat

accidents |> 
  select(nom_districte, nom_barri, descripcio_dia_setmana) |> 
  ggplot(aes(x = descripcio_dia_setmana)) +
  geom_bar()

accidents |> 
  select(nom_districte, nom_barri, descripcio_dia_setmana) |> 
  mutate(wk = if_else(descripcio_dia_setmana %in% c("Dissabte", "Diumenge"),
                      "Cap de setmana", "Entre setmana")) |> 
  ggplot(aes(x = wk)) +
  geom_bar()


#Volem saber quines persones van desaparèixer concretament a Catalunya durant la guerra civil
cens_gc |> 
  glimpse() 

cens_gc |>
  mutate(cat_desaparicio = if_else(provincia_desaparicio %in% c("Tarragona", "Barcelona",
                                                                "Girona", "Lleida"), "Catalunya", "Altres")) |> 
  ggplot(aes(x = cat_desaparicio)) +
  geom_bar(aes(fill = as.character(localitzat)), position = "dodge")



#Una opció sempre és recodificar NOMÉS ALGUNES variables (com a FALSE, indiquem la variable)
cens_gc |>
  mutate(cat_desaparicio = if_else(provincia_desaparicio %in% c("Tarragona", "Barcelona",
                                                                "Girona", "Lleida"), 
                                   "Catalunya", provincia_desaparicio)) |> 
  glimpse()






#### ExErSiSe!!!!

#Dicotomitza les seccions censals de Barcelona segons si són riques (què vol dir ser rica? -> validesa!)
rendacs
range(rendacs$import_euros)
hist(rendacs$import_euros)


# Per casa: Dicotomitza segons tipus de vehicle: 
### - 4 rodes vs altres
### - motor vs altres
accidents
unique(accidents$desc_tipus_vehicle_implicat)




## 3.3. CASE_WHEN (dplyr)
#    - case_when(condition1 ~ if TRUE,
#                condition2 ~ if TRUE,
#                condition3 ~ if TRUE,
#                ..., 
#                .default = others)

polity <- tibble(country = c("United States", "Bolivia", "Australia", "Azerbaijan",
                             "USSR", "Timor Leste", "Eritrea", "Qatar", "Gambia"),
                 year = c(1776, 1825, 1901, 1991, 1922, 2002, 1993, 1971, 1965),
                 polity2 = c(0, -3, 10, -3, -7, 6, -6, -10, 8))
polity

polity <- polity |> 
  mutate(polity_dic = case_when(polity2 > 5 ~  "Democracy", 
                                polity2 > -5 ~  "Anocracy",
                                .default = "Autocracy"))

#Does exactly the same operation:
#polity$polity_dic <- case_when(polity$polity2 > 5 ~  "Democracy", 
#                               polity$polity2 > -5 ~  "Anocracy",
#                               TRUE ~ "Autocracy")

polity <- polity |> 
  mutate(century = case_when(year < 1800 ~  "18c", 
                             year < 1900 ~  "19c",
                             year < 2000 ~  "20c",
                             .default = "21c"))

# Veure la mitjana de democràcia per dècada:
library(vdemdata)
vdem[,1:70] |> glimpse()
vdem_sub <- vdem |> 
  filter(year > 1949) |> 
  select(country_name, country_text_id, year, v2x_libdem) |> 
  tibble()

vdem_sub |> 
  mutate(decade = case_when(year < 1960 ~ "1950s",
                            year < 1970 ~ "1960s",
                            year < 1980 ~ "1970s",
                            year < 1990 ~ "1980s",
                            year < 2000 ~ "1990s",
                            year < 2010 ~ "2000s",
                            year < 2020 ~ "2010s",
                            .default = "2020s")) |> 
  group_by(decade) |> 
  summarize(libdem = mean(v2x_libdem, na.rm = T)) |> 
  ggplot(aes(x = decade, y = libdem)) +
  geom_col()



## 3.3. CASE_MATCH (dplyr)
# change a particular value of a variable

#    - case_match(variable,
#                 "old value1" ~ "new value1",
#                 "old value2" ~ "new value2",
#                 ..., 
#                 .default = others)



#Veiem que Sants-Montjuïc no està ben posat:
accidents |> 
  glimpse()
unique(accidents$nom_districte)

accidents |> 
  mutate(nom_districte = case_match(nom_districte, 
                                    "Sants-Montjuďc" ~ "Sants-Montjuïc",
                                    .default = nom_districte)) |> 
  glimpse()






#####################################################################
# PART 3. FACTORS
#####################################################################



## 3.4. FACTOR
#    - factor(vector, ordered = TRUE, c("Low", "High"))

factor(polity$century,
       ordered = TRUE,
       c("18c", "19c", "20c", "21c"))



# Ordenar dies de la setmana
accidents |> 
  count(hora_dia, descripcio_dia_setmana) |> 
  ggplot(aes(x = hora_dia, y = descripcio_dia_setmana, fill = n)) +
  geom_tile()





### ---- QUIN ÉS EL PARLAMENT NACIONAL DE CADA REGIÓ AMB MÉS DONES?

#Download Data
#Web: https://www.sdgindex.org/reports/sustainable-development-report-2022/
#Automatic, to the folder "data"
download.file("https://github.com/sdsna/SDR2022/raw/main/SDR-2022-database.xlsx",
              "data/SDR-2022-Database.xlsx")

#Descarreguem Codebook, que es troba al Full 3
sdg_codebook <- read_xlsx("data/SDR-2022-Database.xlsx", sheet = 3) |> 
  select(IndCode, Indicator, Description)

sdg_codebook[str_detect(sdg_codebook$Indicator, "women"),] #creem un buscador, busquem "women"

#Descarreguem les dades, que es troben al Full 5
sdg_data <- read_xlsx("data/SDR-2022-Database.xlsx", sheet = 5)
unique(sdg_data$indexreg)
sdg_data |> 
  mutate(indexreg = case_match(indexreg, 
                               "E. Europe & C. Asia" ~ "Europa no-OCDE i Àsia Central",
                               "MENA" ~ "Orient Mitjà i Nord Àfrica",
                               "Sub-Saharan Africa" ~ "Àfrica Sub-sahariana",
                               "LAC" ~ "Amèrica Llatina i Carib", 
                               "East & South Asia" ~ "Sudest Asiàtic",
                               "OECD" ~ "OCDE",
                               .default = indexreg)) |>
  group_by(indexreg) |> 
  filter(sdg5_parl == max(sdg5_parl)) |> 
  ggplot(aes(x = fct_reorder(indexreg, sdg5_parl), y = sdg5_parl)) +
  geom_point(col = "#e1eedd", size = 15) +
  geom_text(aes(label = id)) +
  coord_flip() +
  theme_minimal() +
  labs(x = NULL, y = "% de dones al parlament estatal") +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14))

#Quina relació hi ha entre les dones al parlament i la mida del país? Cap
sdg_data |> 
  ggplot(aes(x = log10(pop_2021), y = sdg5_parl)) +
  geom_text(aes(label = id), size = 2) +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = c("10k", "100k", "1M", 
                                "10M", "100M", "1000M")) +
  theme_minimal() +
  labs(x = "Població (log10)",
       y = "Dones al parlament (%)",
       title = "Percentatge de dones als parlaments nacionals (2021)")




# ExErCiCi
## Una categoria està malament. Canvia-la.
unique(accidents$descripcio_causa_vianant)





## 3.6. AS.XXXXXX
#    - as.numeric(vector)
#    - as.integer(vector)
#    - as.factor(vector)
#    - as.character(vector)
#    - as.Date(vector)

#Ens trobem variables d'un tipus que no ho haurien de ser
polity_new <- polity |>
  mutate(across(country:polity2, ~ as.character(.)))

#Per tant, no podrem fer les operacions que se suposa que hauríem de fer
sum(polity_new$year) #dona error
mean(polity_new$polity2) #dona error

#Les hem de transformar!
polity_new$year <- as.numeric(polity_new$year)
polity_new$polity2 <- as.numeric(polity_new$polity2)




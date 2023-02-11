#####################################################################
############################ GGPLOT2 ################################
#####################################################################



#####################################################################
# PART 0. LEARNING CURVE
#####################################################################

# R learning curve
library(dplyr)
rlc <- tibble(class = 1:8,
             knowledge = (1:8)^3)
class <- 6
plot(rlc$class, rlc$knowledge)
points(rlc$class[class], rlc$knowledge[class], col = "red")



#####################################################################
# PART 1. SUMMARY
#####################################################################

# The dplyr package is THE R PACKAGE
# So let's load it
library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)


# Let's load the datasets

accidents #UO: Accident
contractes_menors #UO: contracte
festivals #UO: festival
municipi #UO: municipi
lloguer_any #UO: ??
rendacs #UO: ??
cens_gc #UO: individu


#### ExErSiSe!!!!


#####################################################################
# PART 1. GGPLOT
#####################################################################


## 1.1. 


## 1.2. 

facet
coord_flip
theme
labs


#####################################################################
# PART 2. TYPES OF GEOMETRIES
#####################################################################

## 2.1. ONE NUMERIC VARIABLE

### 2.1.1. HISTOGRAM




### 2.1.2. DENSITY DIAGRAM








## 2.2. ONE CATEGORIC VARIABLE


### 2.2.1. COLUMN DIAGRAM

#Geom_bar
municipi |> 
  ggplot(aes(x = comarca)) +
  geom_bar()

accidents |> 
  ggplot(aes(x = nom_districte)) +
  geom_bar()




## 2.3. NUMERICAL AND CATEGORICAL

### 2.3.1. GEOM_COL

municipi |> 
  ggplot(aes(x = comarca, y = poblacio)) +
  geom_col()

contractes_menors |> 
  ggplot(aes(x = tipus_ens, y = import_adjudicat)) +
  geom_col()

## 2.3.2. GEOM_BOXPLOT
elecc19 |>
  filter(nombre_de_comunidad == "Andalucía") |> 
  mutate(cs_per = cs / poblacion * 100) |> 
  ggplot(aes(x = nombre_de_provincia, y = cs_per)) +
  geom_boxplot()

## 2.3.3. GEOM VIOLIN (EXTRA - no exam)
elecc19 |>
  filter(nombre_de_comunidad == "Andalucía") |> 
  mutate(cs_per = cs / poblacion * 100) |> 
  ggplot(aes(x = nombre_de_provincia, y = cs_per)) +
  geom_violin()

## 2.3.4. GEOM_POINT (EXTRA - no exam)
lloguer_any |> 
  group_by(nom_districte) |>
  summarize(preu_m2 = round(mean(preu_m2, na.rm = T), 1)) |> 
  ggplot(aes(x = fct_reorder(nom_districte, preu_m2), y = preu_m2)) +
  geom_point(size = 10, col = "darkblue") +
  geom_text(aes(label = preu_m2), col = "white") +
  coord_flip() +
  theme_minimal() +
  labs(x = NULL, y = NULL)


## 2.4. NUMERICAL ACROSS TIME

### 2.4.1. GEOM LINE
lloguer_any |> 
  group_by(any) |> 
  summarize(preu = mean(preu, na.rm = T)) |> 
  ggplot(aes(x = any, y = preu)) +
  geom_line() +
  theme_minimal()

lloguer_any |> 
  group_by(any, nom_districte) |> 
  summarize(preu = mean(preu, na.rm = T)) |> 
  ggplot(aes(x = any, y = preu, col = nom_districte)) +
  geom_line() +
  theme_minimal()










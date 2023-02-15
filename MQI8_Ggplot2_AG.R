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
library(ggplot2)
library(stringr)
library(forcats)
theme_set(theme_minimal())


# Let's load the datasets

accidents #UO: Accident
contractes_menors #UO: contracte
festivals #UO: festival
municipi #UO: municipi
lloguer_any #UO: ??
rendacs #UO: ??
cens_gc #UO: individu
elecc19 # <--------- NEW!!!!!!!!!!!!!!!



#####################################################################
# PART 1. AESTHETICS
#####################################################################

# Volem veure la relació entre renda i gini, segons districte

rendacs$import_euros
rendacs$index_gini
rendacs$nom_districte




#####################################################################
# PART 2. GEOMETRIES
#####################################################################

## 2.1. ONE CATEGORIC VARIABLE

### 2.1.1. BAR PLOT (I: bar)

accidents |> 
  ggplot(aes(x = XXXXXXXXXX)) +
  geom_XXXXXXXX()

# Extra: coord_flip()






## 2.2. ONE NUMERIC VARIABLE

### 2.2.1. HISTOGRAM

accidents

#number of bins
#Extra: fill


### 2.2.2. DENSITY PLOT
  
# geom_density

accidents

#Extra: alpha, fill, col
  

### 2.2.3. DOT PLOT

# geom_dotplot
  
accidents |> 
  filter(nom_barri == "el Camp de l'Arpa del Clot")

#Extra: fill









## 2.3. TWO CATEGORIC VARIABLES

### 2.3.1. BAR PLOT (II: fill)

unique(accidents$nom_districte)
unique(accidents$descripcio_torn)

# geom_bar
accidents




## 2.4. ONE CATEGORIC, ONE NUMERIC

### 2.4.1. BAR PLOT (III: col)

# geom_col
municipi 


### 2.4.2. BOXPLOT


# geom_boxplot
accidents |> 
  ggplot(aes(y = edat))


#Extra: x, fill



### 2.4.3. VIOLIN PLOT

# geom_violin
rendacs |> 
  ggplot(aes(x = fct_reorder(nom_districte, import_euros), y = import_euros)) +
  geom_violin() +
  coord_flip()


# A Andalusia, percentatge de vot a Ciutadans als municipis
elecc19


### 2.4.4. POINT + TEXT
lloguer_any |> 
  group_by(nom_districte) |>
  summarize(preu_m2 = round(mean(preu_m2, na.rm = T), 1)) |> 
  ggplot(aes(x = fct_reorder(nom_districte, preu_m2), y = preu_m2)) +
  geom_point(size = 10, col = "darkblue") +
  geom_text(aes(label = preu_m2), col = "white") +
  coord_flip() +
  theme_minimal() +
  labs(x = NULL, y = NULLLLLL)






## 2.5. NUMERICS IN TIME

### 2.5.1. LINE PLOT

# geom_line
lloguer_any |> 
  group_by(any) |> 
  summarize(preu = mean(preu, na.rm = T))

# more lines
lloguer_any |> 
  group_by(any, nom_districte) |> 
  summarize(preu = mean(preu, na.rm = T))




### 2.5.2. PATH

#geom_path
lloguer_any |> 
  group_by(any, nom_districte) |> 
  summarize(preu = mean(preu, na.rm = T),
            preu_m2 = mean(preu_m2, na.rm = T)) |> 
  ggplot(aes(x = preu_m2, y = preu, col = nom_districte,
             alpha = any)) +
  geom_path(size = 0)



# 2.6. TWO NUMERICS

## 2.6.1. POINT
municipi |> 
  ggplot(aes(x = altitud_m, y = superficie_km2)) +
  geom_point()


#Extra: col, size
  
  
  
## 2.6.2. POINTS (II -> COORDINATES)
accidents  
  


## 2.6.3. JITTER

#geom_jitter / geom_point(position = position_jitter())
accidents |>
  ggplot(aes(x = edat, y = hora_dia)) +
  geom_point()


## 2.6.4. TEXT / LABEL

#geom_text
#geom_label
festivals |> 
  group_by(ambit) |> 
  summarize(bcn = mean(assistents_a_barcelona, na.rm = T),
            fora = mean(assistents_fora_de_barcelona, na.rm = T)) |> 
  ggplot(aes(x = bcn, y = fora)) +
  geom_point()


# 2.7. THREE VARIABLES

## 2.7.1. TILE

#geom_tile
accidents |> 
  count(hora_dia, descripcio_dia_setmana)






# 2.8. DESCRIPTIVE STATISTICS

## Mean and median

#geom_vline() or geom_hline()
municipi |> 
  ggplot(aes(x = superficie_km2)) +
  geom_density()


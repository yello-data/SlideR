#####################################################################
############################## DPLYR ################################
#####################################################################



#####################################################################
# PART 0. LEARNING CURVE
#####################################################################

# R learning curve
library(dplyr)
rlc <- tibble(class = 1:8,
             knowledge = (1:8)^3)
class <- 5
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


# Let's load the datasets

accidents #UO: Accident
contractes_menors #UO: contracte
festivals #UO: festival
municipi #UO: municipi
lloguer_any #UO: ??
rendacs #UO: ??
cens_gc #UO: individu


#### ExErSiSe!!!!

#Q: Quina és la durada dels festivals de titularitat privada?
# Filtra per festivals de titularitat privada...
#...crea la variable durada amb la durada del festival...
#...elimina la columna any (no és una variable), data_inici, data_fi...
#... ordena les dades per durada, ordre descendent
festivals


#####################################################################
# PART 2. SUMMARIZE CATEGORICAL VARIABLES 
#####################################################################

#2.1. Frequency table

##2.1.1. Count (dplyr)
count(festivals, ambit) #sense ordenar
count(festivals, ambit, sort = T) #ordenat

##2.1.2. Table (base)

table(festivals$ambit) # Nombres absoluts
prop.table(table(festivals$ambit)) * 100 # En percentatges

barplot(table(festivals$ambit)) # plot nombres absoluts
barplot(prop.table(table(festivals$ambit))) #plot percentatges

#ExErCiSe!!!!
cens_gc


#####################################################################
# PART 3. SUMMARIZE NUMERICAL VARIABLES 
#####################################################################



### 3.1. SUMMARIZE / SUMMARISE
# Summarizes data

contractes_menors |>
  summarize(import_total = sum(import_adjudicat)) #SEMPRE na.rm = T

accidents |> #es poden fer varis sumaris, separats per comes
  summarize(edat_mitjana = mean(edat),
            n = n())

cens_gc |> #molt útil amb variables binàries (1-0) per treure mitjanes
  summarize(es_afusellat = mean(es_afusellat)) #percentatges, en variables 1-0

cens_gc |> #es pot binaritzar, si a dins de mean/sum posem una operació lògica
  summarize(tarragona = mean(provincia_desaparicio == "Tarragona", na.rm = T)) #percentatges, vectors lògics (TRUE-FALSE)



### 3.4.3. GROUP_BY
# Groups the data by some category

#Mitjana assistència per àmbit
festivals |> 
  group_by(ambit) |> 
  summarize(assistencia = mean(assistents_a_barcelona, na.rm = T))

#nombre d'accidents per barri
accidents |>
  group_by(nom_districte, descripcio_torn) |> 
  summarize(m_edat = mean(edat))

#Preu lloguer i preu lloguer m2 per any i districte
lloguer_any |> 
  group_by(any, nom_districte) |> 
  summarize(preu = mean(preu, na.rm = T),
            preu_m2 = mean(preu_m2, na.rm = T),
            n = n())


#### ExErSiSe: Minority report.

# Quin proveidor s'ha emportat més pasta 'menor'? (group_by -> summarize -> sum)
contractes_menors 

# Quim proveidor s'ha emportat més pasta 'menor' als contractes de l'Ajuntament de Barcelona?
contractes_menors

# Per comarca, calcula la mediana i la desviació típica de població i l'altitud màxima ...
#... ordena els resultats per desviació típica, en ordre descendent.
municipi


### Exemple de group_by amb filter (NO EXAMEN!)
#Q: Quin és el preu mínim i el preu màxim de cada districte?
lloguer_any |> 
  group_by(nom_districte) |> 
  filter(preu_m2 == max(preu_m2) | preu_m2 == min(preu_m2)) |> 
  arrange(nom_districte)

### Exemple de group_by amb mutate (NO EXAMEN!)
#Q: Quins són els barris amb major preu de lloguer en relació a la mitjana del districte?
lloguer_any |> 
  group_by(trimestre, nom_districte, any) |> 
  mutate(m_distr_peu_m2 = mean(preu_m2, na.rm = T)) |> 
  ungroup() |> 
  mutate(rel_distr = preu_m2 / m_distr_peu_m2) |> 
  arrange(desc(rel_distr))

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


## 1.1. LAYERS


accidents |> 
  ggplot(aes(x = nom_mes, y = edat)) +
  geom_col()


## 1.2. 

#facet
#coord_flip
#theme
#labs


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













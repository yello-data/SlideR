library(readxl)
library(tidyverse)
#Download file from https://www.systemicpeace.org/
pol <- read_xls("Datasets/p5v2018.xls")

pol %>%
  select(ccode, scode, country, year, polity2, xrreg:polcomp) %>%
  filter(country %in% c("Afghanistan", "United States", "United Kingdom",
                        "Spain", "Sweden", "South Africa", "Russia", "Belgium",
                        "China", "Japan", "Mexico")) %>%
  write_csv("polity.csv")

unique(pol$country)

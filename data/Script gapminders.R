library(pacman)
p_load(tidyverse, gapminder, readxl, haven, foreign)

write_csv(gapminder, "gapminder.csv")
write_csv2(gapminder, "gapminder2.csv")
write_tsv(gapminder, "gapminder3.tsv")
write_delim(gapminder, "gapminder4.txt", delim = "/")
write.dta(gapminder, "gapminder5.dta")
save(gapminder, file = "gapminder6.Rdata")
write_sav(gapminder, path = "gapminder7.sav")


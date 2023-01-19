## Vector

"TRUE" == "FALSE"

"TRUE" == "true"

class(c("FALSE", "FALSE", "TRUE"))

c(1) == 1

e <- 2
c(6, 4, 2) / c(e, e, e)

20:25

c(10, 100, 1000) / 10

c(3, 5, 8:10) - 1:5

c(TRUE, FALSE, TRUE)[2]

as.Date(c("2022/02/02")) - as.Date(c("2022/02/01"))


c(4, 6, 7, 8

c(TRUE, FALSE TRUE)

c("France", "Spain", "UK)
  "

as.factor("France", "Spain", "UK")
    
    


## Dataframe
library(dplyr)
holi <- tibble(n = 1:5, v = 6 - 1:5)
holi[1,2]

holi <- tibble(n = 1:5, v = 6 - 1:5)
holi[2, "n"]

holi <- tibble(n = 1:5, v = 6 - 1:5)
holi[3, "v"]

holi <- tibble(n = 1:5, v = 6 - 1:5)
holi$v[5]

holi <- tibble(n = 1:5, v = 6 - 1:5)
holi$n == 1

holi <- tibble(n = 1:5, v = 5 - 1:5)
holi$n == 1

holi <- tibble(n = 1:5, v = 5 - 1:5)
dim(holi)


## FUNCTIONS

aa <- function(hello = 4) {hello + 1}
aa()

bb <- function(hello, bye = 0) {hello + bye}
bb(hello = 3)

cc <- function(hello, bye = 0) {hello + bye}
cc(3, 3)

dd <- function(x, y) {x * y}
dd(1, 3)

ee <- function(x, y = 1) {x * y}
ee(1)




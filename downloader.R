library(tidyverse)
library(purrr)
library(glue)
library(haven)
library(eph)




bases_hogar <- get_microdata(year = 1996:2003, wave = c(1,2),type = 'hogar')



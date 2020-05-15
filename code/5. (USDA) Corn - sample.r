#Library--------------------
library(tidyverse)
library(readr)
library(xlsx)
library(readxl)

#Importing excel---------
yield0 <- read_excel("corn3_adjusted.xlsx")

yield_asni <- unique(yield0$Ansi)

ansi.table <- as.data.frame(yield_asni) %>% 
  mutate(test = 1)

write.xlsx(ansi.table, file="country_ansi.xlsx")





#Library
library(tidyverse)
library(data.table)
library(stringr)
library(readxl)

#Importing coordinate excel
coord <- read_excel("ghcnd-inventory.xlsx")

#Filtering 
coord_1 <- coord %>% 
  filter(Column4=="TAVG") %>% 
  select(1,2,3)
coord_2 <- coord_1 %>% 
  filter(str_detect(Column1, "^US"))

names(coord_2)[1] <- "Station"
names(coord_2)[2] <- "Lat"
names(coord_2)[3] <- "Long"

remove(coord, coord_1)

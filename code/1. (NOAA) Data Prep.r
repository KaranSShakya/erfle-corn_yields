#Library
library(tidyverse)
library(data.table)
library(stringr)
library(xlsx)

#Running Coordinate.r
source("2. Coordinate.r")

#NOAA - 2012 (TMAX) -------------------------------------------------
#Importing 2012 gz file (really really large!)
noaa_2012 <- fread("2012.csv.gz")

#Filtering TMAX
noaa_2012_1 <- noaa_2012 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_2012_2 <- noaa_2012_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_2012_3 <- noaa_2012_2 %>% 
  select(1,2,4)
names(noaa_2012_3)[1] <- "Station"
names(noaa_2012_3)[2] <- "Date"
names(noaa_2012_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_2012_3 <- noaa_2012_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.2012 <- t(sapply(noaa_2012_3$Date, function(x) substring(x,
                                                          first=c(1,5,7), 
                                                          last=c(4, 6, 8))))
noaa_2012_4 <- cbind(noaa_2012_3, split.2012)
names(noaa_2012_4)[5] <- "Year"
names(noaa_2012_4)[6] <- "Month"
names(noaa_2012_4)[7] <- "Day"

#Filtering month (April - November)
noaa_2012_4$Month <- as.numeric(noaa_2012_4$Month)

noaa_2012_5 <- noaa_2012_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_2012_6 <- noaa_2012_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_2012_7 <- noaa_2012_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_2012_8 <- noaa_2012_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_2012.m <- merge(x=noaa_2012_8, y=coord_2, by="Station", all=T)

remove(noaa_2012, noaa_2012_1, noaa_2012_2, noaa_2012_3, noaa_2012_4, noaa_2012_5, noaa_2012_6, noaa_2012_7,
       noaa_2012_8, split.2012)

#NOAA - 2013 (TMAX) -------------------------------------------------
#Importing 2013 gz file (really really large!)
noaa_2013 <- fread("2013.csv.gz")

#Filtering TMAX
noaa_2013_1 <- noaa_2013 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_2013_2 <- noaa_2013_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_2013_3 <- noaa_2013_2 %>% 
  select(1,2,4)
names(noaa_2013_3)[1] <- "Station"
names(noaa_2013_3)[2] <- "Date"
names(noaa_2013_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_2013_3 <- noaa_2013_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.2013 <- t(sapply(noaa_2013_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_2013_4 <- cbind(noaa_2013_3, split.2013)
names(noaa_2013_4)[5] <- "Year"
names(noaa_2013_4)[6] <- "Month"
names(noaa_2013_4)[7] <- "Day"

#Filtering month (April - November)
noaa_2013_4$Month <- as.numeric(noaa_2013_4$Month)

noaa_2013_5 <- noaa_2013_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_2013_6 <- noaa_2013_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_2013_7 <- noaa_2013_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_2013_8 <- noaa_2013_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_2013.m <- merge(x=noaa_2013_8, y=coord_2, by="Station", all=T)

remove(noaa_2013, noaa_2013_1, noaa_2013_2, noaa_2013_3, noaa_2013_4, noaa_2013_5, noaa_2013_6, noaa_2013_7,
       noaa_2013_8, split.2013)


#NOAA - 2014 (TMAX) -------------------------------------------------
#Importing 2014 gz file (really really large!)
noaa_2014 <- fread("2014.csv.gz")

#Filtering TMAX
noaa_2014_1 <- noaa_2014 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_2014_2 <- noaa_2014_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_2014_3 <- noaa_2014_2 %>% 
  select(1,2,4)
names(noaa_2014_3)[1] <- "Station"
names(noaa_2014_3)[2] <- "Date"
names(noaa_2014_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_2014_3 <- noaa_2014_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.2014 <- t(sapply(noaa_2014_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_2014_4 <- cbind(noaa_2014_3, split.2014)
names(noaa_2014_4)[5] <- "Year"
names(noaa_2014_4)[6] <- "Month"
names(noaa_2014_4)[7] <- "Day"

#Filtering month (April - November)
noaa_2014_4$Month <- as.numeric(noaa_2014_4$Month)

noaa_2014_5 <- noaa_2014_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_2014_6 <- noaa_2014_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_2014_7 <- noaa_2014_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_2014_8 <- noaa_2014_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_2014.m <- merge(x=noaa_2014_8, y=coord_2, by="Station", all=T)

remove(noaa_2014, noaa_2014_1, noaa_2014_2, noaa_2014_3, noaa_2014_4, noaa_2014_5, noaa_2014_6, noaa_2014_7,
       noaa_2014_8, split.2014)

#NOAA - 2015 (TMAX) -------------------------------------------------
#Importing 2015 gz file (really really large!)
noaa_2015 <- fread("2015.csv.gz")

#Filtering TMAX
noaa_2015_1 <- noaa_2015 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_2015_2 <- noaa_2015_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_2015_3 <- noaa_2015_2 %>% 
  select(1,2,4)
names(noaa_2015_3)[1] <- "Station"
names(noaa_2015_3)[2] <- "Date"
names(noaa_2015_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_2015_3 <- noaa_2015_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.2015 <- t(sapply(noaa_2015_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_2015_4 <- cbind(noaa_2015_3, split.2015)
names(noaa_2015_4)[5] <- "Year"
names(noaa_2015_4)[6] <- "Month"
names(noaa_2015_4)[7] <- "Day"

#Filtering month (April - November)
noaa_2015_4$Month <- as.numeric(noaa_2015_4$Month)

noaa_2015_5 <- noaa_2015_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_2015_6 <- noaa_2015_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_2015_7 <- noaa_2015_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_2015_8 <- noaa_2015_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_2015.m <- merge(x=noaa_2015_8, y=coord_2, by="Station", all=T)

remove(noaa_2015, noaa_2015_1, noaa_2015_2, noaa_2015_3, noaa_2015_4, noaa_2015_5, noaa_2015_6, noaa_2015_7,
       noaa_2015_8, split.2015)

#NOAA - 2016 (TMAX) -------------------------------------------------
#Importing 2016 gz file (really really large!)
noaa_2016 <- fread("2016.csv.gz")

#Filtering TMAX
noaa_2016_1 <- noaa_2016 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_2016_2 <- noaa_2016_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_2016_3 <- noaa_2016_2 %>% 
  select(1,2,4)
names(noaa_2016_3)[1] <- "Station"
names(noaa_2016_3)[2] <- "Date"
names(noaa_2016_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_2016_3 <- noaa_2016_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.2016 <- t(sapply(noaa_2016_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_2016_4 <- cbind(noaa_2016_3, split.2016)
names(noaa_2016_4)[5] <- "Year"
names(noaa_2016_4)[6] <- "Month"
names(noaa_2016_4)[7] <- "Day"

#Filtering month (April - November)
noaa_2016_4$Month <- as.numeric(noaa_2016_4$Month)

noaa_2016_5 <- noaa_2016_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_2016_6 <- noaa_2016_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_2016_7 <- noaa_2016_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_2016_8 <- noaa_2016_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_2016.m <- merge(x=noaa_2016_8, y=coord_2, by="Station", all=T)

remove(noaa_2016, noaa_2016_1, noaa_2016_2, noaa_2016_3, noaa_2016_4, noaa_2016_5, noaa_2016_6, noaa_2016_7,
       noaa_2016_8, split.2016)

#NOAA - 2017 (TMAX) -------------------------------------------------
#Importing 2017 gz file (really really large!)
noaa_2017 <- fread("2017.csv.gz")

#Filtering TMAX
noaa_2017_1 <- noaa_2017 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_2017_2 <- noaa_2017_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_2017_3 <- noaa_2017_2 %>% 
  select(1,2,4)
names(noaa_2017_3)[1] <- "Station"
names(noaa_2017_3)[2] <- "Date"
names(noaa_2017_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_2017_3 <- noaa_2017_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.2017 <- t(sapply(noaa_2017_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_2017_4 <- cbind(noaa_2017_3, split.2017)
names(noaa_2017_4)[5] <- "Year"
names(noaa_2017_4)[6] <- "Month"
names(noaa_2017_4)[7] <- "Day"

#Filtering month (April - November)
noaa_2017_4$Month <- as.numeric(noaa_2017_4$Month)

noaa_2017_5 <- noaa_2017_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_2017_6 <- noaa_2017_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_2017_7 <- noaa_2017_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_2017_8 <- noaa_2017_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_2017.m <- merge(x=noaa_2017_8, y=coord_2, by="Station", all=T)

remove(noaa_2017, noaa_2017_1, noaa_2017_2, noaa_2017_3, noaa_2017_4, noaa_2017_5, noaa_2017_6, noaa_2017_7,
       noaa_2017_8, split.2017)



#NOAA - 2011 (TMAX) -------------------------------------------------
#Importing 2011 gz file (really really large!)
noaa_2011 <- fread("2011.csv.gz")

#Filtering TMAX
noaa_2011_1 <- noaa_2011 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_2011_2 <- noaa_2011_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_2011_3 <- noaa_2011_2 %>% 
  select(1,2,4)
names(noaa_2011_3)[1] <- "Station"
names(noaa_2011_3)[2] <- "Date"
names(noaa_2011_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_2011_3 <- noaa_2011_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.2011 <- t(sapply(noaa_2011_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_2011_4 <- cbind(noaa_2011_3, split.2011)
names(noaa_2011_4)[5] <- "Year"
names(noaa_2011_4)[6] <- "Month"
names(noaa_2011_4)[7] <- "Day"

#Filtering month (April - November)
noaa_2011_4$Month <- as.numeric(noaa_2011_4$Month)

noaa_2011_5 <- noaa_2011_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_2011_6 <- noaa_2011_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_2011_7 <- noaa_2011_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_2011_8 <- noaa_2011_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_2011.m <- merge(x=noaa_2011_8, y=coord_2, by="Station", all=T)

remove(noaa_2011, noaa_2011_1, noaa_2011_2, noaa_2011_3, noaa_2011_4, noaa_2011_5, noaa_2011_6, noaa_2011_7,
       noaa_2011_8, split.2011)

#NOAA - 2010 (TMAX) -------------------------------------------------
#Importing 2010 gz file (really really large!)
noaa_2010 <- fread("2010.csv.gz")

#Filtering TMAX
noaa_2010_1 <- noaa_2010 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_2010_2 <- noaa_2010_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_2010_3 <- noaa_2010_2 %>% 
  select(1,2,4)
names(noaa_2010_3)[1] <- "Station"
names(noaa_2010_3)[2] <- "Date"
names(noaa_2010_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_2010_3 <- noaa_2010_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.2010 <- t(sapply(noaa_2010_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_2010_4 <- cbind(noaa_2010_3, split.2010)
names(noaa_2010_4)[5] <- "Year"
names(noaa_2010_4)[6] <- "Month"
names(noaa_2010_4)[7] <- "Day"

#Filtering month (April - November)
noaa_2010_4$Month <- as.numeric(noaa_2010_4$Month)

noaa_2010_5 <- noaa_2010_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_2010_6 <- noaa_2010_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_2010_7 <- noaa_2010_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_2010_8 <- noaa_2010_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_2010.m <- merge(x=noaa_2010_8, y=coord_2, by="Station", all=T)

remove(noaa_2010, noaa_2010_1, noaa_2010_2, noaa_2010_3, noaa_2010_4, noaa_2010_5, noaa_2010_6, noaa_2010_7,
       noaa_2010_8, split.2010)

#NOAA - 2009 (TMAX) -------------------------------------------------
#Importing 2009 gz file (really really large!)
noaa_2009 <- fread("2009.csv.gz")

#Filtering TMAX
noaa_2009_1 <- noaa_2009 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_2009_2 <- noaa_2009_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_2009_3 <- noaa_2009_2 %>% 
  select(1,2,4)
names(noaa_2009_3)[1] <- "Station"
names(noaa_2009_3)[2] <- "Date"
names(noaa_2009_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_2009_3 <- noaa_2009_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.2009 <- t(sapply(noaa_2009_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_2009_4 <- cbind(noaa_2009_3, split.2009)
names(noaa_2009_4)[5] <- "Year"
names(noaa_2009_4)[6] <- "Month"
names(noaa_2009_4)[7] <- "Day"

#Filtering month (April - November)
noaa_2009_4$Month <- as.numeric(noaa_2009_4$Month)

noaa_2009_5 <- noaa_2009_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_2009_6 <- noaa_2009_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_2009_7 <- noaa_2009_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_2009_8 <- noaa_2009_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_2009.m <- merge(x=noaa_2009_8, y=coord_2, by="Station", all=T)

remove(noaa_2009, noaa_2009_1, noaa_2009_2, noaa_2009_3, noaa_2009_4, noaa_2009_5, noaa_2009_6, noaa_2009_7,
       noaa_2009_8, split.2009)



#NOAA - 2008 (TMAX) -------------------------------------------------
#Importing 2008 gz file (really really large!)
noaa_2008 <- fread("2008.csv.gz")

#Filtering TMAX
noaa_2008_1 <- noaa_2008 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_2008_2 <- noaa_2008_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_2008_3 <- noaa_2008_2 %>% 
  select(1,2,4)
names(noaa_2008_3)[1] <- "Station"
names(noaa_2008_3)[2] <- "Date"
names(noaa_2008_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_2008_3 <- noaa_2008_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.2008 <- t(sapply(noaa_2008_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_2008_4 <- cbind(noaa_2008_3, split.2008)
names(noaa_2008_4)[5] <- "Year"
names(noaa_2008_4)[6] <- "Month"
names(noaa_2008_4)[7] <- "Day"

#Filtering month (April - November)
noaa_2008_4$Month <- as.numeric(noaa_2008_4$Month)

noaa_2008_5 <- noaa_2008_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_2008_6 <- noaa_2008_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_2008_7 <- noaa_2008_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_2008_8 <- noaa_2008_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_2008.m <- merge(x=noaa_2008_8, y=coord_2, by="Station", all=T)

remove(noaa_2008, noaa_2008_1, noaa_2008_2, noaa_2008_3, noaa_2008_4, noaa_2008_5, noaa_2008_6, noaa_2008_7,
       noaa_2008_8, split.2008)

#NOAA - 2007 (TMAX) -------------------------------------------------
#Importing 2007 gz file (really really large!)
noaa_2007 <- fread("2007.csv.gz")

#Filtering TMAX
noaa_2007_1 <- noaa_2007 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_2007_2 <- noaa_2007_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_2007_3 <- noaa_2007_2 %>% 
  select(1,2,4)
names(noaa_2007_3)[1] <- "Station"
names(noaa_2007_3)[2] <- "Date"
names(noaa_2007_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_2007_3 <- noaa_2007_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.2007 <- t(sapply(noaa_2007_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_2007_4 <- cbind(noaa_2007_3, split.2007)
names(noaa_2007_4)[5] <- "Year"
names(noaa_2007_4)[6] <- "Month"
names(noaa_2007_4)[7] <- "Day"

#Filtering month (April - November)
noaa_2007_4$Month <- as.numeric(noaa_2007_4$Month)

noaa_2007_5 <- noaa_2007_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_2007_6 <- noaa_2007_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_2007_7 <- noaa_2007_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_2007_8 <- noaa_2007_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_2007.m <- merge(x=noaa_2007_8, y=coord_2, by="Station", all=T)

remove(noaa_2007, noaa_2007_1, noaa_2007_2, noaa_2007_3, noaa_2007_4, noaa_2007_5, noaa_2007_6, noaa_2007_7,
       noaa_2007_8, split.2007)

#NOAA - 2006 (TMAX) -------------------------------------------------
#Importing 2006 gz file (really really large!)
noaa_2006 <- fread("2006.csv.gz")

#Filtering TMAX
noaa_2006_1 <- noaa_2006 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_2006_2 <- noaa_2006_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_2006_3 <- noaa_2006_2 %>% 
  select(1,2,4)
names(noaa_2006_3)[1] <- "Station"
names(noaa_2006_3)[2] <- "Date"
names(noaa_2006_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_2006_3 <- noaa_2006_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.2006 <- t(sapply(noaa_2006_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_2006_4 <- cbind(noaa_2006_3, split.2006)
names(noaa_2006_4)[5] <- "Year"
names(noaa_2006_4)[6] <- "Month"
names(noaa_2006_4)[7] <- "Day"

#Filtering month (April - November)
noaa_2006_4$Month <- as.numeric(noaa_2006_4$Month)

noaa_2006_5 <- noaa_2006_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_2006_6 <- noaa_2006_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_2006_7 <- noaa_2006_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_2006_8 <- noaa_2006_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_2006.m <- merge(x=noaa_2006_8, y=coord_2, by="Station", all=T)

remove(noaa_2006, noaa_2006_1, noaa_2006_2, noaa_2006_3, noaa_2006_4, noaa_2006_5, noaa_2006_6, noaa_2006_7,
       noaa_2006_8, split.2006)



#NOAA - 2005 (TMAX) -------------------------------------------------
#Importing 2005 gz file (really really large!)
noaa_2005 <- fread("2005.csv.gz")

#Filtering TMAX
noaa_2005_1 <- noaa_2005 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_2005_2 <- noaa_2005_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_2005_3 <- noaa_2005_2 %>% 
  select(1,2,4)
names(noaa_2005_3)[1] <- "Station"
names(noaa_2005_3)[2] <- "Date"
names(noaa_2005_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_2005_3 <- noaa_2005_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.2005 <- t(sapply(noaa_2005_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_2005_4 <- cbind(noaa_2005_3, split.2005)
names(noaa_2005_4)[5] <- "Year"
names(noaa_2005_4)[6] <- "Month"
names(noaa_2005_4)[7] <- "Day"

#Filtering month (April - November)
noaa_2005_4$Month <- as.numeric(noaa_2005_4$Month)

noaa_2005_5 <- noaa_2005_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_2005_6 <- noaa_2005_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_2005_7 <- noaa_2005_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_2005_8 <- noaa_2005_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_2005.m <- merge(x=noaa_2005_8, y=coord_2, by="Station", all=T)

remove(noaa_2005, noaa_2005_1, noaa_2005_2, noaa_2005_3, noaa_2005_4, noaa_2005_5, noaa_2005_6, noaa_2005_7,
       noaa_2005_8, split.2005)

#NOAA - 2004 (TMAX) -------------------------------------------------
#Importing 2004 gz file (really really large!)
noaa_2004 <- fread("2004.csv.gz")

#Filtering TMAX
noaa_2004_1 <- noaa_2004 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_2004_2 <- noaa_2004_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_2004_3 <- noaa_2004_2 %>% 
  select(1,2,4)
names(noaa_2004_3)[1] <- "Station"
names(noaa_2004_3)[2] <- "Date"
names(noaa_2004_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_2004_3 <- noaa_2004_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.2004 <- t(sapply(noaa_2004_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_2004_4 <- cbind(noaa_2004_3, split.2004)
names(noaa_2004_4)[5] <- "Year"
names(noaa_2004_4)[6] <- "Month"
names(noaa_2004_4)[7] <- "Day"

#Filtering month (April - November)
noaa_2004_4$Month <- as.numeric(noaa_2004_4$Month)

noaa_2004_5 <- noaa_2004_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_2004_6 <- noaa_2004_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_2004_7 <- noaa_2004_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_2004_8 <- noaa_2004_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_2004.m <- merge(x=noaa_2004_8, y=coord_2, by="Station", all=T)

remove(noaa_2004, noaa_2004_1, noaa_2004_2, noaa_2004_3, noaa_2004_4, noaa_2004_5, noaa_2004_6, noaa_2004_7,
       noaa_2004_8, split.2004)

#NOAA - 2003 (TMAX) -------------------------------------------------
#Importing 2003 gz file (really really large!)
noaa_2003 <- fread("2003.csv.gz")

#Filtering TMAX
noaa_2003_1 <- noaa_2003 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_2003_2 <- noaa_2003_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_2003_3 <- noaa_2003_2 %>% 
  select(1,2,4)
names(noaa_2003_3)[1] <- "Station"
names(noaa_2003_3)[2] <- "Date"
names(noaa_2003_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_2003_3 <- noaa_2003_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.2003 <- t(sapply(noaa_2003_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_2003_4 <- cbind(noaa_2003_3, split.2003)
names(noaa_2003_4)[5] <- "Year"
names(noaa_2003_4)[6] <- "Month"
names(noaa_2003_4)[7] <- "Day"

#Filtering month (April - November)
noaa_2003_4$Month <- as.numeric(noaa_2003_4$Month)

noaa_2003_5 <- noaa_2003_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_2003_6 <- noaa_2003_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_2003_7 <- noaa_2003_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_2003_8 <- noaa_2003_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_2003.m <- merge(x=noaa_2003_8, y=coord_2, by="Station", all=T)

remove(noaa_2003, noaa_2003_1, noaa_2003_2, noaa_2003_3, noaa_2003_4, noaa_2003_5, noaa_2003_6, noaa_2003_7,
       noaa_2003_8, split.2003)



#NOAA - 2002 (TMAX) -------------------------------------------------
#Importing 2002 gz file (really really large!)
noaa_2002 <- fread("2002.csv.gz")

#Filtering TMAX
noaa_2002_1 <- noaa_2002 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_2002_2 <- noaa_2002_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_2002_3 <- noaa_2002_2 %>% 
  select(1,2,4)
names(noaa_2002_3)[1] <- "Station"
names(noaa_2002_3)[2] <- "Date"
names(noaa_2002_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_2002_3 <- noaa_2002_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.2002 <- t(sapply(noaa_2002_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_2002_4 <- cbind(noaa_2002_3, split.2002)
names(noaa_2002_4)[5] <- "Year"
names(noaa_2002_4)[6] <- "Month"
names(noaa_2002_4)[7] <- "Day"

#Filtering month (April - November)
noaa_2002_4$Month <- as.numeric(noaa_2002_4$Month)

noaa_2002_5 <- noaa_2002_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_2002_6 <- noaa_2002_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_2002_7 <- noaa_2002_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_2002_8 <- noaa_2002_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_2002.m <- merge(x=noaa_2002_8, y=coord_2, by="Station", all=T)

remove(noaa_2002, noaa_2002_1, noaa_2002_2, noaa_2002_3, noaa_2002_4, noaa_2002_5, noaa_2002_6, noaa_2002_7,
       noaa_2002_8, split.2002)

#NOAA - 2001 (TMAX) -------------------------------------------------
#Importing 2001 gz file (really really large!)
noaa_2001 <- fread("2001.csv.gz")

#Filtering TMAX
noaa_2001_1 <- noaa_2001 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_2001_2 <- noaa_2001_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_2001_3 <- noaa_2001_2 %>% 
  select(1,2,4)
names(noaa_2001_3)[1] <- "Station"
names(noaa_2001_3)[2] <- "Date"
names(noaa_2001_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_2001_3 <- noaa_2001_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.2001 <- t(sapply(noaa_2001_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_2001_4 <- cbind(noaa_2001_3, split.2001)
names(noaa_2001_4)[5] <- "Year"
names(noaa_2001_4)[6] <- "Month"
names(noaa_2001_4)[7] <- "Day"

#Filtering month (April - November)
noaa_2001_4$Month <- as.numeric(noaa_2001_4$Month)

noaa_2001_5 <- noaa_2001_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_2001_6 <- noaa_2001_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_2001_7 <- noaa_2001_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_2001_8 <- noaa_2001_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_2001.m <- merge(x=noaa_2001_8, y=coord_2, by="Station", all=T)

remove(noaa_2001, noaa_2001_1, noaa_2001_2, noaa_2001_3, noaa_2001_4, noaa_2001_5, noaa_2001_6, noaa_2001_7,
       noaa_2001_8, split.2001)

#NOAA - 2000 (TMAX) -------------------------------------------------
#Importing 2000 gz file (really really large!)
noaa_2000 <- fread("2000.csv.gz")

#Filtering TMAX
noaa_2000_1 <- noaa_2000 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_2000_2 <- noaa_2000_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_2000_3 <- noaa_2000_2 %>% 
  select(1,2,4)
names(noaa_2000_3)[1] <- "Station"
names(noaa_2000_3)[2] <- "Date"
names(noaa_2000_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_2000_3 <- noaa_2000_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.2000 <- t(sapply(noaa_2000_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_2000_4 <- cbind(noaa_2000_3, split.2000)
names(noaa_2000_4)[5] <- "Year"
names(noaa_2000_4)[6] <- "Month"
names(noaa_2000_4)[7] <- "Day"

#Filtering month (April - November)
noaa_2000_4$Month <- as.numeric(noaa_2000_4$Month)

noaa_2000_5 <- noaa_2000_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_2000_6 <- noaa_2000_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_2000_7 <- noaa_2000_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_2000_8 <- noaa_2000_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_2000.m <- merge(x=noaa_2000_8, y=coord_2, by="Station", all=T)

remove(noaa_2000, noaa_2000_1, noaa_2000_2, noaa_2000_3, noaa_2000_4, noaa_2000_5, noaa_2000_6, noaa_2000_7,
       noaa_2000_8, split.2000)



#NOAA - 1999 (TMAX) -------------------------------------------------
#Importing 1999 gz file (really really large!)
noaa_1999 <- fread("1999.csv.gz")

#Filtering TMAX
noaa_1999_1 <- noaa_1999 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_1999_2 <- noaa_1999_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_1999_3 <- noaa_1999_2 %>% 
  select(1,2,4)
names(noaa_1999_3)[1] <- "Station"
names(noaa_1999_3)[2] <- "Date"
names(noaa_1999_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_1999_3 <- noaa_1999_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.1999 <- t(sapply(noaa_1999_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_1999_4 <- cbind(noaa_1999_3, split.1999)
names(noaa_1999_4)[5] <- "Year"
names(noaa_1999_4)[6] <- "Month"
names(noaa_1999_4)[7] <- "Day"

#Filtering month (April - November)
noaa_1999_4$Month <- as.numeric(noaa_1999_4$Month)

noaa_1999_5 <- noaa_1999_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_1999_6 <- noaa_1999_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_1999_7 <- noaa_1999_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_1999_8 <- noaa_1999_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_1999.m <- merge(x=noaa_1999_8, y=coord_2, by="Station", all=T)

remove(noaa_1999, noaa_1999_1, noaa_1999_2, noaa_1999_3, noaa_1999_4, noaa_1999_5, noaa_1999_6, noaa_1999_7,
       noaa_1999_8, split.1999)

#NOAA - 1998 (TMAX) -------------------------------------------------
#Importing 1998 gz file (really really large!)
noaa_1998 <- fread("1998.csv.gz")

#Filtering TMAX
noaa_1998_1 <- noaa_1998 %>% 
  filter(V3=="TMAX")

#Filtering US stations
noaa_1998_2 <- noaa_1998_1 %>% 
  filter(str_detect(V1, "^US"))

#Organizing table
noaa_1998_3 <- noaa_1998_2 %>% 
  select(1,2,4)
names(noaa_1998_3)[1] <- "Station"
names(noaa_1998_3)[2] <- "Date"
names(noaa_1998_3)[3] <- "Tmax_tenth"

#Proper Tmax
noaa_1998_3 <- noaa_1998_3 %>% 
  mutate(Tmax_12=Tmax_tenth/10)

#Seperating date
split.1998 <- t(sapply(noaa_1998_3$Date, function(x) substring(x,
                                                               first=c(1,5,7), 
                                                               last=c(4, 6, 8))))
noaa_1998_4 <- cbind(noaa_1998_3, split.1998)
names(noaa_1998_4)[5] <- "Year"
names(noaa_1998_4)[6] <- "Month"
names(noaa_1998_4)[7] <- "Day"

#Filtering month (April - November)
noaa_1998_4$Month <- as.numeric(noaa_1998_4$Month)

noaa_1998_5 <- noaa_1998_4 %>% 
  filter(Month >= 4) %>% 
  filter(Month <= 11)

noaa_1998_6 <- noaa_1998_5 %>% 
  select(-2,-3)

#Group by - Tmax >= 30
noaa_1998_7 <- noaa_1998_6 %>% 
  group_by(Station, Month, Year) %>% 
  summarise(Tmax_12_m = sum(Tmax_12 >= 30)) %>% 
  ungroup()

noaa_1998_8 <- noaa_1998_7 %>% 
  group_by(Station, Year) %>% 
  summarise(Tavg_12_mm = sum(Tmax_12_m)) %>% 
  ungroup()

#Join Coordiantes
noaa_1998.m <- merge(x=noaa_1998_8, y=coord_2, by="Station", all=T)

remove(noaa_1998, noaa_1998_1, noaa_1998_2, noaa_1998_3, noaa_1998_4, noaa_1998_5, noaa_1998_6, noaa_1998_7,
       noaa_1998_8, split.1998)



#Library--------------------
library(tidyverse)
library(readr)
library(xlsx)
library(readxl)

#Temperature - Importing ----------------------
t.1998 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp1998.xlsx") %>% 
  select(2,6)
t.1999 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp1999.xlsx") %>% 
  select(2,6)
t.2000 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp2000.xlsx") %>% 
  select(2,6)
t.2001 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp2001.xlsx") %>% 
  select(2,6)
t.2002 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp2002.xlsx") %>% 
  select(2,6)
t.2003 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp2003.xlsx") %>% 
  select(2,6)
t.2004 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp2004.xlsx") %>% 
  select(2,6)
t.2005 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp2005.xlsx") %>% 
  select(2,6)
t.2006 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp2006.xlsx") %>% 
  select(2,6)
t.2007 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp2007.xlsx") %>% 
  select(2,6)
t.2008 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp2008.xlsx") %>% 
  select(2,6)
t.2009 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp2009.xlsx") %>% 
  select(2,6)
t.2010 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp2010.xlsx") %>% 
  select(2,6)
t.2011 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp2011.xlsx") %>% 
  select(2,6)
t.2012 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp2012.xlsx") %>% 
  select(2,6)
t.2013 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp2013.xlsx") %>% 
  select(2,6)
t.2014 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp2014.xlsx") %>% 
  select(2,6)
t.2015 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp2015.xlsx") %>% 
  select(2,6)
t.2016 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp2016.xlsx") %>% 
  select(2,6)
t.2017 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/temp2017.xlsx") %>% 
  select(2,6)

t.merge <- merge(x=t.1998, y=t.1999, by="FIPS")
t.merge <- merge(x=t.merge, y=t.2000, by="FIPS")
t.merge <- merge(x=t.merge, y=t.2001, by="FIPS")
t.merge <- merge(x=t.merge, y=t.2002, by="FIPS")
t.merge <- merge(x=t.merge, y=t.2003, by="FIPS")
t.merge <- merge(x=t.merge, y=t.2004, by="FIPS")
t.merge <- merge(x=t.merge, y=t.2005, by="FIPS")
t.merge <- merge(x=t.merge, y=t.2006, by="FIPS")
t.merge <- merge(x=t.merge, y=t.2007, by="FIPS")
t.merge <- merge(x=t.merge, y=t.2008, by="FIPS")
t.merge <- merge(x=t.merge, y=t.2009, by="FIPS")
t.merge <- merge(x=t.merge, y=t.2010, by="FIPS")
t.merge <- merge(x=t.merge, y=t.2011, by="FIPS")
t.merge <- merge(x=t.merge, y=t.2012, by="FIPS")
t.merge <- merge(x=t.merge, y=t.2013, by="FIPS")
t.merge <- merge(x=t.merge, y=t.2014, by="FIPS")
t.merge <- merge(x=t.merge, y=t.2015, by="FIPS")
t.merge <- merge(x=t.merge, y=t.2016, by="FIPS")
t.merge <- merge(x=t.merge, y=t.2017, by="FIPS")

t.merge2 <- t.merge %>% 
  gather(key="Year", value="Temperature", 2:21)


#Precipitation - Importing --------------------------------
p.1998 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep1998.xlsx") %>% 
  select(2,6)
p.1999 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep1999.xlsx") %>% 
  select(2,6)
p.2000 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep2000.xlsx") %>% 
  select(2,6)
p.2001 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep2001.xlsx") %>% 
  select(2,6)
p.2002 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep2002.xlsx") %>% 
  select(2,6)
p.2003 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep2003.xlsx") %>% 
  select(2,6)
p.2004 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep2004.xlsx") %>% 
  select(2,6)
p.2005 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep2005.xlsx") %>% 
  select(2,6)
p.2006 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep2006.xlsx") %>% 
  select(2,6)
p.2007 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep2007.xlsx") %>% 
  select(2,6)
p.2008 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep2008.xlsx") %>% 
  select(2,6)
p.2009 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep2009.xlsx") %>% 
  select(2,6)
p.2010 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep2010.xlsx") %>% 
  select(2,6)
p.2011 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep2011.xlsx") %>% 
  select(2,6)
p.2012 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep2012.xlsx") %>% 
  select(2,6)
p.2013 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep2013.xlsx") %>% 
  select(2,6)
p.2014 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep2014.xlsx") %>% 
  select(2,6)
p.2015 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep2015.xlsx") %>% 
  select(2,6)
p.2016 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep2016.xlsx") %>% 
  select(2,6)
p.2017 <- read_excel("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/Week 4/Erfle - GISS(6)/prep2017.xlsx") %>% 
  select(2,6)

p.merge <- merge(x=p.1998, y=p.1999, by="FIPS")
p.merge <- merge(x=p.merge, y=p.2000, by="FIPS")
p.merge <- merge(x=p.merge, y=p.2001, by="FIPS")
p.merge <- merge(x=p.merge, y=p.2002, by="FIPS")
p.merge <- merge(x=p.merge, y=p.2003, by="FIPS")
p.merge <- merge(x=p.merge, y=p.2004, by="FIPS")
p.merge <- merge(x=p.merge, y=p.2005, by="FIPS")
p.merge <- merge(x=p.merge, y=p.2006, by="FIPS")
p.merge <- merge(x=p.merge, y=p.2007, by="FIPS")
p.merge <- merge(x=p.merge, y=p.2008, by="FIPS")
p.merge <- merge(x=p.merge, y=p.2009, by="FIPS")
p.merge <- merge(x=p.merge, y=p.2010, by="FIPS")
p.merge <- merge(x=p.merge, y=p.2011, by="FIPS")
p.merge <- merge(x=p.merge, y=p.2012, by="FIPS")
p.merge <- merge(x=p.merge, y=p.2013, by="FIPS")
p.merge <- merge(x=p.merge, y=p.2014, by="FIPS")
p.merge <- merge(x=p.merge, y=p.2015, by="FIPS")
p.merge <- merge(x=p.merge, y=p.2016, by="FIPS")
p.merge <- merge(x=p.merge, y=p.2017, by="FIPS")

p.merge2 <- p.merge %>% 
  gather(key="Year", value="Precipitation", 2:21)

#Merging all -------------------------------
tp.merge <- merge(x=t.merge2, y=p.merge2, by=c("FIPS", "Year"))

corn <- read_excel("corn3_final.xlsx") %>% 
  select(5,2,6)
names(corn)[1] <- "FIPS"
names(corn)[3] <- "Yield"

reg <- merge(x=tp.merge, y=corn, by=c("FIPS", "Year"))

write.xlsx(reg, file="reg-stata.xlsx")



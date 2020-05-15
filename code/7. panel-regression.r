#Library
library(tidyverse)
library(readr)
library(stargazer)
library(knitr)
library(xtable)
library(ggplot2)
library(ggpubr)
library(readxl)
library(reshape2)

#Library - Panel regression
library(sandwich)
library(lmtest)
library(AER)
library(forecast)
library(plm)
library(strsplit)

data0 <- read_excel("reg-stata.xlsx")

data1 <- data0 %>% 
  select(-1) %>% 
  filter(Yield > 0)

#Wide to Long
data2 <- dcast(melt(data1, id.vars = c("FIPS", "Year")), FIPS~variable+Year) %>% 
  na.omit()
remove(data2)

#Panel Regression 
m1 <- pdata.frame(data1, index=c("FIPS", "Year"))
pdim(m1)
attach(m1)

#Fixed Effects Model
m1.fixed <- plm(Yield~Temperature+I(Temperature^2)+Precipitation+I(Precipitation^2), 
                data=m1, model="within", effect = "twoway")
stargazer(m1.fixed, type='text')

data2 <- data1 %>% 
  mutate(State = substr(FIPS, 0, 2)) %>% 
  mutate(State = case_when(State == "17" ~ "Illinois", 
                           State == "18" ~ "Indiana", 
                           State == "21" ~ "Kentucky",
                           State == "39" ~ "ohio",
                           State == "26" ~ "Michigan",
                           State == "55" ~ "Wisconsin",
                           State == "27" ~ "Minnesota",
                           State == "19" ~ "Iowa"))

write.csv(data2, file="stata-reg2.csv")





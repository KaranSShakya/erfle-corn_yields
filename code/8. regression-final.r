###   Library---------------
#General
library(tidyverse)
library(readr)
library(ggplot2)
library(readxl)
library(reshape2)

#Panel Regression
library(sandwich)
library(lmtest)
library(AER)
library(forecast)
library(plm)
library(stargazer)
library(knitr)
library(xtable)
library(margins)

###   Base data file---------------
data0 <- read_excel("reg-stata.xlsx")
data1 <- data0 %>% 
  select(-1)

###   Panel Data - Fixed + Random + Hauseman---------------
m1 <- pdata.frame(data1, index=c("FIPS", "Year"))
pdim(m1)

#Fixed effects
m1.fixed <- plm(Yield~Temperature+I(Temperature^2)+Precipitation+I(Precipitation^2), 
                data=m1, model="within", effect = "twoway")


#Random effects
m1.random <- plm(Yield~Temperature+I(Temperature^2)+Precipitation+I(Precipitation^2), 
                data=m1, model="random")

#Hausman test
phtest(m1.fixed, m1.random)

#Fixed effects regression
m1.fixed <- plm(Yield~Temperature+I(Temperature^2)+Precipitation+I(Precipitation^2), 
                data=m1, model="within", effect = "twoway")
stargazer(m1.fixed, type='text')

#Clustering SE at individual level
coeftest(m1.fixed, vcovHC(m1.fixed, type="HC0", cluster = "group"))

###   Yield - Average Diagram------------------------
data.y <- data1 %>% 
  select(2,5) %>% 
  group_by(Year) %>% 
  summarise(Yield_avg=mean(Yield)) %>% 
  ungroup()

ggplot(data.y, aes(x=Year, y=Yield_avg))+
  geom_point()


###   Year by Year - Regression----------------------
remove(data0, m1.random)
## Temperature
#1998
data.1998 <- data1 %>% 
  filter(Year=="1998")
m1.1998 <- pdata.frame(data.1998, index=c("FIPS", "Year"))
lm.1998 <- lm(Yield~Temperature+I(Temperature^2)+Precipitation+I(Precipitation^2), 
              data=m1.1998)
plot.1998 <- cplot(lm.1998, "Temperature", what="prediction", main="1998")
head(plot.1998)

#2017
data.2017 <- data1 %>% 
  filter(Year=="2017")
m1.2017 <- pdata.frame(data.2017, index=c("FIPS", "Year"))
lm.2017 <- lm(Yield~Temperature+I(Temperature^2)+Precipitation+I(Precipitation^2), 
              data=m1.2017)
plot.2017 <- cplot(lm.2017, "Temperature", what="prediction", main="2017")
head(plot.2017)


#ggplot
ggplot(plot.1998, aes(x=xvals))+
  geom_line(aes(y=yvals), color="tan", size=1)+
  geom_line(data=plot.2017, aes(y=yvals), color="black", size=1)+
  labs(title="Changes in Corn Yield Sensitivity", x="Avg. Temp (C)", y="Corn Yield (BU/Acres)")+
  theme_light(base_size = 12)

## Precipitation
#1998
data.1998 <- data1 %>% 
  filter(Year=="1998")
m1.1998 <- pdata.frame(data.1998, index=c("FIPS", "Year"))
lm.1998 <- lm(Yield~Temperature+I(Temperature^2)+Precipitation+I(Precipitation^2), 
              data=m1.1998)
plot.1998 <- cplot(lm.1998, "Precipitation", what="prediction", main="1998")
head(plot.1998)

#2017
data.2017 <- data1 %>% 
  filter(Year=="2017")
m1.2017 <- pdata.frame(data.2017, index=c("FIPS", "Year"))
lm.2017 <- lm(Yield~Temperature+I(Temperature^2)+Precipitation+I(Precipitation^2), 
              data=m1.2017)
plot.2017 <- cplot(lm.2017, "Precipitation", what="prediction", main="2017")
head(plot.2017)

#2016
data.2016 <- data1 %>% 
  filter(Year=="2016")
m1.2016 <- pdata.frame(data.2016, index=c("FIPS", "Year"))
lm.2016 <- lm(Yield~Temperature+I(Temperature^2)+Precipitation+I(Precipitation^2), 
              data=m1.2016)
plot.2016 <- cplot(lm.2016, "Precipitation", what="prediction", main="2016")
head(plot.2016)


#ggplot
ggplot(plot.1998, aes(x=xvals))+
  geom_line(aes(y=yvals), color="tan", size=1)+
  geom_line(data=plot.2016, aes(y=yvals), color="black", size=1)+
  labs(title="Changes in Corn Yield Sensitivity", x="Avg. Precp (mm)", y="Corn Yield (BU/Acres)")+
  theme_light(base_size = 12)



###   Margins ???----------------------
summary(margins(data1))







###   Lag Regression ???---------------------

###   ARIMA Precition ???-------------------------


#Library--------------------
library(tidyverse)
library(readr)
library(xlsx)
library(reshape2)

#Importing Data-----------------
corn0 <- read_csv("Data/USDA-corn_1998-2018.csv", 
                  na = "NA")

corn1 <- corn0 %>% 
  select(2,6,7,10,11,20)

#Fixing County ANSI-----------
corn2 <- corn1 %>% 
  na.omit()
corn2$`State ANSI` <- sprintf("%02d", as.numeric(corn2$`State ANSI`))
corn2$`County ANSI` <- sprintf("%03d", as.numeric(corn2$`County ANSI`))
corn2$Ansi <- with(corn2, paste0(`State ANSI`,`County ANSI`))

corn2 <- corn2 %>% 
  select(1,2,4,7,6)

#Filtering sample state
corn3 <- corn2 %>% 
  filter(State == "ILLINOIS" | State == "INDIANA" | State == "IOWA" | State == "KENTUCKY" | State == "MICHIGAN" |
           State == "MINNESOTA" | State == "OHIO" | State == "WISCONSIN")

ggplot(corn2, aes(x=Year, y=Value))+
  geom_point(size=1)+
  stat_smooth(method = "lm", size=1, se=F, linetype="dashed")+
  stat_summary(fun.y=mean, geom="line", color="red", size=1)+
  labs(title="County-level Corn Yield", x="Year", y="Yield (BU/Acre)")+
  theme_classic(base_size = 14)

corn2_agg <- corn2 %>% 
  group_by(Year) %>% 
  summarise(National_Average = mean(Value)) %>% 
  ungroup()

corn3_agg <- corn3 %>% 
  group_by(Year) %>% 
  summarise(Sample_Average = mean(Value)) %>% 
  ungroup()

corn_agg <- merge(x=corn2_agg, y=corn3_agg, by="Year")

corn_tall <- corn_agg %>% 
  gather(key="Level", value="Corn_Yield", 2:3)

ggplot(corn_tall, aes(x=Year, y=Corn_Yield, color=Level))+
  geom_point(size=2)+
  stat_smooth(method = "lm", size=0.5, se=F, linetype="dashed")+
  labs(title="Corn Yield Average from 1998 to 2017", x="Year", y="Yield (BU/Acre)")+
  theme_classic(base_size = 14)+
  scale_x_continuous(breaks = seq(1998, 2017, 2))





# #Year-------------------------------------------
# c.1998 <- corn2 %>% 
#   filter(Year == "1998")
# names(c.1998)[3] <- ""
# c.1999 <- corn2 %>% 
#   filter(Year == "1999")
# c.2000 <- corn2 %>% 
#   filter(Year == "2000")
# c.2001 <- corn2 %>% 
#   filter(Year == "2001")
# c.2002 <- corn2 %>% 
#   filter(Year == "2002")
# c.2003 <- corn2 %>% 
#   filter(Year == "2003")
# c.2004 <- corn2 %>% 
#   filter(Year == "2004")
# c.2005 <- corn2 %>% 
#   filter(Year == "2005")
# c.2006 <- corn2 %>% 
#   filter(Year == "2006")
# c.2007 <- corn2 %>% 
#   filter(Year == "2007")
# c.2008 <- corn2 %>% 
#   filter(Year == "2008")
# c.2009 <- corn2 %>% 
#   filter(Year == "2009")
# c.2010 <- corn2 %>% 
#   filter(Year == "2010")
# c.2011 <- corn2 %>% 
#   filter(Year == "2011")
# c.2012 <- corn2 %>% 
#   filter(Year == "2012")
# c.2013 <- corn2 %>% 
#   filter(Year == "2013")
# c.2014 <- corn2 %>% 
#   filter(Year == "2014")
# c.2015 <- corn2 %>% 
#   filter(Year == "2015")
# c.2016 <- corn2 %>% 
#   filter(Year == "2016")
# c.2017 <- corn2 %>% 
#   filter(Year == "2017")
# c.2018 <- corn2 %>% 
#   filter(Year == "2018")

#Exporting csv---------------
# write.csv(c.2018, file="corn_2018.csv")
# write.xlsx(corn3, file="corn3_final.xlsx")




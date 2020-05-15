#Library
library(tidyverse)
library(readr)

#Importing dataset
yield0 <- read_csv("C:/Users/Karan/OneDrive - Dickinson College/Spring 2020/Econ 314 - Applied Empirical Data/R - Data Analysis/USDA/USDA-corn yield_1998_2018.csv", 
                   na = "NA")
yield1 <- yield0 %>% 
  select(2,6,7,10,11,20)

#Selecting 10 states
yield2 <- yield1 %>%
  filter(`State ANSI` == "30" | `State ANSI` == "38" | `State ANSI` == "46" |
           `State ANSI` == "56" | `State ANSI` == "31" | `State ANSI` == "8" |
           `State ANSI` == "20" | `State ANSI` == "35" | `State ANSI` == "40" |
           `State ANSI` == "48")
remove(yield0, yield1)

#Filter by State--------------------------------
mont <- yield2 %>% 
  filter(State == "MONTANA")
ndak <- yield2 %>% 
  filter(State == "NORTH DAKOTA")
sdak <- yield2 %>% 
  filter(State == "SOUTH DAKOTA")
wyom <- yield2 %>% 
  filter(State == "WYOMING")
nebr <- yield2 %>% 
  filter(State == "NEBRASKA")
colo <- yield2 %>% 
  filter(State == "COLORADO")
kans <- yield2 %>% 
  filter(State == "KANSAS")
newm <- yield2 %>% 
  filter(State == "NEW MEXICO")
okla <- yield2 %>% 
  filter(State == "OKLAHOMA")
texa <- yield2 %>% 
  filter(State == "TEXAS")

#Montana 2016 - 1998--------------------
mont.1998 <- mont %>%
  filter(Year == "1998")
mont.1999 <- mont %>%
  filter(Year == "1999")
mont.2000 <- mont %>%
  filter(Year == "2000")
mont.2001 <- mont %>%
  filter(Year == "2001")
mont.2002 <- mont %>%
  filter(Year == "2002")
mont.2003 <- mont %>%
  filter(Year == "2003")
mont.2004 <- mont %>%
  filter(Year == "2004")
mont.2005 <- mont %>%
  filter(Year == "2005")
mont.2006 <- mont %>%
  filter(Year == "2006")
mont.2007 <- mont %>%
  filter(Year == "2007")
mont.2008 <- mont %>%
  filter(Year == "2008")
mont.2009 <- mont %>%
  filter(Year == "2009")
mont.2010 <- mont %>%
  filter(Year == "2010")
mont.2011 <- mont %>%
  filter(Year == "2011")
mont.2012 <- mont %>%
  filter(Year == "2012")
mont.2013 <- mont %>%
  filter(Year == "2013")
mont.2014 <- mont %>%
  filter(Year == "2014")
mont.2015 <- mont %>%
  filter(Year == "2015")
mont.2016 <- mont %>%
  filter(Year == "2016")
remove(mont)

#North Dakota 2018 - 1998--------------------
ndak.1998 <- ndak %>%
  filter(Year == "1998")
ndak.1999 <- ndak %>%
  filter(Year == "1999")
ndak.2000 <- ndak %>%
  filter(Year == "2000")
ndak.2001 <- ndak %>%
  filter(Year == "2001")
ndak.2002 <- ndak %>%
  filter(Year == "2002")
ndak.2003 <- ndak %>%
  filter(Year == "2003")
ndak.2004 <- ndak %>%
  filter(Year == "2004")
ndak.2005 <- ndak %>%
  filter(Year == "2005")
ndak.2006 <- ndak %>%
  filter(Year == "2006")
ndak.2007 <- ndak %>%
  filter(Year == "2007")
ndak.2008 <- ndak %>%
  filter(Year == "2008")
ndak.2009 <- ndak %>%
  filter(Year == "2009")
ndak.2010 <- ndak %>%
  filter(Year == "2010")
ndak.2011 <- ndak %>%
  filter(Year == "2011")
ndak.2012 <- ndak %>%
  filter(Year == "2012")
ndak.2013 <- ndak %>%
  filter(Year == "2013")
ndak.2014 <- ndak %>%
  filter(Year == "2014")
ndak.2015 <- ndak %>%
  filter(Year == "2015")
ndak.2016 <- ndak %>%
  filter(Year == "2016")
remove(ndak)

#South Dakota 2018 - 1988------------------
sdak.1998 <- sdak %>%
  filter(Year == "1998")
sdak.1999 <- sdak %>%
  filter(Year == "1999")
sdak.2000 <- sdak %>%
  filter(Year == "2000")
sdak.2001 <- sdak %>%
  filter(Year == "2001")
sdak.2002 <- sdak %>%
  filter(Year == "2002")
sdak.2003 <- sdak %>%
  filter(Year == "2003")
sdak.2004 <- sdak %>%
  filter(Year == "2004")
sdak.2005 <- sdak %>%
  filter(Year == "2005")
sdak.2006 <- sdak %>%
  filter(Year == "2006")
sdak.2007 <- sdak %>%
  filter(Year == "2007")
sdak.2008 <- sdak %>%
  filter(Year == "2008")
sdak.2009 <- sdak %>%
  filter(Year == "2009")
sdak.2010 <- sdak %>%
  filter(Year == "2010")
sdak.2011 <- sdak %>%
  filter(Year == "2011")
sdak.2012 <- sdak %>%
  filter(Year == "2012")
sdak.2013 <- sdak %>%
  filter(Year == "2013")
sdak.2014 <- sdak %>%
  filter(Year == "2014")
sdak.2015 <- sdak %>%
  filter(Year == "2015")
sdak.2016 <- sdak %>%
  filter(Year == "2016")
remove(sdak)







#Wyoming 2016 - 1998-----------------
wyom.1998 <- wyom %>%
  filter(Year == "1998")
wyom.1999 <- wyom %>%
  filter(Year == "1999")
wyom.2000 <- wyom %>%
  filter(Year == "2000")
wyom.2001 <- wyom %>%
  filter(Year == "2001")
wyom.2002 <- wyom %>%
  filter(Year == "2002")
wyom.2003 <- wyom %>%
  filter(Year == "2003")
wyom.2004 <- wyom %>%
  filter(Year == "2004")
wyom.2005 <- wyom %>%
  filter(Year == "2005")
wyom.2006 <- wyom %>%
  filter(Year == "2006")
wyom.2007 <- wyom %>%
  filter(Year == "2007")
wyom.2008 <- wyom %>%
  filter(Year == "2008")
wyom.2009 <- wyom %>%
  filter(Year == "2009")
wyom.2010 <- wyom %>%
  filter(Year == "2010")
wyom.2011 <- wyom %>%
  filter(Year == "2011")
wyom.2012 <- wyom %>%
  filter(Year == "2012")
wyom.2013 <- wyom %>%
  filter(Year == "2013")
wyom.2014 <- wyom %>%
  filter(Year == "2014")
wyom.2015 <- wyom %>%
  filter(Year == "2015")
wyom.2016 <- wyom %>%
  filter(Year == "2016")
remove(wyom)





#Colorado 2018 - 1998---------------------
colo.1998 <- colo %>%
  filter(Year == "1998")
colo.1999 <- colo %>%
  filter(Year == "1999")
colo.2000 <- colo %>%
  filter(Year == "2000")
colo.2001 <- colo %>%
  filter(Year == "2001")
colo.2002 <- colo %>%
  filter(Year == "2002")
colo.2003 <- colo %>%
  filter(Year == "2003")
colo.2004 <- colo %>%
  filter(Year == "2004")
colo.2005 <- colo %>%
  filter(Year == "2005")
colo.2006 <- colo %>%
  filter(Year == "2006")
colo.2007 <- colo %>%
  filter(Year == "2007")
colo.2008 <- colo %>%
  filter(Year == "2008")
colo.2009 <- colo %>%
  filter(Year == "2009")
colo.2010 <- colo %>%
  filter(Year == "2010")
colo.2011 <- colo %>%
  filter(Year == "2011")
colo.2012 <- colo %>%
  filter(Year == "2012")
colo.2013 <- colo %>%
  filter(Year == "2013")
colo.2014 <- colo %>%
  filter(Year == "2014")
colo.2015 <- colo %>%
  filter(Year == "2015")
colo.2016 <- colo %>%
  filter(Year == "2016")
remove(colo)



#Kansas 2018 - 1998----------------------------
kans.1998 <- kans %>%
  filter(Year == "1998")
kans.1999 <- kans %>%
  filter(Year == "1999")
kans.2000 <- kans %>%
  filter(Year == "2000")
kans.2001 <- kans %>%
  filter(Year == "2001")
kans.2002 <- kans %>%
  filter(Year == "2002")
kans.2003 <- kans %>%
  filter(Year == "2003")
kans.2004 <- kans %>%
  filter(Year == "2004")
kans.2005 <- kans %>%
  filter(Year == "2005")
kans.2006 <- kans %>%
  filter(Year == "2006")
kans.2007 <- kans %>%
  filter(Year == "2007")
kans.2008 <- kans %>%
  filter(Year == "2008")
kans.2009 <- kans %>%
  filter(Year == "2009")
kans.2010 <- kans %>%
  filter(Year == "2010")
kans.2011 <- kans %>%
  filter(Year == "2011")
kans.2012 <- kans %>%
  filter(Year == "2012")
kans.2013 <- kans %>%
  filter(Year == "2013")
kans.2014 <- kans %>%
  filter(Year == "2014")
kans.2015 <- kans %>%
  filter(Year == "2015")
kans.2016 <- kans %>%
  filter(Year == "2016")
remove(kans)



#New Mexico 2018 - 1998---------------------
newm.1998 <- newm %>%
  filter(Year == "1998")
newm.1999 <- newm %>%
  filter(Year == "1999")
newm.2000 <- newm %>%
  filter(Year == "2000")
newm.2001 <- newm %>%
  filter(Year == "2001")
newm.2002 <- newm %>%
  filter(Year == "2002")
newm.2003 <- newm %>%
  filter(Year == "2003")
newm.2004 <- newm %>%
  filter(Year == "2004")
newm.2005 <- newm %>%
  filter(Year == "2005")
newm.2006 <- newm %>%
  filter(Year == "2006")
newm.2007 <- newm %>%
  filter(Year == "2007")
newm.2008 <- newm %>%
  filter(Year == "2008")
newm.2009 <- newm %>%
  filter(Year == "2009")
newm.2010 <- newm %>%
  filter(Year == "2010")
newm.2011 <- newm %>%
  filter(Year == "2011")
newm.2012 <- newm %>%
  filter(Year == "2012")
newm.2013 <- newm %>%
  filter(Year == "2013")
newm.2014 <- newm %>%
  filter(Year == "2014")
newm.2015 <- newm %>%
  filter(Year == "2015")
newm.2016 <- newm %>%
  filter(Year == "2016")
remove(newm)


#Oklahoma 2018 - 1988--------------------
okla.1998 <- okla %>%
  filter(Year == "1998")
okla.1999 <- okla %>%
  filter(Year == "1999")
okla.2000 <- okla %>%
  filter(Year == "2000")
okla.2001 <- okla %>%
  filter(Year == "2001")
okla.2002 <- okla %>%
  filter(Year == "2002")
okla.2003 <- okla %>%
  filter(Year == "2003")
okla.2004 <- okla %>%
  filter(Year == "2004")
okla.2005 <- okla %>%
  filter(Year == "2005")
okla.2006 <- okla %>%
  filter(Year == "2006")
okla.2007 <- okla %>%
  filter(Year == "2007")
okla.2008 <- okla %>%
  filter(Year == "2008")
okla.2009 <- okla %>%
  filter(Year == "2009")
okla.2010 <- okla %>%
  filter(Year == "2010")
okla.2011 <- okla %>%
  filter(Year == "2011")
okla.2012 <- okla %>%
  filter(Year == "2012")
okla.2013 <- okla %>%
  filter(Year == "2013")
okla.2014 <- okla %>%
  filter(Year == "2014")
okla.2015 <- okla %>%
  filter(Year == "2015")
okla.2016 <- okla %>%
  filter(Year == "2016")
remove(okla)

#Texas 2018 - 1998------------------------
texa.1998 <- texa %>%
  filter(Year == "1998")
texa.1999 <- texa %>%
  filter(Year == "1999")
texa.2000 <- texa %>%
  filter(Year == "2000")
texa.2001 <- texa %>%
  filter(Year == "2001")
texa.2002 <- texa %>%
  filter(Year == "2002")
texa.2003 <- texa %>%
  filter(Year == "2003")
texa.2004 <- texa %>%
  filter(Year == "2004")
texa.2005 <- texa %>%
  filter(Year == "2005")
texa.2006 <- texa %>%
  filter(Year == "2006")
texa.2007 <- texa %>%
  filter(Year == "2007")
texa.2008 <- texa %>%
  filter(Year == "2008")
texa.2009 <- texa %>%
  filter(Year == "2009")
texa.2010 <- texa %>%
  filter(Year == "2010")
texa.2011 <- texa %>%
  filter(Year == "2011")
texa.2012 <- texa %>%
  filter(Year == "2012")
texa.2013 <- texa %>%
  filter(Year == "2013")
texa.2014 <- texa %>%
  filter(Year == "2014")
texa.2015 <- texa %>%
  filter(Year == "2015")
texa.2016 <- texa %>%
  filter(Year == "2016")
remove(texa)





#Binding states

library(rjags)
library(runjags)
load.module("mix")
library(tidyverse)
library(ggmcmc)
library(readxl)
library(forcats)

# Path for input data
pathIn<-"H:/Projects/ISAMA/data/orig/"

# Catches (kg)
# ============
D1<-read_xlsx(paste(sep="",pathIn,"MMM 2016 TULOSTAVOITE.xlsx"),sheet="T lohisaalis FIN NOR 1972-", na="", 
              range="A7:E50", col_names=c("year", "catch_FI", "%_FI", "catch_NO", "catch_sum"))

# License days
# ============
D2<-read_xlsx(paste(sep="",pathIn,"MMM 2016 TULOSTAVOITE.xlsx"),sheet="T matkavrkt FIN 1990-", na="*", 
              range="A7:K33",
              col_names=c(
                "year", "boat_Teno", "boat_Teno_spouse", "boat_Inari-Skie", "boat_Inari-Skie_spouse",
                "shore_Teno", "shore_Teno_spouse", "shore_Borat", "shore_Borat_spouse", "weekly", "total_days"))
D2<-select(D2, year,total_days, everything()) # put year first
View(D2)

# Number of licence days per area
# ============
d2<-read_xlsx(paste(sep="",pathIn,"MMM 2016 TULOSTAVOITE.xlsx"),sheet="T matkavrk-aluejako FIN 2000-", na="", 
                   range="A5:R11", col_names=T)

d22<- d2%>% gather(key="year", value="num_license", `2000`:`2016`) # re-arrange  
d2<-select(d22, year, everything()) # put year first

# Catch per area (kg)
# ============
d3<-read_xlsx(paste(sep="",pathIn,"MMM 2016 TULOSTAVOITE.xlsx"),sheet="T matkalohi-aluejako FIN 2000-", na="", 
              range="A4:R10", col_names=T)

d33<- d3%>% gather(key="year", value="catch", `2000`:`2016`) # re-arrange  
d3<-select(d33, year, everything()) # put year first


# Join total catches and licence days
# =====================================

D11<-select(D1, year,catch_FI, catch_NO)
D11<- D11%>% gather(key="Country", value="catch", catch_FI:catch_NO) 

D11<-mutate(D11,Country= fct_recode(Country,
                                    "FI"="catch_FI", 
                                    "NO"="catch_NO"))

D22<-select(D2, year, total_days)

(Dat<-full_join(D11,D22, by=NULL))

Dat<-mutate(Dat, cpue=catch/total_days)
View(Dat)


ggplot(data = Dat)+ 
  geom_line(aes(x = year, y = catch, color=Country), size=1.2)+
  labs(title="Catch in kg")


ggplot(data = filter(Dat, is.na(cpue)==F))+ 
  geom_line(aes(x = year, y = cpue, color=Country), size=1.2)+
  labs(title="cpue")




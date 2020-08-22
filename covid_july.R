setwd("C:/Users/Prol/Documents/R/wd/covid")
set.seed(1)

library(tidyverse)
library(gganimate)
library(zoo)
library(forecast)
library(fable)
library(ggplot2)
library(tsibble)
library(feasts)
library(lubridate)
library(opera)
library(ggExtra)
library(mgcv)
library(timeDate)
library(lmtest)
library(devtools)
library(EIAdata) # install_github("Matt-Brigida/EIAdata", force = T) # dev. version
library(xts)
library(plm)
library(directlabels)
library(gtable)
library(gridExtra)
library(viridis)
library(scales)
library(broom)
library(data.table)
library(sandwich)
library(plotly)
library(gganimate)
library(ggrepel)

write.png<- function(object, filename, width=5, height=4, pointsize=12, res=300){
  png(filename= filename, type="cairo",units="in", width=width,
      height=height, pointsize=pointsize, res=res)
  plot(object)
  dev.off()
  object
}

mytheme <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.border     = element_blank(),
      axis.line        = element_blank(),
      axis.ticks = element_line(color = "grey"),
      panel.grid.major = element_line(),
      panel.grid.major.x = element_line(colour = "grey", size = 0.1, linetype = "dotted"),
      panel.grid.major.y = element_line(colour = "grey", size = 0.1, linetype = "dotted"),
      panel.grid.minor = element_line(),
      panel.grid.minor.x = element_line(colour = "grey", size = 0.1, linetype = "dotted"),
      panel.grid.minor.y = element_line(colour = "grey", size = 0.1, linetype = "dotted"),
      strip.background = element_blank(),
      legend.key       = element_blank()
    )
}

`%nin%` = Negate(`%in%`)

# this version 2 (ddata2.rds and ddata_all2.rds) corrected input with AreaName i.o. AreaCode (not unique)

######################################### 1. Data #########################################
# we include all countries now and data is saved as "dload_all.rds"
### 1.1. ENTSO-E  ####
# import raw ENTSO-E data and aggregate to daily values

get_dload<- function(years = c(2015:2020), months = (1:12)){
  for(y in years){
    for(m in months){
      
      raw_data<- read.csv(paste("D:/data/ENTSO-E/load/actual/raw/", y, "_", m,  "_ActualTotalLoad.csv", sep = ""), 
                          sep = "\t", header = T, fileEncoding = "UTF-16")
      
      # CET time
      c1<- raw_data %>% 
        filter(AreaName %in% c("Italy", # IT
                               "Spain", # ES
                               "France", # FR
                               "Switzerland", # CH
                               "Czech Republic", # CZ
                               "Denmark", # DK
                               "Croatia", # HR
                               "Norway", # NO
                               "Poland", # PL
                               "Slovenia", # SI
                               "Slovakia",  # SK
                               "Austria", # AT
                               "Germany", # DE
                               "Hungary", # HU
                               "Netherlands"  # NL
               )) %>%  
        select(year = Year, month = Month, day = Day, datetime = DateTime, country = MapCode, 
               load = TotalLoadValue) %>% 
        mutate(datetime = as.POSIXct(datetime, tz = "UTC")) %>% 
        mutate(local_datetime = format(datetime, tz= "Europe/Berlin")) %>%
        mutate(date = as.Date(substr(local_datetime, 1, 10))) %>%
        group_by(country, date) %>% summarise(load = sum(load)/1000) %>%
        droplevels() %>% ungroup() %>% as_tibble()
      
      
      # UK time
      c2<- raw_data %>%
        filter(AreaName %in% c("National Grid BZ", # UK
                               "Ireland", # IE 
                               "Portugal"  # PT
        )) %>% 
        select(year = Year, month = Month, day = Day, datetime = DateTime, country = MapCode, 
               load = TotalLoadValue) %>% 
        mutate(datetime = as.POSIXct(datetime, tz = "UTC")) %>% 
        mutate(local_datetime = format(datetime, tz= "Europe/London")) %>%
        mutate(date = as.Date(substr(local_datetime, 1, 10))) %>%
        group_by(country, date) %>% summarise(load = sum(load)/1000) %>%
        droplevels() %>% ungroup() %>% as_tibble()
      
      # Eastern European time
      c3<- raw_data %>%
        filter(AreaName %in% c("Bulgaria", # BG
                               "Estonia", # EE
                               "Finland", # FI
                               "Greece", # GR
                               "Lithuania", # LT
                               "Latvia", # LV
                               "Romania"  # RO
        )) %>%  
        select(year = Year, month = Month, day = Day, datetime = DateTime, country = MapCode, 
               load = TotalLoadValue) %>% 
        mutate(datetime = as.POSIXct(datetime, tz = "UTC")) %>% 
        mutate(local_datetime = format(datetime, tz= "Europe/Athens")) %>%
        mutate(date = as.Date(substr(local_datetime, 1, 10))) %>%
        group_by(country, date) %>% summarise(load = sum(load)/1000) %>%
        droplevels() %>% ungroup() %>% as_tibble()
      
      
      load<- bind_rows(c1, c2, c3) %>% mutate(country = as.factor(country)) %>% arrange(country, date)
      
      saveRDS(load, paste("D:/data/ENTSO-E/load/actual/daily/", y, "_", m,  "_dload_all2.rds", sep = ""))
      
    }
  }
}

get_dload()

# bind all data in one file # gives error when finishes it's OK
bind_dload<- function(){
  df<- data_frame()
  for(y in c(2015:2020)){
    for(m in c(1:12))
      tryCatch({
        load<- readRDS(paste("D:/data/ENTSO-E/load/actual/daily/", y, "_", m,  "_dload_all2.rds", sep = ""))
        df<- bind_rows(df, load)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  df<- df %>% group_by(country, date) %>% summarise(load = sum(load)) %>% ungroup()
  saveRDS(df, "D:/data/ENTSO-E/load/actual/daily/dload_all2.rds")
}

bind_dload() # it's normal to have errors at the end for months of no data till Dec. 2020

dload<- readRDS("D:/data/ENTSO-E/load/actual/daily/dload_all2.rds") %>%
  filter(date < "2020-05-31")

ggplot(dload, aes(date, load)) +geom_line()+facet_wrap(~country, scales = "free")


### 1.2. EIA  ####

key<- "0c8f439d44c52081266bf447f2329118"

getCatEIA(key=key) # index, also here: https://www.eia.gov/opendata/qb.php?category=371

getCatEIA(key=key, cat=2122628) # U.S. Electric System Operating Data > Demand by subregion

getCatEIA(key=key, cat=3389936) # CA                      EBA.CAL-ALL.D.H

getCatEIA(key = key, cat = 3389943) # NY New York         EBA.NY-ALL.D.H

getCatEIA(key = key, cat = 3389939) # FL Florida          EBA.FLA-ALL.D.H


# downloaded in UTC and then converted to real time 
# (see tz here: https://www.eia.gov/realtime_grid/docs/userguide-knownissues.pdf)

# California
cal<- as.xts(getEIA(key = key, ID = "EBA.CAL-ALL.D.H"), tz = "America/Los_Angeles")  %>% fortify() %>% 
  select(datetime = Index, CA = EBA.CAL.ALL.D.H) %>%
  mutate(date = as.Date(substr(datetime, 1, 10))) %>%
  group_by(date) %>% summarize(CA = sum(CA)/1000) %>% ungroup() %>% as_tibble()

# Carolinas
car<- as.xts(getEIA(key = key, ID = "EBA.CAR-ALL.D.H"), tz = "America/New_York")  %>% fortify() %>% 
  select(datetime = Index, CAR = EBA.CAR.ALL.D.H) %>%
  mutate(date = as.Date(substr(datetime, 1, 10))) %>%
  group_by(date) %>% summarize(CAR = sum(CAR)/1000) %>% ungroup() %>% as_tibble()

# Central
cent<- as.xts(getEIA(key = key, ID = "EBA.CENT-ALL.D.H"), tz = "America/Chicago")  %>% fortify() %>% 
  select(datetime = Index, CENT = EBA.CENT.ALL.D.H) %>%
  mutate(date = as.Date(substr(datetime, 1, 10))) %>%
  group_by(date) %>% summarize(CENT = sum(CENT)/1000) %>% ungroup() %>% as_tibble()

# Florida
fla<- as.xts(getEIA(key = key, ID = "EBA.FLA-ALL.D.H"), tz = "America/New_York") %>% fortify() %>% 
  select(datetime = Index, FL = EBA.FLA.ALL.D.H) %>%
  mutate(date = as.Date(substr(datetime, 1, 10))) %>%
  group_by(date) %>% summarize(FL = sum(FL)/1000) %>% ungroup() %>% as_tibble()

# Midatlantic
mida<- as.xts(getEIA(key = key, ID = "EBA.MIDA-ALL.D.H"), tz = "America/New_York") %>% fortify() %>% 
  select(datetime = Index, MIDA = EBA.MIDA.ALL.D.H) %>%
  mutate(date = as.Date(substr(datetime, 1, 10))) %>%
  group_by(date) %>% summarize(MIDA = sum(MIDA)/1000) %>% ungroup() %>% as_tibble()

# Midwest (central tz)
midw<- as.xts(getEIA(key = key, ID = "EBA.MIDW-ALL.D.H"), tz = "America/Chicago") %>% fortify() %>% 
  select(datetime = Index, MIDW = EBA.MIDW.ALL.D.H) %>%
  mutate(date = as.Date(substr(datetime, 1, 10))) %>%
  group_by(date) %>% summarize(MIDW = sum(MIDW)/1000) %>% ungroup() %>% as_tibble()

# New England
ne<- as.xts(getEIA(key = key, ID = "EBA.NE-ALL.D.H"), tz = "America/New_York") %>% fortify() %>% 
  select(datetime = Index, NE = EBA.NE.ALL.D.H) %>%
  mutate(date = as.Date(substr(datetime, 1, 10))) %>%
  group_by(date) %>% summarize(NE = sum(NE)/1000) %>% ungroup() %>% as_tibble()

# New York
ny<- as.xts(getEIA(key = key, ID = "EBA.NY-ALL.D.H"), tz = "America/New_York") %>% fortify() %>% 
  select(datetime = Index, NY = EBA.NY.ALL.D.H) %>%
  mutate(date = as.Date(substr(datetime, 1, 10))) %>%
  group_by(date) %>% summarize(NY = sum(NY)/1000) %>% ungroup() %>% as_tibble()

# North West
nw<- as.xts(getEIA(key = key, ID = "EBA.NW-ALL.D.H"), tz = "America/America/Los_Angeles") %>% fortify() %>% 
  select(datetime = Index, NW = EBA.NW.ALL.D.H) %>%
  mutate(date = as.Date(substr(datetime, 1, 10))) %>%
  group_by(date) %>% summarize(NW = sum(NW)/1000) %>% ungroup() %>% as_tibble()

# South East
se<- as.xts(getEIA(key = key, ID = "EBA.SE-ALL.D.H"), tz = "America/Chicago") %>% fortify() %>% 
  select(datetime = Index, SE = EBA.SE.ALL.D.H) %>%
  mutate(date = as.Date(substr(datetime, 1, 10))) %>%
  group_by(date) %>% summarize(SE = sum(SE)/1000) %>% ungroup() %>% as_tibble()

# South West
sw<- as.xts(getEIA(key = key, ID = "EBA.SW-ALL.D.H"), tz = "America/Phoenix") %>% fortify() %>% 
  select(datetime = Index, SW = EBA.SW.ALL.D.H) %>%
  mutate(date = as.Date(substr(datetime, 1, 10))) %>%
  group_by(date) %>% summarize(SW = sum(SW)/1000) %>% ungroup() %>% as_tibble()

# Tennessee (central tz)
ten<- as.xts(getEIA(key = key, ID = "EBA.TEN-ALL.D.H"), tz = "America/Chicago") %>% fortify() %>% 
  select(datetime = Index, TEN = EBA.TEN.ALL.D.H) %>%
  mutate(date = as.Date(substr(datetime, 1, 10))) %>%
  group_by(date) %>% summarize(TEN = sum(TEN)/1000) %>% ungroup() %>% as_tibble()

# Texas
tex<- as.xts(getEIA(key = key, ID = "EBA.TEX-ALL.D.H"), tz = "America/Austin") %>% fortify() %>% 
  select(datetime = Index, TEX = EBA.TEX.ALL.D.H) %>%
  mutate(date = as.Date(substr(datetime, 1, 10))) %>%
  group_by(date) %>% summarize(TEX = sum(TEX)/1000) %>% ungroup() %>% as_tibble()

usdload<- full_join(cal, car) %>% full_join(cent) %>% full_join(fla) %>%
  full_join(mida) %>% full_join(midw) %>% full_join(ne) %>% full_join(ny) %>%
  full_join(nw) %>% full_join(sw) %>% full_join(ten) %>% full_join(tex) %>%
  gather(country, load, -date)

# saveRDS(usdload, "D:/data/EIA/usdload.rds")

usdload %>% filter(date > "2020-03-01") %>% 
  ggplot(aes(date, load))+geom_line()+facet_wrap(~country, scales = "free")



### 1.3. Temperature  ####
tat<- read.delim("./data/raw/temperature/AT_maxtemp_2020.dat", sep = ",") %>%
  rename(date = day, max_temp = maxT) %>% mutate(date = as.Date(date), country = "AT") %>% as_tibble()

tde<- read.delim("./data/raw/temperature/DE_maxtemp_2020.dat", sep = ",") %>%
  rename(date = day, max_temp = maxT) %>% mutate(date = as.Date(date), country = "DE") %>% as_tibble()

tes<- read.delim("./data/raw/temperature/ES_maxtemp_2020.dat", sep = ",") %>%
  rename(date = day, max_temp = maxT) %>% mutate(date = as.Date(date), country = "ES") %>% as_tibble()

tit<- read.delim("./data/raw/temperature/IT_maxtemp_2020.dat", sep = ",") %>%
  rename(date = day, max_temp = maxT) %>% mutate(date = as.Date(date), country = "IT") %>% as_tibble()

tfr<- read.delim("./data/raw/temperature/FR_maxtemp_2020.dat", sep = ",") %>%
  rename(date = day, max_temp = maxT) %>% mutate(date = as.Date(date), country = "FR") %>% as_tibble()

tgb<- read.delim("./data/raw/temperature/GB_maxtemp_2020.dat", sep = ",") %>%
  rename(date = day, max_temp = maxT) %>% mutate(date = as.Date(date), country = "GB") %>% as_tibble()

tca<- read.delim("./data/raw/temperature/CA_maxtemp_2020.dat", sep = ",") %>%
  rename(date = day, max_temp = maxT) %>% mutate(date = as.Date(date), country = "CA") %>% as_tibble()

tny<- read.delim("./data/raw/temperature/NY_maxtemp_2020.dat", sep = ",") %>%
  rename(date = day, max_temp = maxT) %>% mutate(date = as.Date(date), country = "NY") %>% as_tibble()

tfl<- read.delim("./data/raw/temperature/FL_maxtemp_2020.dat", sep = ",") %>%
  rename(date = day, max_temp = maxT) %>% mutate(date = as.Date(date), country = "FL") %>% as_tibble()


temperature<- bind_rows(tat, tit, tes, tde, tfr, tgb, tca, tny, tfl)

# saveRDS(temperature, "./data/raw/temperature/temperature.rds")

### 1.4. Holiday  ####

# AT
labday<- as.Date(c("2015-05-01", "2016-05-01", "2017-05-01", "2018-05-01", "2019-05-01", "2020-05-01"))
ATwhitsun<- as.Date(c("2015-05-31", "2016-05-31", "2017-05-31", "2018-05-31", "2019-05-31", "2020-05-31"))
ATwhitmon<- as.Date(c("2015-06-01", "2016-06-01", "2017-06-01", "2018-06-01", "2019-06-01", "2020-06-01"))
ATnatday<- as.Date(c("2015-06-02", "2016-06-02", "2017-06-02", "2018-06-02", "2019-06-02", "2020-06-02"))
ATstepday<- as.Date(c("2015-12-26", "2016-12-26", "2017-12-26", "2018-12-26", "2019-12-26", "2020-12-26"))
atholiday_string<- c(as.Date(holiday(2015:2020, Holiday = c("NewYearsDay", "Epiphany", "GoodFriday", 
                                                    "EasterSunday", "EasterMonday", "AssumptionOfMary",
                                                    "AllSaints", "ChristmasDay",
                                                    "DEAscension", "DECorpusChristi"))),
                     labday, ATwhitsun, ATwhitmon, ATnatday, ATstepday)

atholiday<- data.frame(date = seq(as.Date("2015-01-01"), as.Date("2020-12-31"), by = 1)) %>%
  left_join(data.frame(date = atholiday_string, holiday = rep(1, length(atholiday_string)))) %>%
  mutate(holiday = as.factor(replace_na(holiday, 0)), country = "AT") %>% as_tibble()


# ES
ESnatday<- as.Date(c("2015-10-12", "2016-10-12", "2017-10-12", "2018-10-12", "2019-10-12", "2020-10-12"))
ESloadtday<- as.Date(c("2015-12-06", "2016-12-06", "2017-12-06", "2018-12-06", "2019-12-06", "2020-12-06"))
esholiday_string<- c(as.Date(holiday(2015:2020, Holiday = c("NewYearsDay", "Epiphany", "GoodFriday", 
                                                    "EasterSunday", "EasterMonday", "AssumptionOfMary",
                                                    "AllSaints", "ChristmasDay"))),
                     labday, ESnatday, ESloadtday)

esholiday<- data.frame(date = seq(as.Date("2015-01-01"), as.Date("2020-12-31"), by = 1)) %>%
  left_join(data.frame(date = esholiday_string, holiday = rep(1, length(esholiday_string)))) %>%
  mutate(holiday = as.factor(replace_na(holiday, 0)), country = "ES") %>% as_tibble()

# IT
ITferrag<- as.Date(c("2015-08-15", "2016-08-15", "2017-08-15", "2018-08-15", "2019-08-15", "2020-08-15"))
ITrepday<- as.Date(c("2015-06-02", "2016-06-02", "2017-06-02", "2018-06-02", "2019-06-02", "2020-06-02"))
ITstepday<- as.Date(c("2015-12-26", "2016-12-26", "2017-12-26", "2018-12-26", "2019-12-26", "2020-12-26"))
itholiday_string<- c(as.Date(holiday(2015:2020, Holiday = c("NewYearsDay", "Epiphany", "GoodFriday", 
                                                    "EasterSunday", "EasterMonday", "AssumptionOfMary",
                                                    "AllSaints", "ChristmasDay",
                                                    "ITLiberationDay", "ITAssumptionOfVirginMary",
                                                    "ITStAmrose", "ITImmaculateConception"))),
                     labday, ITferrag, ITrepday, ITstepday)

itholiday<- data.frame(date = seq(as.Date("2015-01-01"), as.Date("2020-12-31"), by = 1)) %>%
  left_join(data.frame(date = itholiday_string, holiday = rep(1, length(itholiday_string)))) %>%
  mutate(holiday = as.factor(replace_na(holiday, 0)), country = "IT") %>% as_tibble()

# DE
DEwhitmon<- as.Date(c("2015-06-01", "2016-06-01", "2017-06-01", "2018-06-01", "2019-06-01", "2020-06-01"))
DEunitday<- as.Date(c("2015-10-03", "2016-10-03", "2017-10-03", "2018-10-03", "2019-10-03", "2020-10-03"))
DEstepday<- as.Date(c("2015-12-26", "2016-12-26", "2017-12-26", "2018-12-26", "2019-12-26", "2020-12-26"))
deholiday_string<- c(as.Date(holiday(2015:2020, Holiday = c("NewYearsDay", "GoodFriday",  "EasterSunday", 
                                                    "EasterMonday", "ChristmasDay",
                                                    "DEAscension", "DECorpusChristi"))),
                     labday, DEwhitmon, DEunitday, DEstepday) %>% as.Date()

deholiday<- data.frame(date = seq(as.Date("2015-01-01"), as.Date("2020-12-31"), by = 1)) %>%
  left_join(data.frame(date = deholiday_string, holiday = rep(1, length(deholiday_string)))) %>%
  mutate(holiday = as.factor(replace_na(holiday, 0)), country = "DE") %>% as_tibble()

# FR
frholiday_string<- c(as.Date(holiday(2015:2020, Holiday = c("NewYearsDay", "GoodFriday",  "EasterSunday", 
                                                    "EasterMonday", "ChristmasDay", "DECorpusChristi", 
                                                    "FRFetDeLaVictoire1945", "FRAscension", "FRBastilleDay",
                                                    "FRAssumptionVirginMary", "FRAllSaints", 
                                                    "FRArmisticeDay"))),labday)

frholiday<- data.frame(date = seq(as.Date("2015-01-01"), as.Date("2020-12-31"), by = 1)) %>%
  left_join(data.frame(date = frholiday_string, holiday = rep(1, length(frholiday_string)))) %>%
  mutate(holiday = as.factor(replace_na(holiday, 0)), country = "FR") %>% as_tibble()

# GB
gbboxday<- as.Date(c("2015-12-28", "2016-12-28", "2017-12-28", "2018-12-28", "2019-12-28", "2020-12-28"))
# outliers<- as.Date(c("2020-04-27", "2020-04-28"))
gbholiday_string<- c(as.Date(holiday(2015:2020, Holiday = c("NewYearsDay", "GoodFriday",  "GBMayDay",
                                                    "GBBankHoliday", "GBSummerBankHoliday"))),
                     gbboxday)

gbholiday<- data.frame(date = seq(as.Date("2015-01-01"), as.Date("2020-12-31"), by = 1)) %>%
  left_join(data.frame(date = gbholiday_string, holiday = rep(1, length(gbholiday_string)))) %>%
  mutate(holiday = as.factor(replace_na(holiday, 0)), country = "GB") %>% as_tibble()

# US
usholiday_string<- c(as.Date(holiday(2015:2020, Holiday = c("USNewYearsDay", "USInaugurationDay", 
                                                    "USMLKingsBirthday", "USLincolnsBirthday", 
                                                    "USWashingtonsBirthday", "USMemorialDay",
                                                    "USIndependenceDay", "USLaborDay", "USColumbusDay",
                                                    "USElectionDay", "USVeteransDay", "USThanksgivingDay",
                                                    "USChristmasDay", "USCPulaskisBirthday", 
                                                    "USGoodFriday"))))

usholiday1<- data.frame(date = seq(as.Date("2015-01-01"), as.Date("2020-12-31"), by = 1)) %>%
  left_join(data.frame(date = usholiday_string, holiday = rep(1, length(usholiday_string)))) %>%
  mutate(holiday = as.factor(replace_na(holiday, 0))) %>% as_tibble()

usholiday<- bind_rows(usholiday1, usholiday1, usholiday1) %>%
  mutate(country = c(rep("CA", nrow(usholiday1)), rep("NY", nrow(usholiday1)), 
                     rep("FL", nrow(usholiday1))))

holiday<- bind_rows(atholiday, itholiday, esholiday, deholiday, frholiday, gbholiday, usholiday)

# saveRDS(holiday, "./data/raw/holiday/holiday.rds")

### 1.5. Merge  ####

dload<- readRDS("D:/data/ENTSO-E/load/actual/daily/dload_all2.rds")
usdload<- readRDS("D:/data/EIA/usdload.rds")
load<- bind_rows(dload, usdload)

holiday<- readRDS("./data/raw/holiday/holiday.rds")

tempeature<- readRDS("./data/raw/temperature/temperature.rds")


ddata_all<- left_join(load, holiday) %>% left_join(tempeature) %>% mutate(load = load/1000) %>%
  filter(date >= as.Date("2015-01-03"), date < as.Date("2020-08-01")) %>%
  mutate(wday = wday(date)) %>% mutate(sunday = ifelse(wday == 1, 1, 0)) %>% unique()

ddata<- ddata_all %>% filter(country %in% c("AT", "ES", "DE", "IT", "FR", "GB", "NY", "CA", "FL"))

raw_data_plot<- ggplot(ddata, aes(date, load)) + geom_line() +
  facet_wrap(~country, scales = "free") +
  labs(x= element_blank(), y = "Actual load (TWh)") +
  mytheme()

write.png(raw_data_plot, "./figures/appendix/raw_data.png")

# check
ddata %>% filter(date >= as.Date("2020-03-01")) %>%
  ggplot(aes(date, load)) + geom_line() +
  facet_wrap(~country, scales = "free") +
  labs(x= element_blank()) +
  mytheme()

templot<- ggplot(ddata, aes(max_temp, load)) + 
  geom_point(alpha = .1) + geom_smooth() +
  facet_wrap(~country, scales = "free")  +
  labs(x = "Temperature (ºC)", y = "Actual load (TWh)") +
  mytheme()

write.png(templot, "./figures/appendix/load-temp.png")


# saveRDS(ddata, "C:/Users/Prol/Documents/R/wd/covid/data/clean/ddata2.rds")
# saveRDS(ddata_all, "C:/Users/Prol/Documents/R/wd/covid/data/clean/ddata_all2.rds")

proba<- readRDS("./data/clean/ddata_all2.rds")


proba %>% filter(country == "BG")

######################################### 2. Accuracy #########################################

ddata<- readRDS("./data/clean/ddata2.rds") # %>% filter(date <= "2020-04-30") 

mylist<- list()

get_accuracy<- function(where = "ES", k = 25){
  
  # 1. ARIMA
  # define train (4 years 2015-2018) and test (1 year 2019) sets
  train<- ddata %>% filter(country == where, date < "2019-01-01") %>% na.omit()
  test<- ddata %>% filter(country == where, date >= "2019-01-01", date < "2020-01-01") %>% na.omit()
  
  # convert into multiple (weekly and yearly) seasonality time series (msts)
  train_msts<- msts(train$load, seasonal.periods = c(7, 365.25))
  test_msts<- msts(test$load, seasonal.periods = c(7, 365.25))
  
  lambda<- BoxCox.lambda(train_msts)
  
  # fourier terms for multiple seasonality
  harmonic<- fourier(train_msts, K = c(3, k))
  
  # run model
  arima_fit <- auto.arima(train_msts, 
                          xreg = cbind(train$max_temp, train$max_temp^2,
                                       as.factor(train$holiday), harmonic), 
                          seasonal = FALSE, lambda = lambda)
  
  # forecast
  # define controls for de forecast period of 1 year
  test_harmonic<- fourier(train_msts, K = c(3, k), h = 365)
  
  arima_fct<- forecast(arima_fit, xreg = cbind(test$max_temp, test$max_temp^2,
                                               as.factor(test$holiday), test_harmonic))
  # accuracy
  arima_acc<- accuracy(as.vector(arima_fct$mean), as.vector(test_msts))
  
  # 2. NNAR 
  nnar_fit<- nnetar(train_msts, xreg = cbind(as.factor(train$holiday), as.factor(train$wday),
                                           train$max_temp), lambda = lambda)
  nnar_fct<- forecast(nnar_fit, xreg = cbind(as.factor(test$holiday), as.factor(test$wday),
                                            test$max_temp), h = 365)
  nnar_acc<- accuracy(as.vector(nnar_fct$mean), as.vector(test_msts))
  
  
  # 3. TBATS 
  tbats_fit<- tbats(train_msts, lambda = lambda)
  tbats_fct<- forecast(tbats_fit, h = 365)
  tbats_acc<- accuracy(as.vector(tbats_fct$mean), as.vector(test_msts))
  
  
  # 4. STLF 
  stlf_fit<- stlf(train_msts, lambda = lambda)
  stlf_fct<- forecast(stlf_fit, h = 365)
  stlf_acc<- accuracy(as.vector(stlf_fct$mean), as.vector(test_msts))
  
  
  # 6. Comparison 
  
  acc_comparison<- data.frame(arima = round(as.vector(arima_acc), 2),
                              nnar = round(as.vector(nnar_acc), 2),
                              tbats = round(as.vector(tbats_acc), 2),
                              stlf = round(as.vector(stlf_acc), 2))
  rownames(acc_comparison)<- c("ME", "RMSE", "MAE", "MPE", "MAPE")
  
  
  mylist<<- list(arima = arima_fct, 
                 nnar = nnar_fct, 
                 tbats = tbats_fct, 
                 stlf = stlf_fct,
                 accuracy = acc_comparison)
  
  saveRDS(mylist, paste("./data/results/accuracy/accuracy_", where, ".rds", sep = ""))
  
  return(mylist$accuracy)
}

get_accuracy(where = "AT", k = 20)
get_accuracy(where = "CA", k = 3)
get_accuracy(where = "DE", k = 11)
get_accuracy(where = "ES", k = 23)
get_accuracy(where = "FL", k = 3)
get_accuracy(where = "FR", k = 19)
get_accuracy(where = "GB", k = 3)
get_accuracy(where = "IT", k = 20)
get_accuracy(where = "NY", k = 5)
get_accuracy(where = "AT", k = 20)



# summary accuracy plot
df<- data.frame()
for(c in (c("DE", "ES", "IT", "FR", "AT", "GB", "CA", "NY", "FL"))){
  df<- readRDS(paste("./data/results/accuracy/accuracy_", c, ".rds", sep = ""))[["accuracy"]]
  acc_sum<- acc_sum %>% bind_rows(df[5, ])
}

acc_sum<- gather(acc_sum, model, value)

ggplot(acc_sum, aes(x = model, y = value)) + geom_jitter(width = 0.1, alpha = .3) 

######################################### 3. Forecast #########################################


ddata<- readRDS("./data/clean/ddata2.rds") # %>% filter(date <= "2020-04-30")
mylist<- list()

# check data
ddata %>% filter(country == "CA", date > "2020-01-01") %>%
  ggplot(aes(date, load)) + geom_line()

# simplified auto.arima procedure, when k = NULL iteration to find optimal, takes a long time
get_results<- function(where = "ES", k = NULL){ # if k is set no automatic selection (which takes long time)
  
  # define train (4 years 2015-2018) and test (1 year 2019) sets
  train<- ddata %>% filter(country == where, date < "2020-03-01") %>% na.omit()
  test<- ddata %>% filter(country == where, date >= "2020-03-01") %>% na.omit()
  
  # convert into multiple (weekly and yearly) seasonality time series (msts)
  train_msts<- msts(train$load, seasonal.periods = c(7, 365.25))
  test_msts<- msts(test$load, seasonal.periods = c(7, 365.25)) %>% na.omit()
  
  lambda<- BoxCox.lambda(train_msts)
  
  optimize_k<- function(){
    # if k not set then automatic selection with loop
    df<- data.frame(k = c(1:30), AICC = rep(NA, 30))
    for (k in c(1:30)) {
      harmonic<- fourier(train_msts, K = c(3, k))
      
      fit <- auto.arima(train_msts, # training data
                        xreg = cbind(train$max_temp, train$max_temp^2,
                                     as.factor(train$holiday), harmonic), # covariates
                        seasonal = FALSE, lambda = lambda)
      
      df[k,"AICC"] <- round(fit[["aicc"]],2)
    }
    k<- df[which.min(df$AICC), "k"]
  }
  
  # get optimal k for the yearly fourier term
  k<- ifelse(is.null(k), optimize_k(), k)
  
  # calculate fourier term
  harmonic<- fourier(train_msts, K = c(3, k))
  
  # run model
  fit <- auto.arima(train_msts, 
                          xreg = cbind(train$max_temp, train$max_temp^2,
                                       as.factor(train$holiday), harmonic), 
                          seasonal = FALSE, lambda = lambda)
  
  # forecast fourier term and other control variables
  test_harmonic<- fourier(train_msts, K = c(3, k), h = length(test_msts))
  
  # forecast
  fct<- forecast(fit, xreg = cbind(test$max_temp, test$max_temp^2,
                                               as.factor(test$holiday), test_harmonic))
  
  result<- data.frame(date = test$date,
             actual = test$load,
             fct = as.numeric(fct$mean),
             low80 = as.numeric(fct$lower[, 1]),
             low95 = as.numeric(fct$lower[, 2]),
             up80 = as.numeric(fct$upper[, 1]),
             up95 = as.numeric(fct$upper[, 2])) %>%
    mutate(dif = (100*(actual-fct))/actual,
           diflow80 = (100*(actual-low80))/actual,
           diflow95 = (100*(actual-low95))/actual,
           difup80 = (100*(actual-up80))/actual,
           difup95 = (100*(actual-up95))/actual,
           country = where) %>% as_tibble()
  
  mylist<<- list(fit = fit, 
                forecast = fct, 
                k = k,
                lambda = lambda,
                train = train, 
                test = test,
                result = result)
  
  saveRDS(mylist, paste("./data/results/forecast/results_", where, ".rds", sep = ""))
  
  return(round(c(k, lambda),2))
}
# the output is a list with the following objects
# [1] model (fit) [2] forecast [3] k [4] train data [5] test data [6] results df

# slow auto.arima procedure, more likely to find optimal order (about 6 hours)
get_results2<- function(where = "ES", k = NULL){ # if k is set no automatic selection (which takes long time)
  
  # define train (4 years 2015-2018) and test (1 year 2019) sets
  train<- ddata %>% filter(country == where, date < "2020-03-01") %>% na.omit()
  test<- ddata %>% filter(country == where, date >= "2020-03-01") %>% na.omit()
  
  # convert into multiple (weekly and yearly) seasonality time series (msts)
  train_msts<- msts(train$load, seasonal.periods = c(7, 365.25))
  test_msts<- msts(test$load, seasonal.periods = c(7, 365.25)) %>% na.omit()
  
  lambda<- BoxCox.lambda(train_msts)
  
  optimize_k<- function(){
    # if k not set then automatic selection with loop
    df<- data.frame(k = c(1:30), AICC = rep(NA, 30))
    for (k in c(1:30)) {
      harmonic<- fourier(train_msts, K = c(3, k))
      
      fit <- auto.arima(train_msts, # training data
                        xreg = cbind(train$max_temp, train$max_temp^2,
                                     as.factor(train$holiday), harmonic), # covariates
                        seasonal = FALSE, lambda = lambda, approximation = FALSE, stepwise = FALSE)
      
      df[k,"AICC"] <- round(fit[["aicc"]],2)
    }
    k<- df[which.min(df$AICC), "k"]
  }
  
  # get optimal k for the yearly fourier term
  k<- ifelse(is.null(k), optimize_k(), k)
  
  # calculate fourier term
  harmonic<- fourier(train_msts, K = c(3, k))
  
  # run model
  fit <- auto.arima(train_msts, 
                    xreg = cbind(train$max_temp, train$max_temp^2,
                                 as.factor(train$holiday), harmonic), 
                    seasonal = FALSE, lambda = lambda, approximation = FALSE, stepwise = FALSE)
  
  # forecast fourier term and other control variables
  test_harmonic<- fourier(train_msts, K = c(3, k), h = length(test_msts))
  
  # forecast
  fct<- forecast(fit, xreg = cbind(test$max_temp, test$max_temp^2,
                                   as.factor(test$holiday), test_harmonic))
  
  result<- data.frame(date = test$date,
                      actual = test$load,
                      fct = as.numeric(fct$mean),
                      low80 = as.numeric(fct$lower[, 1]),
                      low95 = as.numeric(fct$lower[, 2]),
                      up80 = as.numeric(fct$upper[, 1]),
                      up95 = as.numeric(fct$upper[, 2])) %>%
    mutate(dif = (100*(actual-fct))/actual,
           diflow80 = (100*(actual-low80))/actual,
           diflow95 = (100*(actual-low95))/actual,
           difup80 = (100*(actual-up80))/actual,
           difup95 = (100*(actual-up95))/actual,
           country = where) %>% as_tibble()
  
  mylist<<- list(fit = fit, 
                 forecast = fct, 
                 k = k, 
                 lambda = lambda,
                 train = train, 
                 test = test,
                 result = result)
  
  saveRDS(mylist, paste("./data/results/forecast2/results_", where, ".rds", sep = ""))
  
  return(mylist$k)
}

# get trunctated results (set max. order)
get_trunc_results<- function(where = "ES", k = NULL, p = 1, d = 1, q = 1){ # if k is set no automatic selection (which takes long time)
  
  # define train (4 years 2015-2018) and test (1 year 2019) sets
  train<- ddata %>% filter(country == where, date < "2020-03-01") %>% na.omit()
  test<- ddata %>% filter(country == where, date >= "2020-03-01") %>% na.omit()
  
  # convert into multiple (weekly and yearly) seasonality time series (msts)
  train_msts<- msts(train$load, seasonal.periods = c(7, 365.25))
  test_msts<- msts(test$load, seasonal.periods = c(7, 365.25)) %>% na.omit()
  
  lambda<- BoxCox.lambda(train_msts)
  
  optimize_k<- function(){
    # if k not set then automatic selection with loop
    df<- data.frame(k = c(1:30), AICC = rep(NA, 30))
    for (k in c(1:30)) {
      harmonic<- fourier(train_msts, K = c(3, k))
      
      fit <- auto.arima(train_msts, # training data
                        xreg = cbind(train$max_temp, train$max_temp^2,
                                     as.factor(train$holiday), harmonic), # covariates
                        seasonal = FALSE, lambda = lambda, approximation = FALSE, stepwise = FALSE,
                        max.p = p, max.d = d, max.q = q)
      
      df[k,"AICC"] <- round(fit[["aicc"]],2)
    }
    k<- df[which.min(df$AICC), "k"]
  }
  
  # get optimal k for the yearly fourier term
  k<- ifelse(is.null(k), optimize_k(), k)
  
  # calculate fourier term
  harmonic<- fourier(train_msts, K = c(3, k))
  
  # run model
  fit <- auto.arima(train_msts, 
                    xreg = cbind(train$max_temp, train$max_temp^2,
                                 as.factor(train$holiday), harmonic), 
                    seasonal = FALSE, lambda = lambda, approximation = FALSE, stepwise = FALSE,
                    max.p = p, max.d = d, max.q = q)
  
  # forecast fourier term and other control variables
  test_harmonic<- fourier(train_msts, K = c(3, k), h = length(test_msts))
  
  # forecast
  fct<- forecast(fit, xreg = cbind(test$max_temp, test$max_temp^2,
                                   as.factor(test$holiday), test_harmonic))
  
  result<- data.frame(date = test$date,
                      actual = test$load,
                      fct = as.numeric(fct$mean),
                      low80 = as.numeric(fct$lower[, 1]),
                      low95 = as.numeric(fct$lower[, 2]),
                      up80 = as.numeric(fct$upper[, 1]),
                      up95 = as.numeric(fct$upper[, 2])) %>%
    mutate(dif = (100*(actual-fct))/actual,
           diflow80 = (100*(actual-low80))/actual,
           diflow95 = (100*(actual-low95))/actual,
           difup80 = (100*(actual-up80))/actual,
           difup95 = (100*(actual-up95))/actual,
           country = where) %>% as_tibble()
  
  mylist<<- list(fit = fit, 
                 forecast = fct, 
                 k = k,
                 lambda = lambda,
                 train = train, 
                 test = test,
                 result = result)
  
  saveRDS(mylist, paste("./data/results/forecast/results_", where, ".rds", sep = ""))
  
  return(round(c(k, lambda),2))
}

# manually select arima order
get_arima_results<- function(where = "ES", k = NULL, j = 3, p = 1, d = 1, q = 1){ # if k is set no automatic selection (which takes long time)
  
  # define train (4 years 2015-2018) and test (1 year 2019) sets
  train<- ddata %>% filter(country == where, date < "2020-03-01") %>% na.omit()
  test<- ddata %>% filter(country == where, date >= "2020-03-01") %>% na.omit()
  
  # convert into multiple (weekly and yearly) seasonality time series (msts)
  train_msts<- msts(train$load, seasonal.periods = c(7, 365.25))
  test_msts<- msts(test$load, seasonal.periods = c(7, 365.25)) %>% na.omit()
  
  lambda<- BoxCox.lambda(train_msts)
  
  # calculate fourier term
  harmonic<- fourier(train_msts, K = c(j, k))
  
  # run model
  fit <- Arima(train_msts, 
                    xreg = cbind(train$max_temp, train$max_temp^2,
                                 as.factor(train$holiday), harmonic),
               lambda = lambda, order = c(p,d,q))
  
  # forecast fourier term and other control variables
  test_harmonic<- fourier(train_msts, K = c(j, k), h = length(test_msts))
  
  # forecast
  fct<- forecast(fit, xreg = cbind(test$max_temp, test$max_temp^2,
                                   as.factor(test$holiday), test_harmonic))
  
  result<- data.frame(date = test$date,
                      actual = test$load,
                      fct = as.numeric(fct$mean),
                      low80 = as.numeric(fct$lower[, 1]),
                      low95 = as.numeric(fct$lower[, 2]),
                      up80 = as.numeric(fct$upper[, 1]),
                      up95 = as.numeric(fct$upper[, 2])) %>%
    mutate(dif = (100*(actual-fct))/actual,
           diflow80 = (100*(actual-low80))/actual,
           diflow95 = (100*(actual-low95))/actual,
           difup80 = (100*(actual-up80))/actual,
           difup95 = (100*(actual-up95))/actual,
           country = where) %>% as_tibble()
  
  mylist<<- list(fit = fit, 
                 forecast = fct, 
                 k = k,
                 lambda = lambda,
                 train = train, 
                 test = test,
                 result = result)
  
  saveRDS(mylist, paste("./data/results/forecast_manual/results_", where, ".rds", sep = ""))
  
  return(round(c(k, lambda),2))
}

get_nnar_results<- function(where = "ES"){
  train<- ddata %>% filter(country == where, date < "2020-03-01") %>% na.omit()
  test<- ddata %>% filter(country == where, date >= "2020-03-01") %>% na.omit()
  
  # convert into multiple (weekly and yearly) seasonality time series (msts)
  train_msts<- msts(train$load, seasonal.periods = 7)
  test_msts<- msts(test$load, seasonal.periods = 7) %>% na.omit()
  
  lambda<- BoxCox.lambda(train_msts)
  
  # neural network
  fit<- nnetar(train_msts, 
                    xreg = cbind(as.factor(train$holiday), as.factor(train$wday),
                                 as.factor(train$month), train$max_temp),
                    lambda = lambda)
  fct<- forecast(fit, 
                      xreg =cbind(as.factor(test$holiday), as.factor(test$wday),
                                  as.factor(test$month), test$max_temp),
                      PI =FALSE)
  
  result<- data.frame(date = test$date,
                      actual = test$load,
                      fct = as.numeric(fct$mean)) %>%
    mutate(dif = (100*(actual-fct))/actual) %>% as_tibble()
  
  mylist<<- list(fit = fit, 
                 forecast = fct, 
                 lambda = lambda,
                 train = train, 
                 test = test,
                 result = result)
  
  saveRDS(mylist, paste("./data/results/forecast_nnar/results_", where, ".rds", sep = ""))

}



get_arima_results(where = "AT", k = 27, p = 1, d = 1, q = 5)

get_results(where = "AT", k = 20) # ((5,1,1)) 0,1,5 lambda .15

get_results2(where = "CA", k = 30) # (3,1,2) lambda 1.1

get_results2(where = "DE", k = 30) # (4,1,1) lambda .81

get_results2(where = "ES", k = 28) # (4,1,1) lambda -.06

get_results2(where = "FL", k = 3) # (1,1,2) lambda  2

get_results(where = "FR", k = 19) # (0,1,4) lambda -1

get_results2(where = "GB", k = 19) # (2,1,3) lambda 1.2

get_arima_results(where = "IT", k = 21, p = 3, d = 1, q = 2)
get_results(where = "IT", k = 21) # (5,1,0) lambda .87

get_results2(where = "NY", k = 5) # (3,3,1) lambda -1


######################################### 4. Results #########################################
get_arima_results<- function(where = "ES", k = NULL, j = 3, p = 1, d = 1, q = 1){ # if k is set no automatic selection (which takes long time)
  
  # define train (4 years 2015-2018) and test (1 year 2019) sets
  train<- ddata %>% filter(country == where, date < "2020-03-01") %>% na.omit()
  test<- ddata %>% filter(country == where, date >= "2020-03-01") %>% na.omit()
  
  # convert into multiple (weekly and yearly) seasonality time series (msts)
  train_msts<- msts(train$load, seasonal.periods = c(7, 365.25))
  test_msts<- msts(test$load, seasonal.periods = c(7, 365.25)) %>% na.omit()
  
  lambda<- BoxCox.lambda(train_msts)
  
  # calculate fourier term
  harmonic<- fourier(train_msts, K = c(j, k))
  
  # run model
  fit <- Arima(train_msts, 
               xreg = cbind(train$max_temp, train$max_temp^2,
                            as.factor(train$holiday), harmonic),
               lambda = lambda, order = c(p,d,q))
  
  # forecast fourier term and other control variables
  test_harmonic<- fourier(train_msts, K = c(j, k), h = length(test_msts))
  
  # forecast
  fct<- forecast(fit, xreg = cbind(test$max_temp, test$max_temp^2,
                                   as.factor(test$holiday), test_harmonic))
  
  result<- data.frame(date = test$date,
                      actual = test$load,
                      fct = as.numeric(fct$mean),
                      low80 = as.numeric(fct$lower[, 1]),
                      low95 = as.numeric(fct$lower[, 2]),
                      up80 = as.numeric(fct$upper[, 1]),
                      up95 = as.numeric(fct$upper[, 2])) %>%
    mutate(dif = (100*(actual-fct))/actual,
           diflow80 = (100*(actual-low80))/actual,
           diflow95 = (100*(actual-low95))/actual,
           difup80 = (100*(actual-up80))/actual,
           difup95 = (100*(actual-up95))/actual,
           country = where) %>% as_tibble()
  
  mylist<<- list(fit = fit, 
                 forecast = fct, 
                 k = k,
                 lambda = lambda,
                 train = train, 
                 test = test,
                 result = result)
  
  saveRDS(mylist, paste("./data/results/forecast_manual/results_", where, ".rds", sep = ""))
  
  return(round(c(k, lambda),2))
}
ddata<- readRDS("./data/clean/ddata2.rds") %>% unique()  %>% filter(date < "2020-08-01")
write.csv(ddata, "./data/clean/ddata2.csv")

mylist<- list()
viridis_col<- c("#440154FF", "#472D7BFF", "#3B528BFF", "#2C728EFF", "#21908CFF",
                "#27AD81FF", "#5DC863FF", "#AADC32FF", "#FDE725FF")

############# 4.1 AT ############
# arima
get_arima_results(where = "AT", j = 3, k = 20, p = 0, d = 1, q = 4)
atlist<- readRDS("./data/results/forecast_manual/results_AT.rds")

at_fit<- atlist$fit
checkresiduals(at_fit)
coeftest(at_fit)

at_result<- atlist$result %>% mutate(wday = weekdays(date))
at_result$lockdown<- as.Date("2020-03-16")

sunday<- at_result %>% mutate(wday = weekdays(date)) %>% filter(wday == "Sunday")
atplot<- ggplot(at_result, aes(date, dif)) +
  geom_ribbon(aes(date, ymin = diflow80, ymax = difup80), fill = viridis_col[7], alpha = .2) +
  geom_ribbon(aes(date, ymin = diflow95, ymax = difup95), fill = viridis_col[7], alpha = .15) +
  geom_line(color = viridis_col[7]) + geom_point(color = viridis_col[7]) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-03-16"), linetype = "dotted") +
  annotate("text", x = as.Date("2020-03-16"), y = -25, label = "(1)") +
  # annotate("text", x = as.Date("2020-03-02"), y = -33, label = "-8%", color = "#21908CFF", hjust = 0) +
  #•annotate("text", x = as.Date("2020-03-02"), y = -33, label = "2.9*", hjust = 0) +
  geom_point(data = sunday, aes(date, dif), color = "darkgrey", size = 1.5) +
  coord_cartesian(ylim = c(-25, 20)) +
  labs(x = element_blank(), y = element_blank(), title = "Austria") + 
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

write.png(atplot, "./figures/at.png")

atplot2<- ggplot(at_result, aes(date, fct)) +
  geom_line(size = .5) +
  geom_ribbon(aes(date, ymin = low80, ymax = up80), alpha = .12) +
  geom_ribbon(aes(date, ymin = low95, ymax = up95), alpha = .08) +
  geom_line(aes(date, actual),  color = viridis_col[7], size = 1) +
  geom_vline(xintercept = as.Date("2020-03-16"), linetype = "dotted") +
  annotate("text", x = as.Date("2020-03-16"), y = .4, label = "(1)") +
  labs(x = element_blank(), y = element_blank(), title = "Austria") +
  coord_cartesian(ylim= c(.4, .8)) +
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

write.png(atplot2, "./figures/at2.png")

atplot3<- ggplot(at_result, aes(date, fct)) +
  geom_line() +
  geom_line(aes(date, actual),  color = viridis_col[6], size = 1) +
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

ggplotly(atplot3)

############# 4.2 CA ############
# arima
get_arima_results(where = "CA", j = 3, k = 3, p = 4, d = 1, q = 3)
calist<- readRDS("./data/results/forecast_manual/results_CA.rds")


ca_fit<- calist$fit
checkresiduals(ca_fit)
coeftest(ca_fit)

ca_result<- calist$result
ca_result$lockdown<- as.Date("2020-03-19")

sunday<- ca_result %>% mutate(wday = weekdays(date)) %>% filter(wday == "Sunday")
caplot<- ggplot(ca_result, aes(date, dif)) +
  geom_ribbon(aes(date, ymin = diflow80, ymax = difup80), fill = viridis_col[6], alpha = .2) +
  geom_ribbon(aes(date, ymin = diflow95, ymax = difup95), fill = viridis_col[6], alpha = .15) +
  geom_line(color = viridis_col[6]) + geom_point(color = viridis_col[6]) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-03-19"), linetype = "dotted") +
  annotate("text", x = as.Date("2020-03-19"), y = -35, label = "(1)") +
  #annotate("text", x = as.Date("2020-03-02"), y = -23, label = "1.8*", hjust = 0) +
  geom_point(data = sunday, aes(date, dif), color = "darkgrey", size = 1.5) +
  coord_cartesian(ylim = c(-35, 10)) +
  labs(x = element_blank(), y = element_blank(), title = "California") + 
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

write.png(caplot, "./figures/ca.png")

caplot2<- ggplot(ca_result, aes(date, fct)) +
  geom_line(size = .5) +
  geom_ribbon(aes(date, ymin = low80, ymax = up80), alpha = .12) +
  geom_ribbon(aes(date, ymin = low95, ymax = up95), alpha = .08) +
  geom_line(aes(date, actual),  color = viridis_col[6], size = 1) +
  geom_vline(xintercept = as.Date("2020-03-19"), linetype = "dotted") +
  annotate("text", x = as.Date("2020-03-19"), y = .5, label = "(1)") +
  labs(x = element_blank(), y = element_blank(), title = "California") +
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

write.png(caplot2, "./figures/ca2.png")

# check fitted
check<- data.frame(fitted = ca_fit$fitted,
                   act = calist$train$load,
                   date = calist$train$date) %>% gather(key, value, -date)

check %>% filter(date >= "2020-01-01") %>%
  ggplot(aes(date, value, color = key)) + geom_line() + geom_point()



############# 4.3 DE ############
get_arima_results(where = "DE", j = 3, k = 11, p = 4, d = 1, q = 1) # get_results2(where = "DE")
delist<- readRDS("./data/results/forecast_manual/results_DE.rds")

de_fit<- delist$fit
checkresiduals(de_fit)
coeftest(de_fit)

de_result<- delist$result
de_result$lockdown<- as.Date("2020-03-22")

sunday<- de_result %>% mutate(wday = weekdays(date)) %>% filter(wday == "Sunday")
deplot<- ggplot(de_result, aes(date, dif)) +
  geom_ribbon(aes(date, ymin = diflow80, ymax = difup80), fill = viridis_col[5], alpha = .2) +
  geom_ribbon(aes(date, ymin = diflow95, ymax = difup95), fill = viridis_col[5], alpha = .15) +
  geom_line(color =viridis_col[5]) + geom_point(color = viridis_col[5]) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-03-22"), linetype = "dotted") +
  annotate("text", x = as.Date("2020-03-22"), y = -35, label = "(1)") +
  #annotate("text", x = as.Date("2020-03-02"), y = -23, label = "1.8*", hjust = 0) +
  geom_point(data = sunday, aes(date, dif), color = "darkgrey", size = 1.5) +
  coord_cartesian(ylim = c(-35, 10)) +
  labs(x = element_blank(), y = element_blank(), title = "Germany") + 
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

write.png(deplot, "./figures/de.png")

deplot2<- ggplot(de_result, aes(date, fct)) +
  geom_line(size = .5) +
  geom_ribbon(aes(date, ymin = low80, ymax = up80), alpha = .12) +
  geom_ribbon(aes(date, ymin = low95, ymax = up95), alpha = .08) +
  geom_line(aes(date, actual),  color = viridis_col[5], size = 1) +
  geom_vline(xintercept = as.Date("2020-03-22"), linetype = "dotted") +
  annotate("text", x = as.Date("2020-03-22"), y = 3.6, label = "(1)") +
  labs(x = element_blank(), y = element_blank(), title = "Germany") +
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

write.png(deplot2, "./figures/de2.png")

############# 4.4 ES ############
get_arima_results(where = "ES", j=3, k = 23, p = 3, d = 1, q = 2)
eslist<- readRDS("./data/results/forecast_manual/results_ES.rds")

es_fit<- eslist$fit
checkresiduals(es_fit)
coeftest(es_fit)

es_result<- eslist$result
es_result$lockdown<- as.Date("2020-03-15")


sunday<- es_result %>% mutate(wday = weekdays(date)) %>% filter(wday == "Sunday")
esplot<- ggplot(es_result, aes(date, dif)) +
  geom_ribbon(aes(date, ymin = diflow80, ymax = difup80), fill = viridis_col[4], alpha = .2) +
  geom_ribbon(aes(date, ymin = diflow95, ymax = difup95), fill = viridis_col[4], alpha = .15) +
  geom_line(color = viridis_col[4]) + geom_point(color = viridis_col[4]) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-03-15"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-03-30"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-04-14"), linetype = "dotted") +
  annotate("text", x = as.Date("2020-03-15"), y = -35, label = "(1)") +
  annotate("text", x = as.Date("2020-03-30"), y = -35, label = "(2)") +
  annotate("text", x = as.Date("2020-04-14"), y = -35, label = "(3)") +
  #annotate("text", x = as.Date("2020-03-02"), y = -43, label = "1.63*", hjust = 0) +
  geom_point(data = sunday, aes(date, dif), color = "darkgrey", size = 1.5) +
  coord_cartesian(ylim = c(-35, 10)) +
  labs(x = element_blank(), y = element_blank(), title = "Spain") + 
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

write.png(esplot, "./figures/es.png")

esplot2<- ggplot(es_result, aes(date, fct)) +
  geom_line(size = .5) +
  geom_ribbon(aes(date, ymin = low80, ymax = up80), alpha = .12) +
  geom_ribbon(aes(date, ymin = low95, ymax = up95), alpha = .08) +
  geom_line(aes(date, actual),  color = viridis_col[4], size = 1) +
  geom_vline(xintercept = as.Date("2020-03-15"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-03-30"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-04-14"), linetype = "dotted") +
  annotate("text", x = as.Date("2020-03-15"), y = .45, label = "(1)") +
  annotate("text", x = as.Date("2020-03-30"), y = .45, label = "(2)") +
  annotate("text", x = as.Date("2020-04-14"), y = .45, label = "(3)") +
  labs(x = element_blank(), y = element_blank(), title = "Spain") +
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

write.png(esplot2, "./figures/es2.png")


############# 4.5 FL ############
get_arima_results(where = "FL", j = 3, k = 3, p = 1, d = 1, q = 2) # get_results2(where = "FL") 
fllist<- readRDS("./data/results/forecast_manual/results_FL.rds")

fl_fit<- fllist$fit
checkresiduals(fl_fit)
coeftest(fl_fit)

fl_result<- fllist$result
fl_result$lockdown<- as.Date("2020-04-03")

sunday<- fl_result %>% mutate(wday = weekdays(date)) %>% filter(wday == "Sunday")
flplot<- ggplot(fl_result, aes(date, dif)) +
  geom_ribbon(aes(date, ymin = diflow80, ymax = difup80), fill = viridis_col[9], alpha = .25) +
  geom_ribbon(aes(date, ymin = diflow95, ymax = difup95), fill = viridis_col[9], alpha = .2) +
  geom_line(color = viridis_col[9], size = 1) + geom_point(color = viridis_col[9], size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-04-03"), linetype = "dotted") +
  annotate("text", x = as.Date("2020-04-03"), y = -25, label = "(1)") +
  #annotate("text", x = as.Date("2020-03-02"), y = -23, label = "2.71*", hjust = 0) +
  geom_point(data = sunday, aes(date, dif), color = "darkgrey", size = 1.5) +
  coord_cartesian(ylim = c(-25, 20)) +
  labs(x = element_blank(), y = element_blank(), title = "Florida") + 
  mytheme()+ theme(plot.title = element_text(hjust = 0.5))

write.png(flplot, "./figures/fl.png")

flplot2<- ggplot(fl_result, aes(date, fct)) +
  geom_line(size = .5) +
  geom_ribbon(aes(date, ymin = low80, ymax = up80), alpha = .12) +
  geom_ribbon(aes(date, ymin = low95, ymax = up95), alpha = .08) +
  geom_line(aes(date, actual),  color = viridis_col[9], size = 1) +
  geom_vline(xintercept = as.Date("2020-04-03"), linetype = "dotted") +
  annotate("text", x = as.Date("2020-04-03"), y = .45, label = "(1)") +
  labs(x = element_blank(), y = element_blank(), title = "Florida") +
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

write.png(flplot2, "./figures/fl2.png")

############# 4.6 FR ############
get_arima_results(where = "FR", j = 3, k = 19, p = 7, d = 1, q = 6)
frlist<- readRDS("./data/results/forecast_manual/results_FR.rds")


fr_fit<- frlist$fit
checkresiduals(fr_fit)
coeftest(fr_fit)

fr_result<- frlist$result
fr_result$lockdown<- as.Date("2020-03-17")

sunday<- fr_result %>% mutate(wday = weekdays(date)) %>% filter(wday == "Sunday")
frplot<- ggplot(fr_result, aes(date, dif)) +
  geom_ribbon(aes(date, ymin = diflow80, ymax = difup80), fill = viridis_col[3], alpha = .2) +
  geom_ribbon(aes(date, ymin = diflow95, ymax = difup95), fill = viridis_col[3], alpha = .15) +
  geom_line(color = viridis_col[3]) + geom_point(color = viridis_col[3]) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-03-17"), linetype = "dotted") +
  annotate("text", x = as.Date("2020-03-17"), y = -45, label = "(1)") +
  #annotate("text", x = as.Date("2020-03-02"), y = -43, label = "2.29*", hjust = 0) +
  geom_point(data = sunday, aes(date, dif), color = "darkgrey", size = 1.5) +
  coord_cartesian(ylim = c(-45, 10)) +
  labs(x = element_blank(), y = element_blank(), title = "France") + 
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

write.png(frplot, "./figures/fr.png")

frplot2<- ggplot(fr_result, aes(date, fct)) +
  geom_line(size = .5) +
  geom_ribbon(aes(date, ymin = low80, ymax = up80), alpha = .12) +
  geom_ribbon(aes(date, ymin = low95, ymax = up95), alpha = .08) +
  geom_line(aes(date, actual),  color = viridis_col[3], size = 1) +
  geom_vline(xintercept = as.Date("2020-03-17"), linetype = "dotted") +
  labs(x = element_blank(), y = element_blank(), title = "France") +
  annotate("text", x = as.Date("2020-03-17"), y = .9, label = "(1)") +
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

write.png(frplot2, "./figures/fr2.png")


############# 4.7 GB ############
get_arima_results(where = "GB",j = 3, k = 3, p = 2, d = 1, q = 1) # get_results2(where = "UK")
gblist<- readRDS("./data/results/forecast_manual/results_GB.rds")

gb_fit<- gblist$fit
checkresiduals(gb_fit)
coeftest(gb_fit)

gb_result<- gblist$result
gb_result$lockdown<- as.Date("2020-03-23")

sunday<- gb_result %>% mutate(wday = weekdays(date)) %>% filter(wday == "Sunday")
gbplot<- ggplot(gb_result, aes(date, dif)) +
  geom_ribbon(aes(date, ymin = diflow80, ymax = difup80), fill = viridis_col[1], alpha = .2) +
  geom_ribbon(aes(date, ymin = diflow95, ymax = difup95), fill = viridis_col[1], alpha = .15) +
  geom_line(color = viridis_col[1]) + geom_point(color = viridis_col[1]) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dotted") +
  annotate("text", x = as.Date("2020-03-23"), y = -45, label = "(1)") +
  #annotate("text", x = as.Date("2020-03-02"), y = -33, label = "2.05*", hjust = 0) +
  geom_point(data = sunday, aes(date, dif), color = "darkgrey", size = 1.5) +
  coord_cartesian(ylim = c(-45, 10)) +
  labs(x = element_blank(), y = element_blank(), title = "Great Britain") + 
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

write.png(gbplot, "./figures/gb.png")

gbplot2<- ggplot(gb_result, aes(date, fct)) +
  geom_line(size = .5) +
  geom_ribbon(aes(date, ymin = low80, ymax = up80), alpha = .12) +
  geom_ribbon(aes(date, ymin = low95, ymax = up95), alpha = .08) +
  geom_line(aes(date, actual),  color = viridis_col[1], size = 1) +
  geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dotted") +
  labs(x = element_blank(), y = element_blank(), title = "Great Britain") +
  annotate("text", x = as.Date("2020-03-23"), y = 1.1, label = "(1)") +
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

write.png(gbplot2, "./figures/gb2.png")

############# 4.8 IT ############
get_arima_results(where = "IT", j = 3, k = 20, p = 3, d = 1, q = 1) 
itlist<- readRDS("./data/results/forecast_manual/results_IT.rds")

it_fit<- itlist$fit
checkresiduals(it_fit)
coeftest(it_fit)

it_result<- itlist$result
it_result$lockdown<-  as.Date("2020-03-10")

sunday<- it_result %>% mutate(wday = weekdays(date)) %>% filter(wday == "Sunday")
itplot<- ggplot(it_result, aes(date, dif)) +
  geom_ribbon(aes(date, ymin = diflow80, ymax = difup80), alpha = .1, fill = viridis_col[2]) +
  geom_ribbon(aes(date, ymin = diflow95, ymax = difup95), alpha = .05, fill = viridis_col[2]) +
  geom_line(color = viridis_col[2]) + geom_point(color = viridis_col[2]) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-03-10"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-03-21"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-04-14"), linetype = "dotted") +
  annotate("text", x = as.Date("2020-03-10"), y = -45, label = "(1)") +
  annotate("text", x = as.Date("2020-03-21"), y = -45,label = "(2)") +
  annotate("text", x = as.Date("2020-04-14"), y = -45, label = "(3)") +
  #annotate("text", x = as.Date("2020-03-02"), y = -43, label = "2.33*", hjust = 0) +
  geom_segment(aes(x = as.Date("2020-05-17"), y = -13, xend = as.Date("2020-05-17"),
                   yend = -30), color = "darkgrey") +
  annotate("text", x = as.Date("2020-05-17"), y = -32, label = "Sunday",  color = "darkgrey") +
  geom_point(data = sunday, aes(date, dif), color = "darkgrey", size = 1.5) +
  coord_cartesian(ylim = c(-45, 10)) +
  labs(x = element_blank(), y = element_blank(), title = "Italy") + 
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

write.png(itplot, "./figures/it.png")

itplot2<- ggplot(it_result, aes(date, fct)) +
  geom_line(size = .5) +
  geom_ribbon(aes(date, ymin = low80, ymax = up80), alpha = .12) +
  geom_ribbon(aes(date, ymin = low95, ymax = up95), alpha = .08) +
  geom_line(aes(date, actual),  color = viridis_col[2], size = 1) +
  geom_vline(xintercept = as.Date("2020-03-10"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-03-21"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2020-04-14"), linetype = "dotted") +
  annotate("text", x = as.Date("2020-03-10"), y = .41, label = "(1)") +
  annotate("text", x = as.Date("2020-03-21"), y = .41,label = "(2)") +
  annotate("text", x = as.Date("2020-04-14"), y = .41, label = "(3)") +
  labs(x = element_blank(), y = element_blank(), title = "Italy") +
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

write.png(itplot2, "./figures/it2.png")

############# 4.9 NY ############
get_arima_results(where = "NY", j = 3, k = 5, p = 3, d = 1, q = 1) 
nylist<- readRDS("./data/results/forecast_manual/results_NY.rds")


ny_fit<- nylist$fit
checkresiduals(ny_fit)
coeftest(ny_fit)

ny_result<- nylist$result
ny_result$lockdown<- as.Date("2020-03-22")

sunday<- ny_result %>% mutate(wday = weekdays(date)) %>% filter(wday == "Sunday")
nyplot<- ggplot(ny_result, aes(date, dif)) +
  geom_ribbon(aes(date, ymin = diflow80, ymax = difup80), fill = viridis_col[8], alpha = .25) +
  geom_ribbon(aes(date, ymin = diflow95, ymax = difup95), fill = viridis_col[8], alpha = .2) +
  geom_line(color = viridis_col[8]) + geom_point(color = viridis_col[8]) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-03-22"), linetype = "dotted") +
  annotate("text", x = as.Date("2020-03-22"), y = -25, label = "(1)") +
  #annotate("text", x = as.Date("2020-03-02"), y = -32, label = "1.63*", hjust = 0) +
  geom_point(data = sunday, aes(date, dif), color = "darkgrey", size = 1.5) +
  coord_cartesian(ylim = c(-25, 20)) +
  labs(x = element_blank(), y = element_blank(), title = "New York") + 
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

write.png(nyplot, "./figures/ny.png")

nyplot2<- ggplot(ny_result, aes(date, fct)) +
  geom_line(size = .5) +
  geom_ribbon(aes(date, ymin = low80, ymax = up80), alpha = .12) +
  geom_ribbon(aes(date, ymin = low95, ymax = up95), alpha = .08) +
  geom_line(aes(date, actual),  color = viridis_col[8], size = 1) +
  geom_vline(xintercept = as.Date("2020-03-22"), linetype = "dotted") +
  labs(x = element_blank(), y = element_blank(), title = "New York") +
  annotate("text", x = as.Date("2020-03-22"), y = .31, label = "(1)") +
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

write.png(nyplot2, "./figures/ny2.png")


### 4.10. Combined #####

dplots<- list(gbplot, itplot, frplot, esplot, deplot, caplot, atplot, nyplot, flplot)

# saveRDS(dplots, "./data/results/dplots.rds")


tiff("./figures/combined.tiff",
     height = 22, width = 31, units = "cm", 
     compression = "lzw", res = 300)
combined <- grid.arrange(gbplot, itplot, frplot, esplot, deplot, caplot, atplot, nyplot, flplot, ncol=3, 
                         left = "Daily percent change in electricity consumption (%)")
combined
dev.off()

# caption manual outside figure
# bottom =  textGrob("*Training set mean average percentage error. 
# Baseline: country-specific dynamic harmonic regression with quadratic temperature, 
# calendar effects, Fourier terms for complex seasonality and ARIMA(p,d,q) errors.
# Dark and light shades indicate 80% and 95% prediction intervals respectively. 
# Sundays are  colored grey. Note that vertical axis scales are different for each row.",
# gp = gpar(fontsize = 10))

dplots2<- list(gbplot2, itplot2, frplot2, esplot2, deplot2, caplot2, atplot2, nyplot2, flplot2)

# saveRDS(dplots2, "./data/results/dplots2.rds")
dplots2<- readRDS("./data/results/dplots2.rds")


tiff("./figures/combined2.tiff",
     height = 22, width = 31, units = "cm", 
     compression = "lzw", res = 300)
combined <- grid.arrange(gbplot2, itplot2, frplot2, esplot2, deplot2, caplot2, atplot2, nyplot2, flplot2,
                         ncol=3, left = "Daily electricity consumption (TWh)")
combined
dev.off()


# merge diff results
dif_results<- bind_rows(at_result, ca_result, de_result, es_result, fl_result,
                        fr_result, gb_result, it_result, ny_result)


# saveRDS(dif_results, "./data/results/dif_results.rds")
# write.csv(dif_results, "./data/results/dif_results.csv")

############# 4.11. Cumulative ###############

atc<- at_result %>% filter(date >= "2020-03-15") %>% mutate(t = 0:(length(date)-1))
cac<- ca_result %>% filter(date >= "2020-03-18") %>% mutate(t = 0:(length(date)-1))
dec<- de_result %>% filter(date >= "2020-03-21") %>% mutate(t = 0:(length(date)-1))
esc<- es_result %>% filter(date >= "2020-03-13") %>% mutate(t = 0:(length(date)-1))
flc<- fl_result %>% filter(date >= "2020-04-02") %>% mutate(t = 0:(length(date)-1))
frc<- fr_result %>% filter(date >= "2020-03-16") %>% mutate(t = 0:(length(date)-1))
gbc<- gb_result %>% filter(date >= "2020-03-22") %>% mutate(t = 0:(length(date)-1))
itc<- it_result %>% filter(date >= "2020-03-09") %>% mutate(t = 0:(length(date)-1))
nyc<- ny_result %>% filter(date >= "2020-03-21") %>% mutate(t = 0:(length(date)-1))

cum_results<- bind_rows(atc, cac, dec, esc, flc, frc, gbc, itc, nyc) %>%
  arrange(country, t) %>% select(country, t, date, actual, fct, dif) %>%
  group_by(country) %>% 
  mutate(index = 100*cumsum(actual)/cumsum(fct),
         index = ifelse(t == 0, 100, index),
         index_pc = index-100)
  
# saveRDS(cum_results, "./data/results/cum_results.rds")
# write.csv(cum_results, "./data/results/cum_results.csv")
cum_results<- readRDS("./data/results/cum_results.rds")

legend<- cum_results %>% summarize(t = max(t), index = last(index), index_pc = last(index_pc))
legend %>% arrange(index_pc)
my_colors <- RColorBrewer::brewer.pal(4, "Blues")[2:4]

cumplot<- ggplot(cum_results, aes(t, index_pc, color = fct_reorder(country, index, .fun='last'))) +
  geom_line(size = 1, alpha = 1.5) + 
  geom_point(data = legend, aes(x = t, y = index_pc)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text_repel(data = legend, aes(x = t, y = index_pc, label = country),
            nudge_x = 5, size = 4, fontface = "bold") +
  scale_color_viridis_d() +
  scale_y_continuous(sec.axis = sec_axis(~ .)) +
  labs(x = "Days from lockdown", y = "Cumulative consumption change (%)") +
  mytheme() + theme(legend.position = "none")

write.png(cumplot, "./figures/cumulative.png")

show_col(viridis_pal()(9))

viridis_col<- c("#440154FF", "#472D7BFF", "#3B528BFF", "#2C728EFF", "#21908CFF",
                "#27AD81FF", "#5DC863FF", "#AADC32FF", "#FDE725FF")

# "#440154FF" "#472D7BFF" "#3B528BFF" "#2C728EFF" "#21908CFF" # from dark to light
# "#27AD81FF" "#5DC863FF" "#AADC32FF" "#FDE725FF"



######################################## 5. Stringency #########################################

# all vbles binary: either national closure or 0
stringencyEU<- read.csv("./data/raw/stringency/OxCGRT_latest.csv") %>%
  select(country = CountryName, date = Date, school = C1_School.closing, school_general = C1_Flag,
         workplace = C2_Workplace.closing, workplace_general = C2_Flag,
         events = C3_Cancel.public.events, events_general = C3_Flag,
         transport = C5_Close.public.transport, transport_general = C5_Flag,
         stayhome = C6_Stay.at.home.requirements, stayhome_general = C6_Flag,
         movement = C7_Restrictions.on.internal.movement, movement_general = C7_Flag,
         travel = C8_International.travel.controls,
         index = StringencyIndex) %>%
  filter(country %in% c("Germany", "Spain", "Italy", "France", "Austria", "United Kingdom")) %>%
  droplevels() %>%
  mutate(country = as.factor(country),
         date = ymd(date),
         school = as.factor(replace_na(ifelse(school_general == 1 & school == 3, 1, 0), 0)),
         workplace = as.factor(replace_na(ifelse(workplace_general == 1 & workplace == 3, 1, 0), 0)),
         events = as.factor(replace_na(ifelse(events_general == 1 & events == 2, 1, 0), 0)),
         transport = as.factor(replace_na(ifelse(transport_general == 1 & transport == 2, 1, 0), 0)),
         stayhome = as.factor(replace_na(ifelse(transport_general == 1 & stayhome == 3, 1, 0), 0)),
         movement = as.factor(replace_na(ifelse(movement_general == 1 & movement == 2, 1, 0), 0)),
         travel = as.factor(replace_na(ifelse(travel == 4, 1, 0), 0))) %>%
  select(country, date, school, workplace, events, transport, movement, travel, index) %>%
  arrange(country, date) %>% as_tibble()

stringencyEU$country<- plyr::mapvalues(stringencyEU$country,
              from = c("Germany", "Spain", "Italy", "France", "Austria", "United Kingdom"),
              to = c("DE", "ES", "IT", "FR", "AT", "GB"))


stringencyUS<- read.csv("./data/raw/stringency/OxCGRT_US_states_temp.csv") %>%
  select(country = RegionName, date = Date, school = C1_School.closing, school_general = C1_Flag,
         workplace = C2_Workplace.closing, workplace_general = C2_Flag,
         events = C3_Cancel.public.events, events_general = C3_Flag,
         transport = C5_Close.public.transport, transport_general = C5_Flag,
         stayhome = C6_Stay.at.home.requirements, stayhome_general = C6_Flag,
         movement = C7_Restrictions.on.internal.movement, movement_general = C7_Flag,
         travel = C8_International.travel.controls,
         index = StringencyIndex) %>%
  filter(country %in% c("California", "New York", "Florida")) %>%
  droplevels() %>%
  mutate(country = as.factor(country),
         date = ymd(date),
         school = as.factor(replace_na(ifelse(school_general == 1 & school == 3, 1, 0), 0)),
         workplace = as.factor(replace_na(ifelse(workplace_general == 1 & workplace == 3, 1, 0), 0)),
         events = as.factor(replace_na(ifelse(events_general == 1 & events == 2, 1, 0), 0)),
         transport = as.factor(replace_na(ifelse(transport_general == 1 & transport == 2, 1, 0), 0)),
         stayhome = as.factor(replace_na(ifelse(transport_general == 1 & stayhome == 3, 1, 0), 0)),
         movement = as.factor(replace_na(ifelse(movement_general == 1 & movement == 2, 1, 0), 0)),
         travel = as.factor(replace_na(ifelse(travel == 4, 1, 0), 0))) %>%
  select(country, date, school, workplace, events, transport, movement, travel, index) %>%
  arrange(country, date) %>% as_tibble()

stringencyUS$country<- plyr::mapvalues(stringencyUS$country,
              from = c("California", "Florida", "New York"),
              to = c("CA", "FL", "NY"))

stringency<- bind_rows(stringencyEU, stringencyUS)


# stringency raw
labels<- stringency %>% filter(date < "2020-08-01") %>%
  group_by(country) %>% summarise(date = last(date)+2, index = last(index))
strplot<- ggplot(stringency, aes(date, index, color = country)) +
  geom_line(size = 1, alpha = .8) + 
  labs(x = element_blank(), y = "Stringency index (0-100)") +
  scale_color_manual(values = mycolors) +
  geom_text_repel(data = labels, aes(x = last(date)+10, y = index, label = country, color = country),
                  segment.color = "transparent", size = 4.5) +
  mytheme() + theme(legend.position = "none", legend.title = element_blank())

write.png(strplot, "./figures/appendix/stringency.png")

# policy components of the stringency index panel data # 
cum_results %>% group_by(country) %>% summarize(cumdif = last(index)-100) %>% arrange(cumdif)

dif_results<- readRDS("./data/results/dif_results.rds") %>%
  select(country, date, dif) %>% mutate(country = as.factor(country))

str_data<- right_join(dif_results, stringency, by = c("country", "date")) %>%
  mutate(country = as.factor(country)) %>% na.omit() %>% droplevels()

# saveRDS(str_data, "./data/results/str_data.rds")
# write.csv(str_data, "./data/results/str_data.csv")


# stringency index vs. electricity consumption

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode<- str_data %>% group_by(country) %>% 
  summarise(mode = getmode(index), median = median(index), mean = mean(index))


medpoint<- str_data %>% group_by(country) %>%
  summarise(dif = median(dif), index = median(index))

# order: AT, CA, DE, ES, FL, \newline FR, GB, IT, NY
mycolors = c(viridis_col[7], viridis_col[6],viridis_col[5], viridis_col[4], viridis_col[9],
             viridis_col[3], viridis_col[1], viridis_col[2], viridis_col[8])
strindexplot<- ggplot(str_data, aes(index, dif)) +
  geom_point(aes(color = country), alpha = .15) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_smooth(method = loess, se = F, color = "black", size = 1) +
  geom_text_repel(data = medpoint, aes(x = index, y = dif, color = country, label = country), 
            fontface = "bold", size = 4.5, force = .01) +
  scale_color_manual(values = mycolors) +
  scale_y_continuous(sec.axis = sec_axis(~ .)) +
  labs(x = "Stringency index", y = "Change daily consumption (%)") +
  mytheme() + theme(aspect.ratio = 1, legend.position = "none")

write.png(strindexplot, "./figures/stringency_index.png", width=5, height=5)

##### Bias #####

# naif comparison with 2019
# daily doesn't make sense it has to bee weekly
naifdata<- readRDS("./data/clean/ddata2.rds") %>% unique()  %>% 
  mutate(year = year(date)) %>% filter(year %in% c(2019:2020)) %>% filter(date < "2020-08-01") %>%
  select(country, date, load, year) %>% mutate(date = substr(date, 6, 10)) %>% spread(year, load) %>%
  mutate(dif = 100*(`2020`-`2019`)/`2019`)

pred_dif<- readRDS("./data/results/cum_results.rds") %>%
  select(country, date, pred_dif = dif) %>%
  mutate(date = substr(date, 6, 10))

naif<- left_join(naifdata, pred_dif) %>%
  select(country, date, Naïf = dif, ARIMA = pred_dif) %>% 
  gather(key, value, -country, -date) %>%
  mutate(date = as.Date(paste("2020-", date), sep = "")) 

ggplot(naif, aes(date, value, color = key)) +
  geom_line() +
  facet_wrap(~country, scales = "free") +
  mytheme() + theme(legend.position = "bottom")

# weekly
naifdata<- readRDS("./data/clean/ddata2.rds") %>% unique() %>% 
  mutate(year = year(date)) %>% filter(year %in% c(2019:2020)) %>% filter(date < "2020-08-01") %>%
  select(country, date, load, year) %>% 
  mutate(week = week(date)) %>% group_by(country, year, week) %>%
  summarise(load = sum(load)) %>%
  spread(year, load) %>%
  group_by(country, week) %>%
  summarise(dif = 100*(`2020`-`2019`)/`2019`)

# check
ggplot(naifdata, aes(week, dif)) + geom_line() + facet_wrap(~country) + mytheme()

pred_dif<- readRDS("./data/results/cum_results.rds") %>%
  select(country, date, actual, fct) %>%
  mutate(week = week(date)) %>% group_by(country, week) %>%
  summarise(pred_dif = 100*(sum(actual)-sum(fct))/sum(fct))

naif<- left_join(naifdata, pred_dif) %>%
  select(country, week, Naïf = dif, ARIMA = pred_dif) %>% 
  gather(key, value, -country, -week) %>% 
  filter(week %in% c(12:30))

ggplot(naif, aes(week, value, color = key)) +
  geom_line() +
  facet_wrap(~country, scales = "free") +
  mytheme() + theme(legend.position = "bottom")


naifbias<- naif %>% spread(key, value) %>% mutate(bias = Naïf-ARIMA)

nbiasplot<- ggplot(naifbias, aes(week, bias)) +
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~country) +
  geom_ribbon(aes(ymin=0, ymax=bias), alpha = .2) +
  labs(x = "Week", y = "Bias of Naive w.r.t. ARIMA (p.p.)") +
  mytheme()

write.png(nbiasplot, "./figures/appendix/naivebias.png")

ggplot(naifbias, aes(ARIMA, Naïf, color = country)) +
  geom_point(alpha = .4) +
  geom_smooth(method = "lm", se = F) +
  geom_abline(slope = 1) +
  coord_cartesian(xlim = c(-30, 20), ylim = c(-30, 20)) +
  mytheme() + 
  theme(aspect.ratio = 1, legend.position = c(.9, .4), legend.title = element_blank())

# accuracy of a naif comparison
wddata<- readRDS("./data/clean/ddata2.rds") %>% unique() %>%
  mutate(year = year(date), week = week(date)) %>% group_by(country, year, week) %>%
  summarize(load = sum(load), max_temp = mean(max_temp))

# get accuracy, anyway not comparable because it's weekly rather than daily  
for(c in unique(wddata$country)){
  ola<- wddata %>% filter(country == c) %>% ungroup()
  
  for(y in c(2016:2018)){
    a<- ola %>% filter(year == y) %>% select(load) %>% as.ts()
    b<- ola %>% filter(year == y+1) %>% select(load) %>% as.ts()
    
    acc[y]<- accuracy(a, b) %>% print
  }
  
}

at18<- wddata %>% filter(country == "AT", year == "2018")
at19<- wddata %>% filter(country == "AT", year == "2019")

accuracy(at18$load, at19$load)

##### Extension #####

eucountries<- c("BG", "CH", "CZ", "DK", "EE", "FI", "GR", "HR", "HU", "IE", "LT", "LV", "NL",
                "NO", "PL", "PT", "RO", "SI")

#all
usregions<- c("CA", "CAR", "CENT", "FL", "MIDA", "MIDW", "NE", "NW", "NY", "SW", "TEN", "TEX" )

# only new
usregions<- c("CAR", "CENT", "MIDA", "MIDW", "NE", "NW", "SW", "TEN", "TEX" )

# proba
usregions<- c("CAR", "NE", "SW", "TEN", "TEX" )

viridis_col<- c("#440154FF", "#472D7BFF", "#3B528BFF", "#2C728EFF", "#21908CFF",
                "#27AD81FF", "#5DC863FF", "#AADC32FF", "#FDE725FF")

# forecast whole process
for(co in eucountries){
  temp<- read.delim(paste("./data/raw/temperature/", co, "_maxtemp.dat", sep = ""), sep = ",") %>%
    rename(date = day, max_temp = maxT) %>% mutate(date = as.Date(date), country = co) %>% as_tibble()
  
  raw<- readRDS("./data/clean/ddata_all2.rds") %>% filter(country == co) %>%
    select(country, date, load, wday, sunday)
  
  holiday<- readRDS("./data/clean/ddata_all.rds") %>% filter(country == "FR") %>%
    select(country, date, holiday) %>% mutate(country = co)
  
  ddata<- left_join(raw, holiday) %>% left_join(temp)
  
  get_results<- function(where = co, k = NULL){ # if k is set no automatic selection (which takes long time)
    
    # define train (4 years 2015-2018) and test (1 year 2019) sets
    train<- ddata %>% filter(country == where, date < "2020-03-01") %>% na.omit()
    test<- ddata %>% filter(country == where, date >= "2020-03-01") %>% na.omit()
    
    # convert into multiple (weekly and yearly) seasonality time series (msts)
    train_msts<- msts(train$load, seasonal.periods = c(7, 365.25))
    test_msts<- msts(test$load, seasonal.periods = c(7, 365.25)) %>% na.omit()
    
    lambda<- BoxCox.lambda(train_msts)
    
    optimize_k<- function(){
      # if k not set then automatic selection with loop
      df<- data.frame(k = c(1:30), AICC = rep(NA, 30))
      for (k in c(1:30)) {
        harmonic<- fourier(train_msts, K = c(3, k))
        
        fit <- auto.arima(train_msts, # training data
                          xreg = cbind(train$max_temp, train$max_temp^2,
                                       as.factor(train$holiday), harmonic), # covariates
                          seasonal = FALSE, lambda = lambda)
        
        df[k,"AICC"] <- round(fit[["aicc"]],2)
      }
      k<- df[which.min(df$AICC), "k"]
    }
    
    # get optimal k for the yearly fourier term
    k<- ifelse(is.null(k), optimize_k(), k)
    
    # calculate fourier term
    harmonic<- fourier(train_msts, K = c(3, k))
    
    # run model
    fit <- auto.arima(train_msts, 
                      xreg = cbind(train$max_temp, train$max_temp^2,
                                   as.factor(train$holiday), harmonic), 
                      seasonal = FALSE, lambda = lambda)
    
    # forecast fourier term and other control variables
    test_harmonic<- fourier(train_msts, K = c(3, k), h = length(test_msts))
    
    # forecast
    fct<- forecast(fit, xreg = cbind(test$max_temp, test$max_temp^2,
                                     as.factor(test$holiday), test_harmonic))
    
    result<- data.frame(date = test$date,
                        actual = test$load,
                        fct = as.numeric(fct$mean),
                        low80 = as.numeric(fct$lower[, 1]),
                        low95 = as.numeric(fct$lower[, 2]),
                        up80 = as.numeric(fct$upper[, 1]),
                        up95 = as.numeric(fct$upper[, 2])) %>%
      mutate(dif = (100*(actual-fct))/actual,
             diflow80 = (100*(actual-low80))/actual,
             diflow95 = (100*(actual-low95))/actual,
             difup80 = (100*(actual-up80))/actual,
             difup95 = (100*(actual-up95))/actual,
             country = where) %>% as_tibble()
    
    mylist<<- list(fit = fit, 
                   forecast = fct, 
                   k = k,
                   lambda = lambda,
                   train = train, 
                   test = test,
                   result = result)
    
    saveRDS(mylist, paste("./data/results/forecast/results_", where, ".rds", sep = ""))
    
    return(round(c(k, lambda),2))
  }
  
  get_results(where = co)
  
  list<- readRDS(paste("./data/results/forecast/results_", co, ".rds", sep = ""))
  
  fit<- list$fit
  checkresiduals(fit)
  coeftest(fit)
  
  result<- list$result %>% mutate(wday = weekdays(date))
  
  sunday<- result %>% mutate(wday = weekdays(date)) %>% filter(wday == "Sunday")
  plot<- ggplot(result, aes(date, dif)) +
    geom_ribbon(aes(date, ymin = diflow80, ymax = difup80), fill = viridis_col[5], alpha = .2) +
    geom_ribbon(aes(date, ymin = diflow95, ymax = difup95), fill = viridis_col[5], alpha = .15) +
    geom_line(color = viridis_col[5]) + geom_point(color = viridis_col[5]) + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(data = sunday, aes(date, dif), color = "darkgrey", size = 1.5) +
    labs(x = element_blank(), y = element_blank(), title = co) + 
    mytheme() + theme(plot.title = element_text(hjust = 0.5))
  
  write.png(plot, paste("./figures/", co, ".png", sep = ""))
  
  plot2<- ggplot(result, aes(date, fct)) +
    geom_line(size = .5) +
    geom_ribbon(aes(date, ymin = low80, ymax = up80), alpha = .12) +
    geom_ribbon(aes(date, ymin = low95, ymax = up95), alpha = .08) +
    geom_line(aes(date, actual),  color = viridis_col[5], size = 1) +
    labs(x = element_blank(), y = element_blank(), title = co) +
    mytheme() + theme(plot.title = element_text(hjust = 0.5))
  
  write.png(plot2, paste("./figures/", co, "2.png", sep = ""))
  
}

# forecast whole process (same as with Europe but with US holidays)
for(co in usregions){
  temp<- read.delim(paste("./data/raw/temperature/", co, "_maxtemp.dat", sep = ""), sep = ",") %>%
    rename(date = day, max_temp = maxT) %>% mutate(date = as.Date(date), country = co) %>% as_tibble()
  
  raw<- readRDS("./data/clean/ddata_all2.rds") %>% filter(country == co) %>%
    select(country, date, load, wday, sunday)
  
  holiday<- readRDS("./data/clean/ddata_all.rds") %>% filter(country == "CA") %>%
    select(country, date, holiday) %>% mutate(country = co)
  
  ddata<- left_join(raw, holiday) %>% left_join(temp)
  
  get_results<- function(where = co, k = NULL){ # if k is set no automatic selection (which takes long time)
    
    # define train (4 years 2015-2018) and test (1 year 2019) sets
    train<- ddata %>% filter(country == where, date < "2020-03-01") %>% na.omit()
    test<- ddata %>% filter(country == where, date >= "2020-03-01") %>% na.omit()
    
    # convert into multiple (weekly and yearly) seasonality time series (msts)
    train_msts<- msts(train$load, seasonal.periods = c(7, 365.25))
    test_msts<- msts(test$load, seasonal.periods = c(7, 365.25)) %>% na.omit()
    
    lambda<- BoxCox.lambda(train_msts)
    
    optimize_k<- function(){
      # if k not set then automatic selection with loop
      df<- data.frame(k = c(1:30), AICC = rep(NA, 30))
      for (k in c(1:30)) {
        harmonic<- fourier(train_msts, K = c(3, k))
        
        fit <- auto.arima(train_msts, # training data
                          xreg = cbind(train$max_temp, train$max_temp^2,
                                       as.factor(train$holiday), harmonic), # covariates
                          seasonal = FALSE, lambda = lambda)
        
        df[k,"AICC"] <- round(fit[["aicc"]],2)
      }
      k<- df[which.min(df$AICC), "k"]
    }
    
    # get optimal k for the yearly fourier term
    k<- ifelse(is.null(k), optimize_k(), k)
    
    # calculate fourier term
    harmonic<- fourier(train_msts, K = c(3, k))
    
    # run model
    fit <- auto.arima(train_msts, 
                      xreg = cbind(train$max_temp, train$max_temp^2,
                                   as.factor(train$holiday), harmonic), 
                      seasonal = FALSE, lambda = lambda)
    
    # forecast fourier term and other control variables
    test_harmonic<- fourier(train_msts, K = c(3, k), h = length(test_msts))
    
    # forecast
    fct<- forecast(fit, xreg = cbind(test$max_temp, test$max_temp^2,
                                     as.factor(test$holiday), test_harmonic))
    
    result<- data.frame(date = test$date,
                        actual = test$load,
                        fct = as.numeric(fct$mean),
                        low80 = as.numeric(fct$lower[, 1]),
                        low95 = as.numeric(fct$lower[, 2]),
                        up80 = as.numeric(fct$upper[, 1]),
                        up95 = as.numeric(fct$upper[, 2])) %>%
      mutate(dif = (100*(actual-fct))/actual,
             diflow80 = (100*(actual-low80))/actual,
             diflow95 = (100*(actual-low95))/actual,
             difup80 = (100*(actual-up80))/actual,
             difup95 = (100*(actual-up95))/actual,
             country = where) %>% as_tibble()
    
    mylist<<- list(fit = fit, 
                   forecast = fct, 
                   k = k,
                   lambda = lambda,
                   train = train, 
                   test = test,
                   result = result)
    
    saveRDS(mylist, paste("./data/results/forecast/results_", where, ".rds", sep = ""))
    
    return(round(c(k, lambda),2))
  }
  
  get_results(where = co)
  
  list<- readRDS(paste("./data/results/forecast/results_", co, ".rds", sep = ""))
  
  fit<- list$fit
  checkresiduals(fit)
  coeftest(fit)
  
  result<- list$result %>% mutate(wday = weekdays(date))
  
  sunday<- result %>% mutate(wday = weekdays(date)) %>% filter(wday == "Sunday")
  plot<- ggplot(result, aes(date, dif)) +
    geom_ribbon(aes(date, ymin = diflow80, ymax = difup80), fill = viridis_col[5], alpha = .2) +
    geom_ribbon(aes(date, ymin = diflow95, ymax = difup95), fill = viridis_col[5], alpha = .15) +
    geom_line(color = viridis_col[5]) + geom_point(color = viridis_col[5]) + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(data = sunday, aes(date, dif), color = "darkgrey", size = 1.5) +
    labs(x = element_blank(), y = element_blank(), title = co) + 
    mytheme() + theme(plot.title = element_text(hjust = 0.5))
  
  write.png(plot, paste("./figures/", co, ".png", sep = ""))
  
  plot2<- ggplot(result, aes(date, fct)) +
    geom_line(size = .5) +
    geom_ribbon(aes(date, ymin = low80, ymax = up80), alpha = .12) +
    geom_ribbon(aes(date, ymin = low95, ymax = up95), alpha = .08) +
    geom_line(aes(date, actual),  color = viridis_col[5], size = 1) +
    labs(x = element_blank(), y = element_blank(), title = co) +
    mytheme() + theme(plot.title = element_text(hjust = 0.5))
  
  write.png(plot2, paste("./figures/", co, "2.png", sep = ""))
  
}

# create figure 1
for(co in eucountries){

  list<- readRDS(paste("./data/results/forecast/results_", co, ".rds", sep = ""))
  
  fit<- list$fit
  checkresiduals(fit)
  coeftest(fit)
  
  result<- list$result %>% mutate(wday = weekdays(date))
  
  sunday<- result %>% mutate(wday = weekdays(date)) %>% filter(wday == "Sunday")
  plot<- ggplot(result, aes(date, dif)) +
    geom_ribbon(aes(date, ymin = diflow80, ymax = difup80), fill = viridis_col[5], alpha = .2) +
    geom_ribbon(aes(date, ymin = diflow95, ymax = difup95), fill = viridis_col[5], alpha = .15) +
    geom_line(color = viridis_col[5]) + geom_point(color = viridis_col[5]) + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(data = sunday, aes(date, dif), color = "darkgrey", size = 1.5) +
    labs(x = element_blank(), y = element_blank(), title = co) + 
    mytheme() + theme(plot.title = element_text(hjust = 0.5))
  
  write.png(plot, paste("./figures/", co, ".png", sep = ""))
  
  
}

# create figure 2
for(co in eucountries){
  
  list<- readRDS(paste("./data/results/forecast/results_", co, ".rds", sep = ""))
  
  fit<- list$fit
  checkresiduals(fit)
  coeftest(fit)
  
  result<- list$result %>% mutate(wday = weekdays(date))
  
  sunday<- result %>% mutate(wday = weekdays(date)) %>% filter(wday == "Sunday")
  plot2<- ggplot(result, aes(date, fct)) +
    geom_line(size = .5) +
    geom_ribbon(aes(date, ymin = low80, ymax = up80), alpha = .12) +
    geom_ribbon(aes(date, ymin = low95, ymax = up95), alpha = .08) +
    geom_line(aes(date, actual),  color = viridis_col[5], size = 1) +
    labs(x = element_blank(), y = element_blank(), title = co) +
    mytheme() + theme(plot.title = element_text(hjust = 0.5))
  
  write.png(plot2, paste("./figures/", co, "2.png", sep = ""))
  
  
}

# accuracy
for(co in eucountries){
  
  temp<- read.delim(paste("./data/raw/temperature/", co, "_maxtemp.dat", sep = ""), sep = ",") %>%
    rename(date = day, max_temp = maxT) %>% mutate(date = as.Date(date), country = co) %>% as_tibble()
  
  raw<- readRDS("./data/clean/ddata_all.rds") %>% filter(country == co) %>%
    select(country, date, load, wday, sunday)
  
  holiday<- readRDS("./data/clean/ddata_all.rds") %>% filter(country == "DE") %>%
    select(country, date, holiday) %>% mutate(country = co)
  
  ddata<- left_join(raw, holiday) %>% left_join(temp)
  
  list<- readRDS(paste("./data/results/forecast/results_", co, ".rds", sep = ""))
  
  k<- list$k
  
  get_accuracy<- function(where = "ES", k = 28){
    
    # 1. ARIMA
    # define train (4 years 2015-2018) and test (1 year 2019) sets
    train<- ddata %>% filter(country == where, date < "2019-01-01") %>% na.omit()
    test<- ddata %>% filter(country == where, date >= "2019-01-01", date < "2020-01-01") %>% na.omit()
    
    # convert into multiple (weekly and yearly) seasonality time series (msts)
    train_msts<- msts(train$load, seasonal.periods = c(7, 365.25))
    test_msts<- msts(test$load, seasonal.periods = c(7, 365.25))
    
    lambda<- BoxCox.lambda(train_msts)
    
    # fourier terms for multiple seasonality
    harmonic<- fourier(train_msts, K = c(3, k))
    
    # run model
    arima_fit <- auto.arima(train_msts, 
                            xreg = cbind(train$max_temp, train$max_temp^2,
                                         as.factor(train$holiday), harmonic), 
                            seasonal = FALSE, lambda = lambda)
    
    # forecast
    # define controls for de forecast period of 1 year
    test_harmonic<- fourier(train_msts, K = c(3, k), h = 365)
    
    arima_fct<- forecast(arima_fit, xreg = cbind(test$max_temp, test$max_temp^2,
                                                 as.factor(test$holiday), test_harmonic))
    # accuracy
    arima_acc<- accuracy(as.vector(arima_fct$mean), as.vector(test_msts))
    
    # 2. NNAR 
    nnar_fit<- nnetar(train_msts, xreg = cbind(as.factor(train$holiday), as.factor(train$wday),
                                               train$max_temp), lambda = lambda)
    nnar_fct<- forecast(nnar_fit, xreg = cbind(as.factor(test$holiday), as.factor(test$wday),
                                               test$max_temp), h = 365)
    nnar_acc<- accuracy(as.vector(nnar_fct$mean), as.vector(test_msts))
    
    
    # 3. TBATS 
    tbats_fit<- tbats(train_msts, lambda = lambda)
    tbats_fct<- forecast(tbats_fit, h = 365)
    tbats_acc<- accuracy(as.vector(tbats_fct$mean), as.vector(test_msts))
    
    
    # 4. STLF 
    stlf_fit<- stlf(train_msts, lambda = lambda)
    stlf_fct<- forecast(stlf_fit, h = 365)
    stlf_acc<- accuracy(as.vector(stlf_fct$mean), as.vector(test_msts))
    
    
    # 6. Comparison 
    
    acc_comparison<- data.frame(arima = round(as.vector(arima_acc), 2),
                                nnar = round(as.vector(nnar_acc), 2),
                                tbats = round(as.vector(tbats_acc), 2),
                                stlf = round(as.vector(stlf_acc), 2))
    rownames(acc_comparison)<- c("ME", "RMSE", "MAE", "MPE", "MAPE")
    
    
    mylist<<- list(arima = arima_fct, 
                   nnar = nnar_fct, 
                   tbats = tbats_fct, 
                   stlf = stlf_fct,
                   accuracy = acc_comparison)
    
    saveRDS(mylist, paste("./data/results/accuracy/accuracy_", where, ".rds", sep = ""))
    
    return(mylist$accuracy)
  }
  
  get_accuracy(where = co, k = k)
  
}

# check accuracy
for(co in eucountries){
  acc<- readRDS(paste("./data/results/accuracy/accuracy_", co, ".rds", sep = ""))
  print(acc$accuracy)
}

# accuracy (same as with Europe but with US)
for(co in usregions){
  
  temp<- read.delim(paste("./data/raw/temperature/", co, "_maxtemp.dat", sep = ""), sep = ",") %>%
    rename(date = day, max_temp = maxT) %>% mutate(date = as.Date(date), country = co) %>% as_tibble()
  
  raw<- readRDS("./data/clean/ddata_all.rds") %>% filter(country == co) %>%
    select(country, date, load, wday, sunday)
  
  holiday<- readRDS("./data/clean/ddata_all.rds") %>% filter(country == "DE") %>%
    select(country, date, holiday) %>% mutate(country = co)
  
  ddata<- left_join(raw, holiday) %>% left_join(temp)
  
  list<- readRDS(paste("./data/results/forecast/results_", co, ".rds", sep = ""))
  
  k<- list$k
  
  get_accuracy<- function(where = "ES", k = 28){
    
    # 1. ARIMA
    # define train (4 years 2015-2018) and test (1 year 2019) sets
    train<- ddata %>% filter(country == where, date < "2019-01-01") %>% na.omit()
    test<- ddata %>% filter(country == where, date >= "2019-01-01", date < "2020-01-01") %>% na.omit()
    
    # convert into multiple (weekly and yearly) seasonality time series (msts)
    train_msts<- msts(train$load, seasonal.periods = c(7, 365.25))
    test_msts<- msts(test$load, seasonal.periods = c(7, 365.25))
    
    lambda<- BoxCox.lambda(train_msts)
    
    # fourier terms for multiple seasonality
    harmonic<- fourier(train_msts, K = c(3, k))
    
    # run model
    arima_fit <- auto.arima(train_msts, 
                            xreg = cbind(train$max_temp, train$max_temp^2,
                                         as.factor(train$holiday), harmonic), 
                            seasonal = FALSE, lambda = lambda)
    
    # forecast
    # define controls for de forecast period of 1 year
    test_harmonic<- fourier(train_msts, K = c(3, k), h = 365)
    
    arima_fct<- forecast(arima_fit, xreg = cbind(test$max_temp, test$max_temp^2,
                                                 as.factor(test$holiday), test_harmonic))
    # accuracy
    arima_acc<- accuracy(as.vector(arima_fct$mean), as.vector(test_msts))
    
    # 2. NNAR 
    nnar_fit<- nnetar(train_msts, xreg = cbind(as.factor(train$holiday), as.factor(train$wday),
                                               train$max_temp), lambda = lambda)
    nnar_fct<- forecast(nnar_fit, xreg = cbind(as.factor(test$holiday), as.factor(test$wday),
                                               test$max_temp), h = 365)
    nnar_acc<- accuracy(as.vector(nnar_fct$mean), as.vector(test_msts))
    
    
    # 3. TBATS 
    tbats_fit<- tbats(train_msts, lambda = lambda)
    tbats_fct<- forecast(tbats_fit, h = 365)
    tbats_acc<- accuracy(as.vector(tbats_fct$mean), as.vector(test_msts))
    
    
    # 4. STLF 
    stlf_fit<- stlf(train_msts, lambda = lambda)
    stlf_fct<- forecast(stlf_fit, h = 365)
    stlf_acc<- accuracy(as.vector(stlf_fct$mean), as.vector(test_msts))
    
    
    # 6. Comparison 
    
    acc_comparison<- data.frame(arima = round(as.vector(arima_acc), 2),
                                nnar = round(as.vector(nnar_acc), 2),
                                tbats = round(as.vector(tbats_acc), 2),
                                stlf = round(as.vector(stlf_acc), 2))
    rownames(acc_comparison)<- c("ME", "RMSE", "MAE", "MPE", "MAPE")
    
    
    mylist<<- list(arima = arima_fct, 
                   nnar = nnar_fct, 
                   tbats = tbats_fct, 
                   stlf = stlf_fct,
                   accuracy = acc_comparison)
    
    saveRDS(mylist, paste("./data/results/accuracy/accuracy_", where, ".rds", sep = ""))
    
    return(mylist$accuracy)
  }
  
  get_accuracy(where = co, k = k)
  
}

for(co in usregions){
  acc<- readRDS(paste("./data/results/accuracy/accuracy_", co, ".rds", sep = ""))
  print(acc$accuracy)
}

# cumulative decline
cum_resultsEU<- data.frame()
for(co in eucountries){
  list<- readRDS(paste("./data/results/forecast/results_", co, ".rds", sep = ""))
  
  # As of 13 March 2020, when the number of new cases became greater than those in China, 
  # the WHO began to consider Europe the active centre of the COVID-19 pandemic.
  # https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Europe
  result<- list$result %>% mutate(wday = weekdays(date)) %>% 
    filter(date >= "2020-03-13") %>% mutate(t = 0:(length(date)-1))
  
  cum<- result %>% select(country, t, date, actual, fct, dif) %>%
    mutate(index = 100*cumsum(actual)/cumsum(fct),
           index = ifelse(t == 0, 100, index),
           index_pc = index-100,
           country = co)
  
  cum_resultsEU<- bind_rows(cum_resultsEU, cum)
  
  saveRDS(cum_resultsEU, "./data/results/cum_resultsEU.rds")
}

cum_resultsUS<- readRDS("./data/results/cum_resultsEU.rds")


legend<- cum_resultsEU %>% group_by(country) %>% filter(country %nin% c("RO", "CH")) %>%
  summarize(t = max(t), index = last(index), index_pc = last(index_pc))
legend %>% arrange(index_pc)

cumplotEU<- cum_resultsEU  %>% filter(country %nin% c("RO", "CH")) %>%
  ggplot(aes(t, index_pc, color = fct_reorder(country, index, .fun='last'))) +
  geom_line(size = 1, alpha = 1.5) + 
  geom_point(data = legend, aes(x = t, y = index_pc)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text_repel(data = legend, aes(x = t, y = index_pc, label = country),
            nudge_x = 3, size = 4, fontface = "bold") +
  scale_color_viridis_d() +
  scale_y_continuous(sec.axis = sec_axis(~ .)) +
  labs(x = "Days from March 13", y = "Cumulative consumption change (%)") +
  mytheme() + theme(legend.position = "none")

write.png(cumplotEU, "")

# cumulative decline in the USA
cum_resultsUS<- data.frame()
for(co in usregions){
  list<- readRDS(paste("./data/results/forecast/results_", co, ".rds", sep = ""))
  
  # As of 13 March 2020, when the number of new cases became greater than those in China, 
  # the WHO began to consider Europe the active centre of the COVID-19 pandemic.
  # https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Europe
  result<- list$result %>% mutate(wday = weekdays(date)) %>% 
    filter(date >= "2020-03-13") %>% mutate(t = 0:(length(date)-1))
  
  cum<- result %>% select(country, t, date, actual, fct, dif) %>%
    mutate(index = 100*cumsum(actual)/cumsum(fct),
           index = ifelse(t == 0, 100, index),
           index_pc = index-100,
           country = co)
  
  cum_resultsEU<- bind_rows(cum_resultsEU, cum)
  
  saveRDS(cum_resultsEU, "./data/results/cum_resultsUS.rds")
}

cum_resultsUS<- readRDS("./data/results/cum_resultsUS.rds")


legend<- cum_resultsUS %>% group_by(country) %>% filter(country %nin% c("RO", "CH")) %>%
  summarize(t = max(t), index = last(index), index_pc = last(index_pc))
legend %>% arrange(index_pc)

cumplotUS<- cum_resultsUS  %>% filter(country %nin% c("RO", "CH")) %>%
  ggplot(aes(t, index_pc, color = fct_reorder(country, index, .fun='last'))) +
  geom_line(size = 1, alpha = 1.5) + 
  geom_point(data = legend, aes(x = t, y = index_pc)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text_repel(data = legend, aes(x = t, y = index_pc, label = country),
                  nudge_x = 3, size = 4, fontface = "bold") +
  scale_color_viridis_d() +
  scale_y_continuous(sec.axis = sec_axis(~ .)) +
  labs(x = "Days from March 13", y = "Cumulative consumption change (%)") +
  mytheme() + theme(legend.position = "none")


# define new manual function
get_arima_results_ext<- function(where = "ES", k = NULL, j = 3, p = 1, d = 1, q = 1){
  
  raw<- readRDS("./data/clean/ddata_all2.rds") %>% filter(country == co) %>%
    select(country, date, load, wday, sunday)
  
  temp<- read.delim(paste("./data/raw/temperature/", co, "_maxtemp.dat", sep = ""), sep = ",") %>%
    rename(date = day, max_temp = maxT) %>% mutate(date = as.Date(date), country = co) %>% as_tibble()
  
  holiday<- readRDS("./data/clean/ddata_all.rds") %>% filter(country == "FR") %>%
    select(country, date, holiday) %>% mutate(country = co)
  
  ddata<- left_join(raw, holiday) %>% left_join(temp)
  
  # define train (4 years 2015-2018) and test (1 year 2019) sets
  train<- ddata %>% filter(country == where, date < "2020-03-01") %>% na.omit()
  test<- ddata %>% filter(country == where, date >= "2020-03-01") %>% na.omit()
  
  # convert into multiple (weekly and yearly) seasonality time series (msts)
  train_msts<- msts(train$load, seasonal.periods = c(7, 365.25))
  test_msts<- msts(test$load, seasonal.periods = c(7, 365.25)) %>% na.omit()
  
  lambda<- BoxCox.lambda(train_msts)
  
  # calculate fourier term
  harmonic<- fourier(train_msts, K = c(j, k))
  
  # run model
  fit <- Arima(train_msts, 
               xreg = cbind(train$max_temp, train$max_temp^2,
                            as.factor(train$holiday), harmonic),
               lambda = lambda, order = c(p,d,q))
  
  # forecast fourier term and other control variables
  test_harmonic<- fourier(train_msts, K = c(j, k), h = length(test_msts))
  
  # forecast
  fct<- forecast(fit, xreg = cbind(test$max_temp, test$max_temp^2,
                                   as.factor(test$holiday), test_harmonic))
  
  result<- data.frame(date = test$date,
                      actual = test$load,
                      fct = as.numeric(fct$mean),
                      low80 = as.numeric(fct$lower[, 1]),
                      low95 = as.numeric(fct$lower[, 2]),
                      up80 = as.numeric(fct$upper[, 1]),
                      up95 = as.numeric(fct$upper[, 2])) %>%
    mutate(dif = (100*(actual-fct))/actual,
           diflow80 = (100*(actual-low80))/actual,
           diflow95 = (100*(actual-low95))/actual,
           difup80 = (100*(actual-up80))/actual,
           difup95 = (100*(actual-up95))/actual,
           country = where) %>% as_tibble()
  
  mylist<<- list(fit = fit, 
                 forecast = fct, 
                 k = k,
                 lambda = lambda,
                 train = train, 
                 test = test,
                 result = result)
  
  saveRDS(mylist, paste("./data/results/forecast_manual/results_", where, ".rds", sep = ""))
  
  return(round(c(k, lambda),2))
}
############# 5.1. BG ############
# arima
# resultados automáticos
list<- readRDS("./data/results/forecast/results_BG.rds") # (2,0,1) k = 11 MAPE = 5.29
list<- readRDS("./data/results/forecast_manual/results_BG.rds")
# resultados manuais

get_arima_results_ext(where = "BG", j = 3, k = 11, p = 4, d = 0, q = 4)
list<- readRDS("./data/results/forecast_manual/results_BG.rds")


list$k
list$lambda
fit<- list$fit
checkresiduals(fit)
coeftest(fit)

result<- list$result %>% mutate(wday = weekdays(date))
result$lockdown<- as.Date("2020-03-16")

sunday<- result %>% mutate(wday = weekdays(date)) %>% filter(wday == "Sunday")
plot<- ggplot(result, aes(date, dif)) +
  geom_ribbon(aes(date, ymin = diflow80, ymax = difup80), fill = viridis_col[6], alpha = .2) +
  geom_ribbon(aes(date, ymin = diflow95, ymax = difup95), fill = viridis_col[6], alpha = .15) +
  geom_line(color = viridis_col[6]) + geom_point(color = viridis_col[6]) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data = sunday, aes(date, dif), color = "darkgrey", size = 1.5) +
  coord_cartesian(ylim = c(-35, 15)) +
  labs(x = element_blank(), y = element_blank(), title = "Bulgaria") + 
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

# write.png(plot, "./figures/BG.png")

plot2<- ggplot(result, aes(date, fct)) +
  geom_line(size = .5) +
  geom_ribbon(aes(date, ymin = low80, ymax = up80), alpha = .12) +
  geom_ribbon(aes(date, ymin = low95, ymax = up95), alpha = .08) +
  geom_line(aes(date, actual),  color = viridis_col[6], size = 1) +
  labs(x = element_blank(), y = element_blank(), title = "Bulgaria") +
  # coord_cartesian(ylim= c(.4, .8)) +
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

# write.png(plot2, "./figures/BG.png")

############# 5.2. CH ############
# arima
# resultados automáticos
list<- readRDS("./data/results/forecast/results_CH.rds") # (1,1,3) k = 10 MAPE = 4.41

# resultados manuais
get_arima_results_ext(where = "CH", j = 3, k = 10, p = 2, d = 1, q = 3)
list<- readRDS("./data/results/forecast_manual/results_CH.rds")


list$k
list$lambda
fit<- list$fit
checkresiduals(fit)
summary(fit)
coeftest(fit)

result<- list$result %>% mutate(wday = weekdays(date))
result$lockdown<- as.Date("2020-03-16")

sunday<- result %>% mutate(wday = weekdays(date)) %>% filter(wday == "Sunday")
plot<- ggplot(result, aes(date, dif)) +
  geom_ribbon(aes(date, ymin = diflow80, ymax = difup80), fill = viridis_col[6], alpha = .2) +
  geom_ribbon(aes(date, ymin = diflow95, ymax = difup95), fill = viridis_col[6], alpha = .15) +
  geom_line(color = viridis_col[6]) + geom_point(color = viridis_col[6]) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data = sunday, aes(date, dif), color = "darkgrey", size = 1.5) +
  #coord_cartesian(ylim = c(-35, 15)) +
  labs(x = element_blank(), y = element_blank(), title = "Switzerland") + 
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

# write.png(plot, "./figures/BG.png")

plot2<- ggplot(result, aes(date, fct)) +
  geom_line(size = .5) +
  geom_ribbon(aes(date, ymin = low80, ymax = up80), alpha = .12) +
  geom_ribbon(aes(date, ymin = low95, ymax = up95), alpha = .08) +
  geom_line(aes(date, actual),  color = viridis_col[6], size = 1) +
  labs(x = element_blank(), y = element_blank(), title = "Switzerland") +
  # coord_cartesian(ylim= c(.4, .8)) +
  mytheme() + theme(plot.title = element_text(hjust = 0.5))

# write.png(plot2, "./figures/BG.png")

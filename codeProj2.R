library(dplyr)
library(lubridate)
library(tidyverse)
library(stringr)
library(ggplot2)



data<-read.csv("repdata_data_StormData.csv.bz2")

data<-read.csv("repdata_data_StormData.csv.bz2",stringsAsFactors = F)




dim(data)
head(data)
names(data)



df<-select(data,BGN_DATE,EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)
df.f<-filter(df,CROPDMG>0 | PROPDMG>0 | FATALITIES>0 | INJURIES>0)



df.f$PROPDMGEXP[(df.f$PROPDMGEXP)=="K"]<-as.numeric(10^3)
df.f$PROPDMGEXP[(df.f$PROPDMGEXP)=="M"]<-as.numeric(10^6)
df.f$PROPDMGEXP[(df.f$PROPDMGEXP)=="B"]<-as.numeric(10^9)
df.f$PROPDMGEXP[(df.f$PROPDMGEXP)=="m"]<-as.numeric(10^6)
df.f$PROPDMGEXP[(df.f$PROPDMGEXP)==""]<-as.numeric(0)
df.f$PROPDMGEXP[(df.f$PROPDMGEXP)=="+"]<-as.numeric(1)
df.f$PROPDMGEXP[(df.f$PROPDMGEXP)=="0"]<-as.numeric(10)
df.f$PROPDMGEXP[(df.f$PROPDMGEXP)=="5"]<-as.numeric(10)
df.f$PROPDMGEXP[(df.f$PROPDMGEXP)=="6"]<-as.numeric(10)
df.f$PROPDMGEXP[(df.f$PROPDMGEXP)=="4"]<-as.numeric(10)
df.f$PROPDMGEXP[(df.f$PROPDMGEXP)=="h"]<-as.numeric(100)
df.f$PROPDMGEXP[(df.f$PROPDMGEXP)=="2"]<-as.numeric(10)
df.f$PROPDMGEXP[(df.f$PROPDMGEXP)=="7"]<-as.numeric(10)
df.f$PROPDMGEXP[(df.f$PROPDMGEXP)=="3"]<-as.numeric(10)
df.f$PROPDMGEXP[(df.f$PROPDMGEXP)=="H"]<-as.numeric(100)
df.f$PROPDMGEXP[(df.f$PROPDMGEXP)=="-"]<-as.numeric(0)


df.f$CROPDMGEXP[(df.f$CROPDMGEXP)==""]<-as.numeric(0)
df.f$CROPDMGEXP[(df.f$CROPDMGEXP)=="M"]<-as.numeric(10^6)
df.f$CROPDMGEXP[(df.f$CROPDMGEXP)=="K"]<-as.numeric(10^3)
df.f$CROPDMGEXP[(df.f$CROPDMGEXP)=="m"]<-as.numeric(10^6)
df.f$CROPDMGEXP[(df.f$CROPDMGEXP)=="B"]<-as.numeric(10^9)
df.f$CROPDMGEXP[(df.f$CROPDMGEXP)=="?"]<-as.numeric(0)
df.f$CROPDMGEXP[(df.f$CROPDMGEXP)=="0"]<-as.numeric(10)
df.f$CROPDMGEXP[(df.f$CROPDMGEXP)=="k"]<-as.numeric(10^3)


df.m<-mutate(df.f,PROPDMG.2=PROPDMG*as.numeric(PROPDMGEXP),
             CROPDMG.2=CROPDMG*as.numeric(CROPDMGEXP),DATE=mdy_hms(BGN_DATE))



### remove outlier
df.g<-mutate(df.m,TOTALDMG=PROPDMG.2+CROPDMG.2,YEAR = as.POSIXlt(DATE)$year + 1900) %>%
  filter(TOTALDMG<115032500000,YEAR>1995) %>%
  arrange(desc(TOTALDMG)) 

### count number of events per year
df.y.cnt<-group_by(df.g,YEAR) %>%
  summarize(CountOfEvType=n()) %>%
  arrange(YEAR)
View(df.y.cnt)

# Consoolidate events
newevents<-case_when(
grepl("TORNADO",df.g$EVTYPE) ~ "Tornado",
grepl("FLOOD",df.g$EVTYPE) ~ "Flood",
grepl("HAIL",df.g$EVTYPE) ~ "Hail",
grepl("LIGHTNING",df.g$EVTYPE) ~ "Lightning",
grepl("HIGH WIND",df.g$EVTYPE)~ "High Wind" ,
grepl("STRONG WIND",df.g$EVTYPE)~"High Wind",
grepl("WINTER STORM",df.g$EVTYPE)~"Winter Storm",
grepl("HEAVY RAIN",df.g$EVTYPE)~"Heavy Rain" ,
grepl("HEAVY SNOW",df.g$EVTYPE)~"Heavy Snow",
grepl("WILDFIRE",df.g$EVTYPE)~"Wildfire",
grepl("URBAN/SML STREAM FLD",df.g$EVTYPE)~"Flood",
grepl("ICE STORM",df.g$EVTYPE)~"Ice Storm" ,
grepl("TSTM WIND/HAIL",df.g$EVTYPE)~"Hail",
grepl("TROPICAL STORM",df.g$EVTYPE)~"Tropical Storm",
grepl("WINTER WEATHER",df.g$EVTYPE)~"Winter Weather",
grepl("WILD/FOREST FIRE",df.g$EVTYPE)~"Wildfire",
grepl("DROUGHT",df.g$EVTYPE)~"Drought",
grepl("LAKE-EFFECT SNOW",df.g$EVTYPE)~"Lake-Effect Snow",
grepl("BLIZZARD",df.g$EVTYPE)~"Blizzard",
grepl("LANDSLIDE",df.g$EVTYPE)~"Heavy Rain",
grepl("STORM SURGE",df.g$EVTYPE)~"Storm Surge/Tide",
grepl("COASTAL FLOOD",df.g$EVTYPE)~"Coastal Flood",
grepl("WINTER WEATHER/MIX",df.g$EVTYPE)~"Winter Weather",
grepl("HURRICANE",df.g$EVTYPE)~"Hurricane (Typhoon)",
grepl("LIGHT SNOW",df.g$EVTYPE)~"Heavy Snow",
grepl("FROST/FREEZE",df.g$EVTYPE)~"Frost/Freeze",
grepl("MARINE TSTM WIND",df.g$EVTYPE)~"Marine Thunderstorm Wind",
grepl("RIVER FLOOD",df.g$EVTYPE)~"Flood",
grepl("EXTREME COLD",df.g$EVTYPE)~"Cold/Wind Chill",
grepl("DUST STORM",df.g$EVTYPE)~"Dust Storm",
grepl("DUST DEVIL",df.g$EVTYPE)~"Dust Devil",
grepl("HURRICANE/TYPHOON",df.g$EVTYPE)~"Hurricane (Typhoon)",
grepl("DRY MICROBURST",df.g$EVTYPE)~"Thunderstorm Wind",
grepl("FOG",df.g$EVTYPE)~"Dense Fog",
grepl("DENSE FOG",df.g$EVTYPE)~"Dense Fog",
grepl("AVALANCHE",df.g$EVTYPE)~"Avalanche",
grepl("HIGH SURF",df.g$EVTYPE)~"High Surf",
grepl("STORM SURGE/TIDE",df.g$EVTYPE)~"Storm Surge/Tide",
grepl("WIND",df.g$EVTYPE)~"Strong Wind",
grepl("TSTM WIND",df.g$EVTYPE)~"Thunderstorm Wind",
grepl("TROPICAL DEPRESSION",df.g$EVTYPE)~"Tropical Depression",
grepl("MARINE STRONG WIND",df.g$EVTYPE)~"Marine Strong Wind",
grepl("OTHER",df.g$EVTYPE)~"OTHER",
grepl("STRONG WINDS",df.g$EVTYPE)~"Strong Wind",
grepl("COASTAL FLOODING",df.g$EVTYPE)~"Coastal Flood",
grepl("WATERSPOUT",df.g$EVTYPE)~"Waterspout",
grepl("EXCESSIVE SNOW",df.g$EVTYPE)~"Heavy Snow",
grepl("EXCESSIVE HEAT",df.g$EVTYPE)~"Excessive Heat",
grepl("MARINE THUNDERSTORM WIND",df.g$EVTYPE)~"Marine Thunderstorm Wind",
grepl("LIGHT FREEZING RAIN",df.g$EVTYPE)~"Ice Storm",
grepl("Light Snow",df.g$EVTYPE)~"Heavy Snow",
grepl("EXTREME COLD/WIND CHILL",df.g$EVTYPE)~"xtreme Cold/Wind Chill",
grepl("HEAVY SURF",df.g$EVTYPE)~"High Surf",
grepl("ICY ROADS",df.g$EVTYPE)~"Ice Storm",
grepl("MARINE HIGH WIND",df.g$EVTYPE)~"Marine High Wind",
grepl("GUSTY WINDS",df.g$EVTYPE)~"Strong Wind",
grepl("HEAVY SURF/HIGH SURF",df.g$EVTYPE)~"High Surf",
grepl("COLD/WIND CHILL",df.g$EVTYPE)~"xtreme Cold/Wind Chill",
grepl("MIXED PRECIPITATION",df.g$EVTYPE)~"Heavy Rain",
grepl("TSUNAMI",df.g$EVTYPE)~"Tsunami",
grepl("GUSTY WIND",df.g$EVTYPE)~"Strong Wind",
grepl("FREEZE",df.g$EVTYPE)~"xtreme Cold/Wind Chill",
grepl("SNOW",df.g$EVTYPE)~"Heavy Snow",
grepl("Gusty Winds",df.g$EVTYPE)~"Strong Wind",
grepl("SMALL HAIL",df.g$EVTYPE)~"Hail",
grepl("SEICHE",df.g$EVTYPE)~"Seiche",
grepl("TSTM WIND",df.g$EVTYPE)~"Thunderstorm Wind",
grepl("TYPHOON",df.g$EVTYPE)~"Hurricane (Typhoon)",
grepl("ASTRONOMICAL HIGH TIDE",df.g$EVTYPE)~"Astronomical Low Tide",
grepl("FUNNEL CLOUD",df.g$EVTYPE)~"Funnel Cloud",
grepl("FREEZING FOG",df.g$EVTYPE)~"Freezing Fog",
grepl("FREEZING RAIN",df.g$EVTYPE)~"Frost/Freeze",
grepl("Coastal Flooding",df.g$EVTYPE)~"Coastal Flood",
grepl("Dust Devil",df.g$EVTYPE)~"Dust Devil",
grepl("HEAT",df.g$EVTYPE)~"Heat",
grepl("LAKESHORE FLOOD",df.g$EVTYPE)~"Lakeshore Flood",
grepl("RIP CURRENTS",df.g$EVTYPE)~"Rip Current",
grepl("River Flooding",df.g$EVTYPE)~"Flash Flood",
grepl("Coastal Flood",df.g$EVTYPE)~"Coastal Flood",
grepl("Cold",df.g$EVTYPE)~"Cold/Wind Chill",
grepl("EXTREME WINDCHILL",df.g$EVTYPE)~"Cold/Wind Chill",
grepl("AGRICULTURAL FREEZE",df.g$EVTYPE)~"Cold/Wind Chill",
grepl("COASTAL FLOODING/EROSION",df.g$EVTYPE)~"Coastal Flood",
grepl("Gradient wind",df.g$EVTYPE)~"Strong Wind",
grepl("LAKE EFFECT SNOW",df.g$EVTYPE)~"Lake-Effect Snow",
grepl("Mixed Precipitation",df.g$EVTYPE)~"Hail",
grepl("MUDSLIDE",df.g$EVTYPE)~"Debris Flow",
grepl("RAIN",df.g$EVTYPE)~"Heavy Rain",
grepl("SNOW SQUALLS",df.g$EVTYPE)~"Heavy Snow",
grepl("UNSEASONABLY COLD",df.g$EVTYPE)~"Cold/Wind Chill",
grepl("WET MICROBURST",df.g$EVTYPE)~"Thunderstorm Wind",
grepl("TSTM WIND",df.g$EVTYPE)~"Thunderstorm Wind",
grepl("ASTRONOMICAL LOW TIDE",df.g$EVTYPE)~"Astronomical Low Tide",
grepl("DAM BREAK",df.g$EVTYPE)~"Debris Flow",
grepl("Damaging Freeze",df.g$EVTYPE)~"Cold/Wind Chill",
grepl("Erosion/Cstl Flood",df.g$EVTYPE)~"Flash Flood",
grepl("Freeze",df.g$EVTYPE)~"xtreme Cold/Wind Chill",
grepl("Freezing Drizzle",df.g$EVTYPE)~"xtreme Cold/Wind Chill",
grepl("FREEZING DRIZZLE",df.g$EVTYPE)~"xtreme Cold/Wind Chill",
grepl("Glaze",df.g$EVTYPE)~"OTHER",
grepl("gradient wind",df.g$EVTYPE)~"High Wind",
grepl("High Surf",df.g$EVTYPE)~"High Surf",
grepl("HIGH WIND",df.g$EVTYPE)~"High Wind",
grepl("LANDSPOUT",df.g$EVTYPE)~"Debris Flow",
grepl("MARINE HAIL",df.g$EVTYPE)~"Marine Hail",
grepl("MUD SLIDE",df.g$EVTYPE)~"Debris Flow",
grepl("Snow",df.g$EVTYPE)~"Heavy Snow",
grepl("Snow Squalls",df.g$EVTYPE)~"Heavy Snow",
grepl("Strong Wind",df.g$EVTYPE)~"Strong Wind",
grepl("TIDAL FLOODING",df.g$EVTYPE)~"Storm Surge/Tide",
grepl("UNSEASONAL RAIN",df.g$EVTYPE)~"Heavy Rain",
grepl("VOLCANIC ASH",df.g$EVTYPE)~"Volcanic Ash",
grepl("	   HIGH SURF ADVISORY",df.g$EVTYPE)~"OTHER",
grepl("FLASH FLOOD",df.g$EVTYPE)~"OTHER",
grepl("TSTM WIND (G45)",df.g$EVTYPE)~"OTHER",
grepl("Beach Erosion",df.g$EVTYPE)~"OTHER",
grepl("BLOWING DUST",df.g$EVTYPE)~"OTHER",
grepl("blowing snow",df.g$EVTYPE)~"OTHER",
grepl("COASTAL  FLOODING/EROSION",df.g$EVTYPE)~"OTHER",
grepl("COASTAL EROSION",df.g$EVTYPE)~"OTHER",
grepl("Coastal Storm",df.g$EVTYPE)~"OTHER",
grepl("COLD",df.g$EVTYPE)~"OTHER",
grepl("DAMAGING FREEZE",df.g$EVTYPE)~"OTHER",
grepl("DENSE SMOKE",df.g$EVTYPE)~"OTHER",
grepl("DOWNBURST",df.g$EVTYPE)~"OTHER",
grepl("Early Frost",df.g$EVTYPE)~"OTHER",
grepl("Extended Cold",df.g$EVTYPE)~"OTHER",
grepl("Extreme Cold",df.g$EVTYPE)~"OTHER",
grepl("FLASH FLOOD/FLOOD",df.g$EVTYPE)~"OTHER",
grepl("FLOOD/FLASH/FLOOD",df.g$EVTYPE)~"OTHER",
grepl("Freezing drizzle",df.g$EVTYPE)~"OTHER",
grepl("Freezing Rain",df.g$EVTYPE)~"OTHER",
grepl("FROST",df.g$EVTYPE)~"OTHER",
grepl("Frost/Freeze",df.g$EVTYPE)~"OTHER",
grepl("GLAZE",df.g$EVTYPE)~"OTHER",
grepl("GRADIENT WIND",df.g$EVTYPE)~"OTHER",
grepl("GUSTY WIND/HAIL",df.g$EVTYPE)~"OTHER",
grepl("GUSTY WIND/HVY RAIN",df.g$EVTYPE)~"OTHER",
grepl("Gusty wind/rain",df.g$EVTYPE)~"OTHER",
grepl("HARD FREEZE",df.g$EVTYPE)~"OTHER",
grepl("Heavy Rain/High Surf",df.g$EVTYPE)~"OTHER",
grepl("Heavy snow shower",df.g$EVTYPE)~"OTHER",
grepl("Heavy Surf",df.g$EVTYPE)~"OTHER",
grepl("HIGH SEAS",df.g$EVTYPE)~"OTHER",
grepl("HIGH SWELLS",df.g$EVTYPE)~"OTHER",
grepl("HIGH WINDS",df.g$EVTYPE)~"OTHER",
grepl("Ice jam flood \\(minor",df.g$EVTYPE)~"OTHER",
grepl("ICE ROADS",df.g$EVTYPE)~"OTHER",
grepl("Lake Effect Snow",df.g$EVTYPE)~"OTHER",
grepl("LANDSLIDES",df.g$EVTYPE)~"OTHER",
grepl("Landslump",df.g$EVTYPE)~"OTHER",
grepl("LATE SEASON SNOW",df.g$EVTYPE)~"OTHER",
grepl("Light snow",df.g$EVTYPE)~"OTHER",
grepl("Light Snowfall",df.g$EVTYPE)~"OTHER",
grepl("Marine Accident",df.g$EVTYPE)~"OTHER",
grepl("Microburst",df.g$EVTYPE)~"OTHER",
grepl("NON-SEVERE WIND DAMAGE",df.g$EVTYPE)~"OTHER",
grepl("NON-TSTM WIND",df.g$EVTYPE)~"OTHER",
grepl("Other",df.g$EVTYPE)~"OTHER",
grepl("RIP CURRENT",df.g$EVTYPE)~"OTHER",
grepl("ROCK SLIDE",df.g$EVTYPE)~"OTHER",
grepl("ROUGH SURF",df.g$EVTYPE)~"OTHER",
grepl("SNOW SQUALL",df.g$EVTYPE)~"OTHER",
grepl("Strong Winds",df.g$EVTYPE)~"OTHER",
grepl("Tidal Flooding",df.g$EVTYPE)~"OTHER",
grepl("Tstm Wind",df.g$EVTYPE)~"OTHER",
grepl("TSTM WIND  (G45)",df.g$EVTYPE)~"OTHER",
grepl("TSTM WIND (41)",df.g$EVTYPE)~"OTHER",
grepl("TSTM WIND (G35)",df.g$EVTYPE)~"OTHER",
grepl("TSTM WIND 40",df.g$EVTYPE)~"OTHER",
grepl("TSTM WIND 45",df.g$EVTYPE)~"OTHER",
grepl("TSTM WIND AND LIGHTNING",df.g$EVTYPE)~"OTHER",
grepl("TSTM WIND G45",df.g$EVTYPE)~"OTHER",
grepl("Unseasonable Cold",df.g$EVTYPE)~"OTHER",
grepl("UNSEASONABLY WARM",df.g$EVTYPE)~"OTHER",
grepl("Whirlwind",df.g$EVTYPE)~"OTHER",
grepl("WHIRLWIND",df.g$EVTYPE)~"OTHER",
grepl("Wind",df.g$EVTYPE)~"OTHER",
grepl("WIND AND WAVE",df.g$EVTYPE)~"OTHER",
grepl("Wind Damage",df.g$EVTYPE)~"OTHER",
grepl("WINTER WEATHER MIX",df.g$EVTYPE)~"OTHER",
grepl("Wintry Mix",df.g$EVTYPE)~"OTHER",
grepl("WINTRY MIX",df.g$EVTYPE)~"OTHER"
)

newevents<-as.data.frame(newevents)

newdata<-cbind(df.g,newevents)

newdata<-mutate(newdata,newevents=as.character(newevents))

# In order to determine which events accross the United States are most harmful with respect to population health we combine both fatalities and injuries.

df.harm<-mutate(newdata,TotalHarm=FATALITIES+INJURIES)
df.harm2<-group_by(df.harm,newevents) %>%
  summarize(TotalHarmSum=sum(TotalHarm)) %>%
  arrange(desc(TotalHarmSum))

head(df.harm2,20)

df.harm2.f<-filter(df.harm2,TotalHarmSum>507)

ggplot(df.harm2.f,aes(newevents,TotalHarmSum))+geom_bar(stat = "identity")+ 
  theme(axis.text.x = element_text(angle = 45,hjust = 1))



par(mfrow=c(2,2))

tornado<-filter(df.harm,newevents=="Tornado") %>%
  group_by(YEAR) %>%
  summarize(TornadoHarm=sum(TotalHarm))
with(tornado,plot(YEAR,TornadoHarm,type="l"))

flood<-filter(df.harm,newevents=="Flood") %>%
  group_by(YEAR) %>%
  summarize(FloodHarm=sum(TotalHarm))
with(flood,plot(YEAR,FloodHarm,type="l"))

StrngWnd<-filter(df.harm,newevents=="Strong Wind") %>%
  group_by(YEAR) %>%
  summarize(StrngWndHarm=sum(TotalHarm))
with(StrngWnd,plot(YEAR,StrngWndHarm,type="l"))

Canes<-filter(df.harm,newevents=="Hurricane (Typhoon)") %>%
  group_by(YEAR) %>%
  summarize(HurricaneHarm=sum(TotalHarm))
with(Canes,plot(YEAR,HurricaneHarm,type="l"))









### After counting  number of events and view the top ten most common type of events and confirm Strong Wind is the most common followed by Flood and Hail.
df.g2<-group_by(newdata,newevents) %>%
  summarize(CountOfEvType=n()) %>%
  arrange(desc(CountOfEvType))
head(df.g2,10)

## Across the United Sates Hurricane, Flood, Storm Surges, and Tornados have the greatest economic impact being heavily weighted by property damage cost.
# with Hurricane causing 87 Billion dollars in Damage since 1996

df.g3<-group_by(newdata,newevents) %>%
  summarize(TOTAL_CROPDMG=sum(CROPDMG.2),TOTAL_PROPDMG=sum(PROPDMG.2),TOTAL=sum(TOTALDMG)) %>%
  arrange(desc(TOTAL))
head(df.g3,20)



ggplot(df.g3.f20,aes(newevents,TOTAL))+geom_bar(stat = "identity")+ 
  theme(axis.text.x = element_text(angle = 45,hjust = 1))


## The table below show the top 20 types of events with the gretest economic consequences broken by Crop and Property Damages

#df.g3.f20<-filter(df.g3,TOTAL >= 207835000) %>%
 # arrange(desc(TOTAL))
#head(df.g3,20)

# We see that drought has the most economic consequence for crop damages.

#p2<-ggplot(df.g3.f20,aes(newevents,TOTAL_CROPDMG))+geom_bar(stat = "identity")+ 
 # theme(axis.text.x = element_text(angle = 45,hjust = 1))

# We see that Hurricane has the most econimic consequence for property damages.
#ggplot(df.g3.f20,aes(newevents,TOTAL_PROPDMG))+geom_bar(stat = "identity")+ 
 # theme(axis.text.x = element_text(angle = 45,hjust = 1))

#  We see below that high economic impact of hurricanes are mostly driven by single devastating event.  Also we noticed that top three most impactful economic events more likely occur in coastal areas.

par(mfrow=c(2,2))

canes<-filter(newdata,newevents=="Hurricane (Typhoon)") %>%
  group_by(YEAR) %>%
  summarize(HurricaneDamage=sum(TOTALDMG))
with(canes,plot(YEAR,HurricaneDamage,type="l"))

flood<-filter(newdata,newevents=="Flood") %>%
  group_by(YEAR) %>%
  summarize(FloodDamage=sum(TOTALDMG))
with(flood,plot(YEAR,FloodDamage,type="l"))

surge<-filter(newdata,newevents=="Storm Surge/Tide") %>%
  group_by(YEAR) %>%
  summarize(StormSurgeDamage=sum(TOTALDMG))
with(surge,plot(YEAR,StormSurgeDamage,type="l"))

tornado<-filter(newdata,newevents=="Tornado") %>%
  group_by(YEAR) %>%
  summarize(TornadoDamage=sum(TOTALDMG))
with(tornado,plot(YEAR,TornadoDamage,type="l"))








#df.g2<-mutate(df.g, EVTYP2=replace(EVTYPE,,"High Surf")
#df.g2<-mutate(df.g, EVTYP2=replace(EVTYPE,str_detect(EVTYPE,regex("SURF",ignore_case = TRUE)),"High Surf"),
 #             EVTYP2=replace(EVTYPE,str_detect(EVTYPE,regex("SEAS",ignore_case = TRUE)),"High Surf"))


#df.g2<-mutate(df.g, EVTYP2=replace(EVTYPE,str_detect(EVTYPE,regex("SURF",ignore_case = TRUE)),"High Surf") | 
#             replace(EVTYPE,str_detect(EVTYPE,regex("SEAS",ignore_case = TRUE)),"High Surf"))


#df.g2<-mutate(df.g,EVTYP2=replace(EVTYPE,str_detect(EVTYPE,regex("SEAS$",ignore_case = TRUE)),"High Surf"))

  
  
  



 
  
##According to NOAA the data recording start from Jan. 1950. At that time they recorded one event type, tornado. They add more events gradually and only from Jan. 1996 they start recording all events type. Since our objective is comparing the effects of different weather events, do we need to include all years, even it has only single event type?

### count number of events per year
df.y.cnt<-group_by(df.m2,YEAR) %>%
  summarize(CountOfEvType=n()) %>%
  arrange(desc(YEAR))



df.s<-summarize(df.g,TotalPropDmg=sum(PROPDMG.2),TotalCropDmg=sum(CROPDMG.2),TotalDmg=sum(TOTALDMG)) %>%
  arrange(desc(TotalDmg))





flood<-filter(df.m2,EVTYPE=="FLOOD") %>%
  group_by(YEAR) %>%
  summarize(TotalDmgFlood=sum(TOTALDMG))

with(flood,plot(YEAR,TotalDmgFlood))


##########

#Based on remarks it appears damages was down in milliiona of dollars rather than billions so there is appears to be a data entry entry error for the exponenientil values of this event.
#Major flooding continued into the early hours of January 1st, before the Napa River finally fell below flood stage and the water receeded. Flooding was severe in Downtown Napa from the Napa Creek and the City and Parks Department was hit with $6 million in damage alone. The City of Napa had 600 homes with moderate damage, 150 damaged businesses with costs of at least $70 million.	
# for entry we 

flood2<-select(data,BGN_DATE,REMARKS,EVTYPE,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP) %>%
  filter(EVTYPE=="STORM SURGE") %>%
  mutate(DATE=mdy_hms(BGN_DATE)) %>%
  mutate(YEAR = as.POSIXlt(DATE)$year + 1900) %>%
  filter(YEAR==2005)
  


which.max(activity.2$StepsInt5min)
activity.2$interval[which.max(activity.2$StepsInt5min)]



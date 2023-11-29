rm(list=ls())

# Load libraries
library(data.table)
library(readr)
library(tidyverse) # includes ggplot2, dplyr, tidyr, readr, etc.
library(here)

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

################# Test each parameter

load(here("Data/Canada_Clean/TDatabase_1996_2001.RData"))
load(here("Data/Canada_Clean/TDatabase_2002_2022.RData"))

#names(TDatabase_1996_2001)
#names(TDatabase_2002_2022)

Database_1996_2001=data.table(TDatabase_1996_2001)
Database_2002_2022=data.table(TDatabase_2002_2022)

#names(TDatabase_2002_2022)
#names(TDatabase_1996_2001)

###################### Selected variables and merge the two databases.

DATA1 = Database_2002_2022[YEAR>=2002,.(YEAR,MONTH,DOY,date, 
                                        lat,lon,
                                        Effort_2,COUNTb,
                                        FLEET,GEAR ,NAFO_Pos,
                                        VR_NUMBER,MON_DOC_ID,
                                        BFT_LANDED_WEIGHT_LBS,weight_kg  ,
                                        SST)]

names(DATA1)= c("Year" , "Month", "Jday", "Fishing_date",
                "latitude" , "longitude", 
                "Effort_Nday", "COUNT_bft_per_trip",
                "HMA","Gear","NAFO",
                "Vessel_ID","Trip_ID",
                "WEIGHT_LBS" , "WEIGHT_KG" ,  
                "SST")

DATA2 = Database_1996_2001[YEAR < 2002 ,.(YEAR,MONTH,DOY,date,
                                          lat,lon,
                                          Effort_2,TOTAL_NUM_BFT_CAUGHT_TRIP3,
                                          FLEET,GEAR,NAFO_Pos,
                                          VR_NUMBER,TRIP_ID,
                                          BFT_LANDED_WEIGHT_LBS,weight_kg, 
                                          SST)]

names(DATA2)= c("Year" , "Month", "Jday", "Fishing_date",
                "latitude" , "longitude", 
                "Effort_Nday", "COUNT_bft_per_trip",
                "HMA","Gear", "NAFO",
                "Vessel_ID","Trip_ID",
                "WEIGHT_LBS" , "WEIGHT_KG",  
                "SST")

### Final filter
DATA = rbind.data.frame(DATA1,DATA2)
DATA=data.table(DATA)
DATA = DATA[Year>1995]
DATA = DATA[COUNT_bft_per_trip<=12]
DATA = DATA[Gear%in%c("RRt", "TL", "HARP", "RR")]
DATA[Gear=="RRt",Gear:="RR"]
#DATA = DATA[Jday<350 & Jday>180]
DATA$Month <- lubridate::month(DATA$Fishing_date)
DATA =DATA[Month>=6 & Month <=10]
DATA = DATA[Effort_Nday <=1]

DATA[HMA%in%c("UNK"), HMA := "SF"]
# Housekeeping on Fleet-Area combinations
DATA = DATA[!(NAFO=="4XSR"&!HMA=="SF")]
DATA = DATA[!(HMA=="PQ"&as.numeric(as.character(Year))>2010)]
DATA = DATA[!(NAFO=="4XNP"& HMA=="GNB"& Year>=2017)]
DATA[NAFO=="5ZU", HMA := "SF"]
DATA = DATA[!(NAFO=="4XO"&HMA%in%c("PQ","PEI"))]
DATA[,HMA := factor(HMA)]
# combine areas
DATA[,Gear := factor(Gear)]
DATA[,FLEET:= factor(ifelse(HMA=="SF","Scotia","Other"))]
DATA[,LogEffort := log(Effort_Nday)]

### USE DATA to add other variables on fishing location before creating 
### database per size class.

### Define size based on SCRS 2014/039 used in DFO for CANADA
# Small: <272 kgs
# Medium: 272-362 kgs  
# Large: >362 kgs

# DATA$Weight_Class=cut(DATA$WEIGHT_KG, breaks = c(15, 61.235, 362, 600),
#                                      labels = c("Small", "Medium", "Large"))  

DATA$Weight_Class=cut(DATA$WEIGHT_LBS, breaks = c(0, 135, 235, 1252),
                                       labels=c('Small', 'Medium', 'Large'))

DATA$Caught=0

DATA$Caught[DATA$Weight_Class=="Small"| 
              DATA$Weight_Class=="Medium"| 
              DATA$Weight_Class=="Large"]=1

#### Create database per trip
mode_qualitatif <- function(vecteur) {
  tab_freq <- table(vecteur)
  modes <- as.character(names(tab_freq)[tab_freq == max(tab_freq)])
  return(modes[1])  # Take only the first mode
}



Database_global= DATA %>% 
  group_by(Trip_ID,  Vessel_ID, latitude, longitude) %>%
  dplyr::summarize( Year=first(Year), 
                    Month=first(Month),
                    Jday= first(Jday),  
                    #latitude=mean(latitude), 
                    #longitude=mean(longitude), 
                    Effort=first(Effort_Nday),
                    COUNT_bft_per_trip=mean(COUNT_bft_per_trip),
                    NAFO=mode_qualitatif(NAFO), 
                    HMA= mode_qualitatif(HMA), 
                    Gear=mode_qualitatif(Gear), 
                    SST=mean(SST))


Database_weight_class= DATA %>% 
  group_by(Trip_ID,  Vessel_ID, Weight_Class, latitude, longitude) %>%
  dplyr::summarize( Year=first(Year), 
                    Month=first(Month),
                    Jday= first(Jday), 
                    #latitude=mean(latitude), 
                    #longitude=mean(longitude), 
                    Effort=first(Effort_Nday),
                    COUNT_bft_per_trip=first(COUNT_bft_per_trip),
                    COUNT_for_size=sum(Caught, na.rm = T),   
                    NAFO=mode_qualitatif(NAFO), 
                    HMA= mode_qualitatif(HMA), 
                    Gear=mode_qualitatif(Gear), 
                    SST=mean(SST))


######################## Edit and save

dbw <- as.data.frame(Database_weight_class)

dbw <- dbw %>% 
  mutate_at(c('NAFO', 'HMA', 'Gear'), as.factor) %>% 
  mutate(spacetime = paste0(Trip_ID, '-', latitude, longitude,
                            '-', Year, '-', Jday))

# Every space-time needs a row for each size class
dbw$Weight_Class <- as.character(dbw$Weight_Class)
trips <- split(dbw, f=dbw$spacetime)
sizes <- c('Small', 'Medium', 'Large')

for(i in 1:length(trips)){
  print(i)
  # One row and it's no fish
  if(nrow(trips[[i]]) == 1 &
     trips[[i]]$COUNT_for_size[1] == 0){
    trips[[i]] <- rbind(trips[[i]],
                        trips[[i]],
                        trips[[i]])
    trips[[i]]$Weight_Class <- c('Small', 'Medium', 'Large')
    trips[[i]]$COUNT_for_size <- c(0,0,0)
    trips[[i]]$COUNT_for_spacetime <- sum(trips[[i]]$COUNT_for_size)
    next()
  }
  # One row and it's all one size
  if(nrow(trips[[i]]) == 1 &
     trips[[i]]$COUNT_bft_per_trip[1] != 0){
    trips[[i]] <- rbind(trips[[i]], 
                        trips[[i]],
                        trips[[i]])
    trips[[i]]$Weight_Class[2:3] <- sizes[sizes %notin%
                                          trips[[i]]$Weight_Class]
    trips[[i]]$COUNT_for_size[2:3] <- 0
    trips[[i]]$COUNT_for_spacetime <- sum(trips[[i]]$COUNT_for_size)
    next()
  }
  # Two rows and it's one size plus no fish
  if(nrow(trips[[i]]) == 2 &
     0 %in% trips[[i]]$COUNT_for_size){
    trips[[i]] <- rbind(trips[[i]],
                       trips[[i]][1,])
    trips[[i]]$Weight_Class[2:3] <- sizes[sizes %notin%
                                           trips[[i]]$Weight_Class]
    trips[[i]]$COUNT_for_size[2:3] <- 0
    trips[[i]]$COUNT_for_spacetime <- sum(trips[[i]]$COUNT_for_size)
    next()
  }
  # Two rows and it's two sizes
  if(nrow(trips[[i]])==2 &
     0 %notin% trips[[i]]$COUNT_for_size){
    trips[[i]] <- rbind(trips[[i]],
                       trips[[i]][1,])
    trips[[i]]$Weight_Class[3] <- sizes[sizes %notin%
                                         trips[[i]]$Weight_Class]
    trips[[i]]$COUNT_for_size[3] <- 0
    trips[[i]]$COUNT_for_spacetime <- sum(trips[[i]]$COUNT_for_size)
    next()
  }
  # Three rows and it's three sizes
  if(nrow(trips[[i]]) == 3 &
     0 %notin% trips[[i]]$COUNT_for_size){
    trips[[i]]$COUNT_for_spacetime <- sum(trips[[i]]$COUNT_for_size)
    next()
  }
  # Three rows and it's two sizes and one no fish
  if(nrow(trips[[i]]) == 3 &
     0 %in% trips[[i]]$COUNT_for_size){
    trips[[i]]$Weight_Class[3] <- sizes[sizes %notin%
                                         trips[[i]]$Weight_Class]
    trips[[i]]$COUNT_for_size[3] <- 0
    trips[[i]]$COUNT_for_spacetime <- sum(trips[[i]]$COUNT_for_size)
    next()
  }
}

test <- do.call(rbind, trips)
rownames(test) <- NULL

test$spacetime <- as.factor(as.numeric(as.factor(test$spacetime)))
test <- test %>% 
  mutate_at(c("Trip_ID", 'spacetime'), as.numeric) %>% 
  dplyr::select(-COUNT_bft_per_trip) %>% 
  factor(Weight_Class, levels=c('Small', "Medium", 'Large'))

dwa <- test[with(test, order(Year, Jday, Trip_ID, spacetime)),]
rownames(dwa) <- NULL

write.csv(dwa,
          here("Data/Canada_Clean/compiled_clean_canada.csv"),
          row.names = F)

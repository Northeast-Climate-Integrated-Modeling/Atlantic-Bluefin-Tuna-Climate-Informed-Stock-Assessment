wd=paste0('C:/Users/klankowicz/Documents/GitHub/',
          'Atlantic-Bluefin-Tuna-Climate-Informed-Stock-Assessment/',
          'Data/LPS/ALL')

setwd(wd)
dat=read.csv('LPS_trip_level_0221.csv')
head(dat)


## LARGE MEDIUM AND GIANT ANALYSIS DATASET 

# FILTER FOR BLUEFIN TARGETED TRIPS, 
# JULY to OCTOBER, PRIVATE AND CHARTER VESSELS ONLY, 
# FISHED BETWEEN 1 and 24 HRS 
filter_bft=subset(dat,
                  #month%in%c(6:10)&
                    prim_op%in%c(1,2)&
                    #stcode%in%c(9,10,23,24,25,33,34,36,44,51)&
                    fhours>0.99&
                    fhours<24.01)
large_bft=subset(filter_bft,
                 prim1%in%c(4671,4679)|
                   prim2%in%c(4671,4679))

as.matrix(table(large_bft$year))

#SEASONAL CLOSURES
large_bft$closure=NA
closure=function(year,month,day)
	{
	if(year==2002)
		{
		return(ifelse(month%in%c(6:10),'open',
		ifelse(month==11&day%in%c(1:25),'open',
		ifelse(month==12&day%in%c(1:15),'open','closed'))))
		}
    if(year==2003)
        {
        return(ifelse(month%in%c(6:10),'open',
            ifelse(month==11&day%in%c(1:11),'open',
            ifelse(month==12&day%in%c(1:9),'open','closed'))))
        }
    if(year==2004) 
        {
        return(ifelse(month%in%c(6:10),'open',
            ifelse(month==11&day%in%c(1:18),'open',
            ifelse(month==12&day%in%c(8:20),'open','closed'))))
        }
    if(year==2005)
        {
        return(ifelse(month%in%c(6:11),'open',
            ifelse(month==12&day%in%c(1,5:8,12:22,26:29,31),'open','closed')))
        }
    if(year==2006)
        {
        return(ifelse(month%in%c(6:12),'open','closed'))
        }
    if(year==2007)
        {
        return(ifelse(month%in%c(6:12),'open','closed'))
        }
    if(year==2008)
        {
        return('open')
        }
    if(year==2009)
        {
        return(ifelse(month%in%c(1,6:12),'open','closed'))
        }
    if(year==2010)
        {
        return(ifelse(month%in%c(1,6:12),'open','closed'))
        }
    if(year==2011)
        {
        return(ifelse(month%in%c(1,6:12),'open','closed'))
        }
    if(year==2012)
        {
        return(ifelse(month%in%c(6:12),'open',
            ifelse(month==1&day<23,'open','closed')))
        }
    if(year==2013)
        {
        return(ifelse(month%in%c(2,6:12),'open',
            ifelse(month==1&day<16,'open','closed')))
        }
	if(year==2014)
        {
        return(ifelse(month%in%c(1:3,6:12),'open','closed'))
        }
	if(year==2015)
		{
        return(ifelse(month%in%c(1:3,6:12),'open','closed'))
        }
	if(year==2016)
		{
        return(ifelse(month%in%c(1:3,6:12),'open','closed'))
        }
	if(year==2017)
		{
        return(ifelse(month%in%c(1:3,6:12),'open','closed'))
        }
	if(year==2018)
		{
        return(ifelse(month%in%c(1:3,6:12),'open','closed'))
        }
	}
large_bft$closure=sapply(1:length(large_bft[,1]),function(i)closure(large_bft$year[i],large_bft$month[i],large_bft$day[i]))

write.csv(with(large_bft,cbind(id,year,month,day,stcode,ppstate,latddmm,londdmm,prim1,prim2,prim_op,todayt,category,tourn,miles,depth,sst,
	hook,lines,fhours,bt_live,bt_art,bt_dead,fm_troll,fm_chunk,fm_chum,fm_other,closure,large_med_bft,giant_bft))
		,"lgmd_giant_bft_analysis_0221.csv",row.names=FALSE)


### SMALL BFT ANALYSIS DATASET

# FILTER FOR TARGETED TRIPS, JUNE to SEPTEMBER
filter_bft_2=subset(dat,month%in%c(6,7,8,9, 10)&
                      stcode%in%c(9,10,23, 24,25,33, 34,36,44,51)&
                      prim_op%in%c(1,2)&
                      fhours>0.99&fhours<24.01)

small_bft=subset(filter_bft_2,prim1%in%c(4670,4672,4673,4676,4677,4678)|prim2%in%c(4670,4672,4673,4676,4677,4678))
small_bft$latdeg=substr(small_bft$latddmm,1,2)
small_bft$latdeg=ifelse(small_bft$latdeg==99,"NA",small_bft$latdeg)
small_bft$londeg=substr(small_bft$londdmm,1,2)
small_bft$londeg=ifelse(small_bft$londeg==99,"NA",small_bft$londeg)
small_bft$area=ifelse(small_bft$latdeg<39,"south",
					ifelse(small_bft$latdeg>38,"north","NA"))

school_bft=with(small_bft,cbind(id,year,month,day,stcode,ppstate,latddmm,londdmm,prim1,prim2,prim_op,todayt,category,tourn,miles,depth,sst,
	hook,lines,fhours,bt_live,bt_art,bt_dead,fm_troll,fm_chunk,fm_chum,fm_other,school_bft))
write.csv(school_bft,"school_bft_analysis_0221.csv")

large_school_bft=with(small_bft,cbind(id,year,month,day,stcode,ppstate,latddmm,londdmm,latdeg,londeg,area,prim1,prim2,prim_op,todayt,category,tourn,miles,depth,sst,
	hook,lines,fhours,bt_live,bt_art,bt_dead,fm_troll,fm_chunk,fm_chum,fm_other,large_school_bft))

write.csv(large_school_bft,"large_school_bft_analysis_0221.csv")





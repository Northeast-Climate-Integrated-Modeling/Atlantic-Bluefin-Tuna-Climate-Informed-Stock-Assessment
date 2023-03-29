wd='C:/YEAR2020/BFT/LPS/ALL/'
setwd(wd)
library(dplyr)

### DATA SECTION
filepath=wd
yearly=2002:2010
monthly=2011:2018
catch_files=sort(c(paste('catch_',yearly,'.csv',sep=''),paste('catch_',rep(monthly,length(6:10)),rep(c('06','07','08','09','10'),length(monthly)),'.csv',sep='')))
catches=lapply(catch_files,function(x){read.csv(file=x,header=T)})
main_files=sort(c(paste('main_',yearly,'.csv',sep=''),paste('main_',rep(monthly,length(6:10)),rep(c('06','07','08','09','10'),length(monthly)),'.csv',sep='')))
mains=lapply(main_files,function(x){read.csv(file=x,header=T)})

### PROCEDURE SECTION
# MERGE INTO A SINGLE CATCH DATASET
catch_data=Reduce(function(x,y){merge(x,y,all.x=TRUE,all.y=TRUE)},catches)
catch_data$kept[is.na(catch_data$kept)]=0
catch_data$alive[is.na(catch_data$alive)]=0
catch_data$dead[is.na(catch_data$dead)]=0
catch_data$catch_n=catch_data$kept+catch_data$alive+catch_data$dead
catches=aggregate(catch_n~id+stcode+docno+year+month+species,data=catch_data,sum)
#head(catches)
length(catches[,1]) #89641 with 2002-18 data

# MERGE INTO A SINGLE MAIN DATASET
main=Reduce(function(x,y){merge(x,y,all.x=TRUE,all.y=TRUE)},mains)
#head(main)
length(main[,1])

# MERGE MAIN AND CATCH DATASETS
dat=merge(main,catches,all.x=TRUE)  #,all.y=TRUE) #this commented out part includes catches with no associated main record, 2002-2017 included 15 samples with 6 total bluefin
length(dat[,1])
#head(dat)
write.csv(dat,"compiled_data/LPS_compiled_0218.csv")

# AGGREGATE CATCHES OF HMS TO SAMPLE LEVEL
agg = group_by(dat, id,year,month,day,inttime,county,stcode,siteno,site_no,sitetype,docno,location,sitetype,
	prim_op,todayt,category,ppstate,ppstfips,returnt,prim1,prim2,tourn,tourn_name,location,tcode,hook,hook_oth,lines,fhours,bt_live,bt_art,bt_dead,
	fm_troll,fm_chunk,fm_chum,fm_other,fm_other_oth,party,latddmm,londdmm,miles,depth,sst)
sum = summarise(agg, 
	young_school_bft=sum(catch_n[species%in%c(4673)],na.rm=TRUE),
	school_bft=sum(catch_n[species%in%c(4677)],na.rm=TRUE),
	large_school_bft=sum(catch_n[species%in%c(4678)],na.rm=TRUE),
	small_med_bft=sum(catch_n[species%in%c(4676)],na.rm=TRUE),
	large_med_bft=sum(catch_n[species%in%c(4679)],na.rm=TRUE),
	giant_bft=sum(catch_n[species%in%c(4671)],na.rm=TRUE),
	unk_bft=sum(catch_n[species%in%c(4670)],na.rm=TRUE),
	school_or_lgschool_bft=sum(catch_n[species%in%c(4672)],na.rm=TRUE),
	bum=sum(catch_n[species%in%c(2171)],na.rm=TRUE),
	sai=sum(catch_n[species%in%c(3026)],na.rm=TRUE),
	whm=sum(catch_n[species%in%c(2161)],na.rm=TRUE),
	lsp=sum(catch_n[species%in%c(4010)],na.rm=TRUE),
	rsp=sum(catch_n[species%in%c(4009)],na.rm=TRUE),
	swo=sum(catch_n[species%in%c(4328)],na.rm=TRUE),
	bet=sum(catch_n[species%in%c(4691)],na.rm=TRUE),
	alb=sum(catch_n[species%in%c(4701)],na.rm=TRUE),
	yft=sum(catch_n[species%in%c(4711)],na.rm=TRUE),
	skj=sum(catch_n[species%in%c(4661)],na.rm=TRUE),
	knm=sum(catch_n[species%in%c(1940)],na.rm=TRUE),
	spm=sum(catch_n[species%in%c(3840)],na.rm=TRUE)
	)

write.csv(sum,file="LPS_trip_level_0218.csv",row.names=FALSE)
       "prim1"                  "prim2"

       fhours
      round(  tapply(sum$fhours, list(sum$year,sum$stcode),    mean)   ,2)
         tapply(sum$large_school_bft, list(sum$year,sum$stcode),    mean)
          tapply(sum$small_med_bft, list(sum$year,sum$stcode),    mean)
             tapply(sum$small_med_bft, list(sum$year,sum$stcode),    mean)
             plot(-sum$londdmm , sum$latddmm  )

   names(sum)
head(sum)
tapply(sum$swo, sum$year,    mean)
   table(sum$prim1,  sum$year,sum$stcode )
     table(sum$prim1 ,sum$year )
     table(sum$prim2 ,sum$year )

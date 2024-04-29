# Process LPS data to contain only L-M and G BFT
rm(list=ls())

# Negate function
'%notin%' <- function(x,y)!('%in%'(x,y))

# Packages
library(dplyr)
library(here)
library(ggplot2)
library(gridExtra)

# Set wd
wd=here('Data/LPS/ALL')
setwd(wd)

# Set GGplot auto theme
theme_set(theme(panel.grid.major = element_line(color='lightgray'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(color='black', linewidth=1, fill=NA),
                legend.position = "bottom",
                axis.text.x=element_text(size=12),
                axis.text.y=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14, angle=90, vjust=2),
                plot.title=element_text(size=14, hjust = 0, vjust = 1.2),
                plot.caption=element_text(hjust=0, face='italic', size=12)))

# Create dataframe of state codes
statecodes <- data.frame(
  stcode = c(9,10,23,24,25,33,34,36,44,51),
  state = c('CT', 'DE', 'ME', 'MD', 'MA', 'NH', 'NJ', 'NY', 'RI', 'VA')
)

# Create dataframe of species size codes
speccode <- data.frame(
  prim = c(4673, 4677, 4678, 4676, 
           4679, 4671, 4670, 4672
  ),
  size = c('young school', 'school', 'large school', 'small med',
           'large med', 'giant', 'unknown', 'school or large school'
  ),
  sizecat = c('Discard', 'Small', 'Small', 'Medium',
              'Large', 'Large', 'Discard', 'Small')
)

# ### DATA SECTION - RUN ONCE
# filepath=wd
# yearly=2002:2010
# monthly=2011:2022
# catch_files=sort(c(paste('catch_',yearly,'.csv',sep=''),paste('catch_',rep(monthly,length(6:10)),rep(c('06','07','08','09','10'),length(monthly)),'.csv',sep='')))
# catches=lapply(catch_files,function(x){read.csv(file=x,header=T)})
# main_files=sort(c(paste('main_',yearly,'.csv',sep=''),paste('main_',rep(monthly,length(6:10)),rep(c('06','07','08','09','10'),length(monthly)),'.csv',sep='')))
# mains=lapply(main_files,function(x){read.csv(file=x,header=T)})
# size_files=sort(c(paste('size_',yearly,'.csv',sep=''),paste('size_',rep(monthly,length(6:10)),rep(c('06','07','08','09','10'),length(monthly)),'.csv',sep='')))
# sizes=lapply(size_files,function(x){read.csv(file=x,header=T)})
# 
# ### PROCEDURE SECTION
# # MERGE INTO A SINGLE CATCH DATASET
# catch_data=Reduce(function(x,y){merge(x,y,all.x=TRUE,all.y=TRUE)},catches)
# tempcatch <- catch_data
# 
# # MERGE INTO A SINGLE MAIN DATASET
# main=Reduce(function(x,y){merge(x,y,all.x=TRUE,all.y=TRUE)},mains)
# tempmain <- main
# 
# # MERGE INTO A SINGLE SIZE DATASET
# size=Reduce(function(x,y){merge(x,y,all.x=TRUE,all.y=TRUE)},sizes)
# tempsize <- size
# 
# # Remove intermediates
# rm(catches, mains, sizes, catch_files, filepath, main_files,
#    monthly, size_files, wd, yearly)

# # Save
# save.image(here('Merge_Cheat.RData'))

# Load
load(here('Merge_Cheat.RData'))

# Save og copies
catch_data_old <- catch_data
main_old <- main
size_old <- size

# Remove records ocatch_data# Remove records outside of states of interest
catch_data <- subset(catch_data, stcode %in% statecodes$stcode)
main <- subset(main, stcode %in% statecodes$stcode)
size <- subset(size, stcode %in% statecodes$stcode)

# Remove catch and size records of non-tuna
catch_data <- subset(catch_data, species %in% speccode$prim)
size <- subset(size, species %in% speccode$prim)

# Remove main records not targeting small tuna
main.1 <- subset(main, prim1 %in% speccode$prim[speccode$sizecat == "Small"])
main.2 <- subset(main, prim2 %in% speccode$prim[speccode$sizecat == "Small"])
target.main <- rbind(main.1, main.2)
target.main <- unique(target.main)
rm(main.1, main.2)
target.main <- target.main %>% 
  filter(prim_op %in% c(1,2,3)) %>% 
  filter(fhours>=1 & fhours<=24)

# Remove extraneous fields
catch_data <- dplyr::select(catch_data,
                            
                            id, tracker, year, month, stcode,
                            control, docno, intcode, cluster, 
                            sitetype, site_no, measur,
                            
                            species, kept, alive, dead)

# Remove catch data of non-small tuna
catch_data <- catch_data %>% 
  filter(species %in% speccode$prim[speccode$sizecat == 'Small'])

catch_data <- unique(catch_data)

# COunt number of small tuna caught
catch_data$kept[is.na(catch_data$kept)]=0
catch_data$alive[is.na(catch_data$alive)]=0
catch_data$dead[is.na(catch_data$dead)]=0
catch_data$catch_n <- catch_data$kept +
  catch_data$alive + 
  catch_data$dead

# Remove extraneous
catch_data <- dplyr::select(catch_data,
                            id, tracker, year, month, stcode, species, 
                            catch_n)
# Remove extraneous
catch_data <- dplyr::select(
  catch_data, id, year, month, stcode, species, catch_n
)

catch_data=aggregate(catch_n~
                       id+year+month+stcode+species,
                     data=catch_data,
                     sum)
catch_data <- catch_data[with(catch_data,
                              order(year, month, stcode, id)),]
rownames(catch_data) <- NULL
head(catch_data)

nrow(catch_data[catch_data$id %notin% target.main$id,])

size <- dplyr::select(size,
                      
                      id, tracker, year, month, stcode,
                      control, docno, intcode, cluster,
                      sitetype, site_no, measur,
                      
                      day, species, length, curve, prep)
# Do not use unique: mult fish can be same size

# Check data
catch_data <- catch_data %>% 
  rename(catch_species = species) %>% 
  mutate(from.catch = 'catch')

size <- size %>% 
  rename(size_species = species) %>% 
  mutate(from.size = 'size')

## Catch Data assessment
catch.but.no.size <- catch_data[catch_data$id %notin% size$id,]
catch.but.no.main <- catch_data[catch_data$id %notin% tempmain$id,]
catch.but.not.targeted <- catch_data[catch_data$id %notin% target.main$id,]
# There are 7,373 records of small ABFT being caught, representing 19,118 individuals
# Of these, 3,855 records representing 9,356 are missing some sort of information
# or are from trips that did not target large ABFT
## No records do not have an entry in "main" dataframe (missing location)
## Finally, 4,501 records (12,538 fish) do not have size information
# I have to drop the records that come from trips that did not target smalls
# But I can keep records that do not have size info.
catch.targeted <- catch_data[catch_data$id %in% target.main$id,]
# This leaves us with 2095 records (9762 fish)
rm(catch.but.no.main, catch.but.no.size,
   catch.but.not.targeted)

## Size data assessment
size.but.no.catch <- size[size$id %notin% catch_data$id,]
size.but.no.main <- size[size$id %notin% main$id,]
size.but.not.targeted <- size[size$id %notin% target.main$id,]
# There are 4976 records of ABFT size (4976 fish) indicating fish SFL, CFL/CPL, and disposition
# Of these, 3180 records are missing some soft of information or are from trips 
# that did not target small ABFT.
## 4 do not have an entry in "main" dataframe (missing location)
## Another 3176 records come from trips that did not target small ABFT
## Finally, 1340 records have no associated catch data
## This is mostly due to incorrect species size entry in the catch records
# I have to drop the records that come from trips that did not target smalls AND
# records that do not have main information
# But I can re-assign and keep the records that didn't have catch info and assume they caught fish.
size.targeted <- size[size$id %in% target.main$id,]
rm(size.but.no.catch, size.but.no.main, size.but.not.targeted)
# This contains that one record
# The next step here is to make sure the species catgory assigns fish to the right size
# I've found that this isn't always the case
# We'll use "length" (SFL) as the primary method of assignment, given the fish is whole or gutted
# If the fish is headed, we'll have to use "curve" (PCFL)
# Possible to use "length" if it refers to PFL? Has to make sense though.
size.categories <- read.csv(here('Background_Info/BFT_size_information.csv'))
# Conversion functions
SFL_to_CFL = function(x){
  (-0.8319 + 1.0314*x)
}
CFL_to_SFL = function(x){
  (1.85746 + 0.9606*x)
}
PCFL_to_CFL = function(x){
  (x * 1.35)
}

# Fish disposition (Prep) 0 = whole, 2 = gutted, 6=headed (PFL taken), 7=skipped
table(size.targeted$prep)
# Check SFL of whole fish
size.targeted$given.spec[size.targeted$size_species %in% c(4677, 4678, 4672)] <- 'Small'
size.targeted$given.spec[size.targeted$size_species == 4673] <- 'Young school'
size.targeted$given.spec[size.targeted$size_species == 4676] <- 'Small medium'
size.targeted$given.spec[size.targeted$size_species %in% c(4679, 4671)] <- 'Large'

size.targeted$length[size.targeted$length > 9990] <- 0
size.targeted$length[is.na(size.targeted$length)] <- 0
size.targeted$curve[size.targeted$curve > 9990] <- 0
size.targeted$curve[is.na(size.targeted$curve)] <- 0

disposition <- split(size.targeted, f=size.targeted$prep)
names(disposition) <- c('Whole','Gutted', 'Skip')

for(i in 1:length(disposition)){
  p1 <- ggplot(disposition[[i]]) +
    geom_histogram(aes(x=length), binwidth = 50) +
    geom_vline(xintercept=c(660, 1440), col='red') +
    xlim(0,3500) +
    #ylim(0,45) +
    facet_wrap(vars(given.spec)) +
    xlab('Length (SFL)') +
    ylab('Count')
  
  p2 <- ggplot(disposition[[i]]) +
    geom_histogram(aes(x=curve), binwidth = 50)+
    geom_vline(xintercept=c(690, 1500), col='blue')+ # CFL
    #geom_vline(xintercept=1380, col='green') + #PFCFL
    xlim(0,3500) +
    #ylim(0,45)+
    facet_wrap(vars(given.spec)) +
    xlab('Length (CFL or PCFL)') +
    ylab('Count')
    
    print(grid.arrange(p1, p2, nrow=2#,
                       #top=textGrob(
                         #paste0(names(disposition)[i]),
                         #gp=gpar(fontsize=15, font=3)
                         )
                       )
          #)
    rm(p1, p2)
}
# There is one "Small, Whole" fish that is not long enough (either SFL or CFL), discard them
size.targeted$flag[size.targeted$given.spec == 'Small' & size.targeted$prep == 0 &
                     size.targeted$length <660 & size.targeted$curve < 690 ] <- 'changed small to young school'
size.targeted$size_species[size.targeted$given.spec == 'Small' & size.targeted$prep == 0 &
                     size.targeted$length <660 & size.targeted$curve < 690 ] <- 4673
size.targeted$given.spec[size.targeted$given.spec == 'Small' & size.targeted$prep == 0 &
                size.targeted$length <660 & size.targeted$curve < 690 ] <- 'Young school'

# Check again
disposition <- split(size.targeted, f=size.targeted$prep)
names(disposition) <- c('Whole', 'Gutted', 'Headed')

for(i in 1:length(disposition)){
  disposition[[i]]$curve[disposition[[i]]$curve==0] <- NA
  disposition[[i]]$length[disposition[[i]]$length==0] <- NA  
  
  p1 <- ggplot(disposition[[i]]) +
    geom_histogram(aes(x=length), binwidth = 10) +
    geom_vline(xintercept=c(660, 1440), col='red') +
    #xlim(350,3200)+
    facet_wrap(vars(given.spec),
               scales="free") +
    xlab('Length (SFL)') +
    ylab('Count')
  
  p2 <- ggplot(disposition[[i]]) +
    geom_histogram(aes(x=curve), binwidth = 10)+
    geom_vline(xintercept=c(690, 1500), col='blue')+ # CFL
    #geom_vline(xintercept=1380, col='green') + #PFCFL
    #xlim(700,3200)+
    facet_wrap(vars(given.spec),
               scales="free") +
    xlab('Length (CFL or PCFL)') +
    ylab('Count')
  
  print(grid.arrange(p1, p2, nrow=2#,
                     #top=textGrob(
                      # paste0(names(disposition)[i]),
                      # gp=gpar(fontsize=15, font=3)
                     #)
  )
  )
  rm(p1, p2)
}
rm(disposition)

## Merge
size.targeted$length[size.targeted$length==0] <- NA
size.targeted$curve[size.targeted$curve == 0] <- NA

size.targeted$calc.sfl[size.targeted$prep == 0] <-
  CFL_to_SFL(size.targeted$curve[size.targeted$prep == 0 & size.targeted$curve != 0])

size.targeted$calc.sfl[size.targeted$prep == 2 ] <-
  CFL_to_SFL(size.targeted$curve[size.targeted$prep == 2 & size.targeted$curve != 0])

size.targeted$calc.cfl[size.targeted$prep == 0] <- 
  SFL_to_CFL(size.targeted$length[size.targeted$prep ==0 & size.targeted$length !=0])

size.targeted$calc.cfl[size.targeted$prep == 2] <- 
  SFL_to_CFL(size.targeted$length[size.targeted$prep ==2 & size.targeted$length !=0])

ggplot(size.targeted) +
  geom_point(aes(x=length, y=calc.sfl, col=as.factor(prep))) +
  geom_abline(slope=1) +
  geom_abline(slope=1, lty=2, intercept=sd(size.targeted$calc.sfl, na.rm = T)) +
  geom_abline(slope=1, lty=2, intercept=-sd(size.targeted$calc.sfl, na.rm=T)) +
  facet_wrap(vars(given.spec)) +
  labs(x="SFL", y="Calculated SFL", col="Disposition") +
  ggtitle('Congruence - SFL')

ggplot(size.targeted) +
  geom_point(aes(x=curve, y=calc.cfl, col=as.factor(prep))) +
  geom_abline(slope=1) +
  geom_abline(slope=1, lty=2, intercept=sd(size.targeted$calc.cfl, na.rm = T)) +
  geom_abline(slope=1, lty=2, intercept=-sd(size.targeted$calc.cfl, na.rm=T)) +
  facet_wrap(vars(given.spec)) +
  labs(x="CFL", y="Calculated CFL", col="Disposition") +
  ggtitle('Congruence - CFL')

whole <- size.targeted[size.targeted$prep %in% c(0,2),]
whole$use.length <- NA
whole$use.length[!is.na(whole$length)] <- whole$length[!is.na(whole$length)]
whole$use.length[is.na(whole$length)] <- whole$calc.sfl[is.na(whole$length)]
whole$use.length <- round(whole$use.length)
whole$length.units[!is.na(whole$length)] <- 'given sfl'
whole$length.units[is.na(whole$length)] <- 'calculated sfl'

whole <- dplyr::select(whole, -length, -curve, -calc.sfl, -calc.cfl)

size.tabulated <- rbind(whole)

ggplot(size.tabulated) +
  geom_histogram(aes(x=use.length, fill=as.factor(prep), group=as.factor(prep))) +
  geom_vline(xintercept=c(660, 1440), col='red') +
  geom_vline(xintercept=c(690, 1500), col='blue') +
  facet_wrap(vars(size_species))

# Ok that looks pretty clean. Let's merge.
catch.targeted$given.spec[catch.targeted$catch_species == 4673
] <- 'Young school'
catch.targeted$given.spec[catch.targeted$catch_species == 4676
] <- 'Small medium'
catch.targeted$given.spec[catch.targeted$catch_species %in% 
                            c(4671, 4679)] <- 'Large'
catch.targeted$given.spec[catch.targeted$catch_species %in%
                            c(4677, 4672, 4678)] <- 'Small'
catch.targeted$given.spec[catch.targeted$id %in%
                                  size.tabulated$id[
                                    size.tabulated$flag == 'changed small to young school'
                                  ]] <- 'Young school'
catch.targeted$catch_species[catch.targeted$id %in%
                            size.tabulated$id[
                              size.tabulated$flag == 'chhanged small to young school'
                            ]] <- 4673

catch.large <- catch.targeted[catch.targeted$given.spec == 'Small',]
catch.large=aggregate(catch_n~
                       id+year+month+stcode+
                       catch_species+from.catch+given.spec,
                     data=catch.large,
                     sum)
catch.large <- catch.large[with(catch.large,
                              order(year, month, stcode, id)),]
rownames(catch.large) <- NULL

catch.large$useid <- paste0(catch.large$id, "_", catch.large$catch_species)

catch.list <- split(catch.large, f=catch.large$useid)
for(i in 1:length(catch.list)){
  n <- catch.list[[i]]$catch_n
  catch.list[[i]] <- catch.list[[i]][
    rep(seq_len(nrow(catch.list[[i]])), each = n),
  ]
}
catch.outlist <- do.call(rbind, catch.list)
rownames(catch.outlist) <- NULL

size.tabulated$useid <- paste0(size.tabulated$id, "_", 
                               size.tabulated$size_species)

size.large <- size.tabulated[size.tabulated$given.spec == 'Small',]

cs <- merge(catch.outlist,
            size.large,
            by=c('useid', 'id', 'year', 'month', 
                 'stcode', 'given.spec'), 
            all.x=TRUE)
cs.nas <- cs[is.na(cs$use.length),]
cs.as <- cs[!is.na(cs$use.length),]
cs.as <- unique(cs.as)

cs.fin <- rbind(cs.nas, cs.as)
rm(catch.list, catch.outlist, catch.targeted, cs, cs.as, cs.nas,
   size.tabulated, size.targeted,whole, i, n, wd)

# MERGE MAIN AND CATCH
dat <- merge(target.main, 
             catch.large,
             by=c('id', 'year', 'month', 'stcode'),
             all.x=TRUE)

dat$catch_n[is.na(dat$catch_n)] <- 0

dat.list <- split(dat, f=dat$id)
for(i in 1:length(dat.list)){
  print(i)
  ncaught <- dat.list[[i]]$catch_n[1]
  dat.list[[i]] <- dplyr::select(dat.list[[i]],
                                 -catch_species,
                                 -from.catch,
                                 -useid)
  dat.list[[i]] <- dat.list[[i]][1,]
  dat.list[[i]]$catch_n <- ncaught
  rm(ncaught)
}
dat2 <- do.call(rbind, dat.list)
dat2 <- dat2[with(dat2, order(year, month, day, stcode, id)),]
rownames(dat2) <- NULL

dat2$given.spec[is.na(dat2$given.spec)] <- 'Failure'

write.csv(dat2,
          here('Data/Clean/LPS_SmallTarget_TripLevel.csv'),
          row.names = F)

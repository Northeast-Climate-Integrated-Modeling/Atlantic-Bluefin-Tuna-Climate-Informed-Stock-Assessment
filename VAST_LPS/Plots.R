
setwd("C:/Users/alex.hansell/Documents/BFT/Bft_Vast/LPS")

library(patchwork)
library(ggplot2)
library(readxl)
library(dplyr)

# Load in data

z<-dat%>%
  filter(cpue > 0)

# Annual indices
data<- read.csv("lps_size.csv")

AMO <- read_excel("~/BFT/Bft_Vast/LPS/AMO.xlsx")


ts<-AMO%>%
  filter(Year > 1992)

a<-ggplot()+
  geom_line(data=ts,aes(x=Year, y=Values), size=2, color="Blue")+
  theme_bw() + ylab("AMO") + xlab("")

b<-ggplot()+
  geom_line(data=ts,aes(x=Year, y=herring), size=2, color="Purple")+
  theme_bw() + ylab("Herring Biomass") + xlab("")

c<-ggplot()+
  geom_line(data=ts,aes(x=Year, y=Catch), size=2, color="Red")+
  theme_bw() + ylab("Landings/Biomass")

d<-ggplot()+
  geom_line(data=ts,aes(x=Year, y=Biomass), size=2, color="green")+
  theme_bw() + ylab("SSB")

a+b+c + d +plot_layout(ncol = 2)

# Histogram local

ggplot(data=dat, aes(x=temp))+
  geom_histogram()

ggplot(data=dat, aes(x=log(depth)))+
  geom_histogram()
# range shift

library(readxl)
range <- read_excel("range.xlsx", sheet = "Range")
View(range)

x<-as.data.frame(fit$parameter_estimates$SD$value)
x<-x[1:112,]
# sd
sd<-as.data.frame(fit$parameter_estimates$SD$sd)
sd<-sd[1:112,]

range<-merge(range,sd)

ggplot(data=range, aes(y=value, x=year, color=size))+
  #geom_abline(y=0)+
  geom_point() + 
  geom_line(size = 2)+ 
  theme_bw()+ ylab("Center of gravity (km)")+
  #scale_x_discrete(breaks =c(1993, 1995, 1997, 1999,2001,2003,2005,2007,2009,2011,2013,2015,2017,2019))+
  geom_errorbar(data=range, aes(x=year,ymax=value+sd, ymin=value-sd))+
  facet_wrap( ~ direction, scales = "free" )

# index

index <- read.csv("~/BFT/Bft_Vast/LPS/test/Table_for_SS3.csv")

lg<-rep("Large",28)
sm<-rep("Small",28)
size<-c(lg,sm)
size<-rep(size,2)

index<-cbind(index,size)

I<-ggplot(data=index, aes(x=Year,y=Estimate_metric_tons, color = size))+
  geom_point() + 
  geom_line(size = 2)+ 
  theme_bw()+ 
  ylab("Index")+ 
  geom_errorbar(data=index, aes(x=Year,ymax=Estimate_metric_tons+SD_mt, ymin=Estimate_metric_tons-SD_mt))+
  facet_wrap( ~ size, scales = "free" )

I+theme(legend.position = "none")

# Counter 

counter <- read_excel("counter.xlsx")
View(counter)

ggplot(data=counter, aes(x=Year,y=V1, color = V2))+
  #geom_point() + 
  geom_line(data=counter, aes(x=Year,y=V1, color = Covariates))+ 
  theme_bw()+ 
  ylab("Index")+ 
 # geom_errorbar(data=index, aes(x=year,ymax=Estimate_metric_tons+SD_mt, ymin=Estimate_metric_tons-SD_mt))+
  facet_wrap( ~ Direction +V2, scales = "free" )



##################################

load("C:/Users/alex.hansell/Documents/BFT/Bft_Vast/LPS/CPUE/Fixed-no/parameter_estimates.RData")
Sdreport_spring <-parameter_estimates$SD

load("C:/Users/alex.hansell/Documents/BFT/Bft_Vast/LPS/CPUE/GMRF2/parameter_estimates.RData")
Sdreport_fall<-parameter_estimates$SD

nrow_sd <- length(Sdreport_fall$value)

df_sdreport <-
  data.frame(variable = c(names(Sdreport_fall$value),
                          names(Sdreport_spring$value)),
             value = c(Sdreport_fall$value,
                       Sdreport_spring$value),
             sd = c(Sdreport_fall$sd,
                    Sdreport_spring$sd),
             season = c(rep("Fixed", times = nrow_sd),
                        rep("GMRF", times = nrow_sd)))

n_yea <- length(unique(dat$Year))
n_spp <- max(dat$target)
n_spp<-max(as.numeric(dat$length))
n_season <- nlevels(as.factor(df_sdreport$season))

df_cog <-
  df_sdreport %>%
  dplyr::filter(variable %in% c("mean_Z_ctm"))%>%
  dplyr::mutate(variable = rep(c("Eastings", "Northings"),
                               each = n_yea*2,
                               times= n_season),
                size = rep(c("large", "small"),
                           each = 1,
                           times = n_yea* n_season*2),
                year   = rep(c(1993:2020),
                             each = 2,
                             times = 4)
  )


z<-df_cog%>%
  filter(season == "GMRF",
         size =="small",
         variable =="Northings")%>%
  #group_by(size)%>% 
  mutate(diff= value - lag(value,
                           default =value[1]))

mean(z$diff)


x<-df_effArea%>%
  filter(season == "GMRF",
         size =="small",
         variable =="Eastings")%>%
  #group_by(size)%>%
  mutate(diff= value - lag(value,
                           default =value[1]))
mean(x$diff)

mean(x$sd)/27



a = df_cog%>%
  filter(variable == "Northings",
    season=="GMRF")%>%
  group_by(size)%>%
  summarise(value2 = value - mean(value),
            year = year,
            sd = sd,
            size = size,
            variable = variable)

b = df_cog%>%
  filter(variable == "Eastings",
         season=="GMRF")%>%
  group_by(size)%>%
  summarise(value2 = value - mean(value),
            year = year,
            sd = sd,
            size = size,
            variable = variable)

c<-rbind(a,b)

c$size<-recode_factor(c$size, large = "Large (> 177 cm)", small = " Small (< 177 cm)")

ah<-ggplot(data = c,aes(x=year, y=value2, col=size))+
  geom_line(size=1.15)+
  geom_point()+
  geom_errorbar(aes(ymin=value2-sd, ymax=value2+sd))+
  facet_wrap(~size+variable, scales ="free")+
  ylab("Center of Gravity (Km)")+ xlab("Year")+
  theme_bw()

ah + theme(legend.position="none")

ggsave("CG.png", width = 20, height = 20, units = "cm", dpi=600)


ggplot(data = df_cog%>%
         filter(variable == "Eastings",
                season=="GMRF"),aes(x=year, y=value-mean(value), col=size))+
  geom_line(size=1.15)+
  geom_point()+
  geom_errorbar(aes(ymin=(value-mean(value))-sd, ymax=(value-mean(value))+sd))+
  facet_grid(size+season, scales="fixed")+
  ylab("Eastings(Km)")+
  theme_bw()

ggplot(data = df_cog%>%
         filter(variable == "Eastings"),aes(x=year, y=value-mean(value), col=season))+
  geom_line(size=1.15)+
  geom_point()+
  #geom_errorbar(aes(ymin=value-sd, ymax=value/mean+sd))+
  facet_grid(~size, scales="fixed")+
  ylab("Northings(Km)")+
  theme_bw()




df_effArea <-
  df_sdreport %>%
  dplyr::filter(variable %in% c("effective_area_ctl"))%>%
  dplyr::mutate(size = rep(c("large", "small"),
                           times = n_yea*n_season))%>%
  dplyr::mutate(year   = rep(c(1993:2020),
                             each =2,
                             times = 2))
za = df_effArea%>%
  filter(season == "GMRF")

za$size<-recode_factor(za$size, large = "Large (> 177 cm)", small = " Small (< 177 cm)")


pp<-ggplot(data = za,aes(x=year, y=value, color=size))+
  geom_line(size=1.15)+
  geom_point()+
  geom_errorbar(aes(ymin=(value)-sd, ymax=(value)+sd))+
  facet_wrap(~size, scales ="free")+
  ylab("Effective Area (Km)")+ xlab("Year")+
  theme_bw()

pp + theme(legend.position="none")

ggsave("EA.png", width = 20, height = 10, units = "cm", dpi=600)

z<-df_effArea%>%
  filter(season == "GMRF",
         size =="large")%>%
  group_by(size)%>% 
  mutate(diff= value - lag(value,
                           default =value[1]))

mean(z$diff)/27

x<-df_effArea%>%
  filter(season == "GMRF",
         size =="small")%>%
  group_by(size)%>%
  mutate(diff= value - lag(value,
                           default =value[1]))
mean(x$diff)/27

mean(x$sd)/27

AnnualSpending - lag(AnnualSpending, 
                     default = AnnualSpending[1])

### CPUE

setwd("~/BFT/Bft_Vast/LPS/CPUE/plots")



bft <- read_excel("~/BFT/BFTCPUE.xlsx", 
                  sheet = "Sheet2")

bft$Category<-recode_factor(bft$Category, Large = "Large (> 177 cm)", Small = " Small (< 177 cm)")


ggplot(data = bft,aes(x=Year, y=Values, color=Method))+
  geom_line(size=1.15)+
  geom_point()+
  geom_errorbar(aes(ymin=(Values)-CV, ymax=(Values)+CV))+
  facet_wrap(~Category, scales ="free")+
  ylab("Index")+
  theme_bw()

ggsave("Index.png", width = 20, height = 10, units = "cm", dpi=600)


############# Confactual 


load("C:/Users/alex.hansell/Documents/BFT/Bft_Vast/LPS/CPUE/Fixed/parameter_estimates.RData")
Sdreport_spring <-parameter_estimates$SD

load("C:/Users/alex.hansell/Documents/BFT/Bft_Vast/LPS/CPUE/GMRF2/parameter_estimates.RData")
Sdreport_fall<-parameter_estimates$SD

nrow_sd <- length(Sdreport_fall$value)

df_sdreport <-
  data.frame(variable = c(names(Sdreport_fall$value),
                          names(Sdreport_spring$value)),
             value = c(Sdreport_fall$value,
                       Sdreport_spring$value),
             sd = c(Sdreport_fall$sd,
                    Sdreport_spring$sd),
             season = c(rep("Fixed", times = nrow_sd),
                        rep("GMRF", times = nrow_sd)))

n_yea <- length(unique(dat$Year))
n_spp <- max(dat$target)
n_spp<-max(as.numeric(dat$length))
n_season <- nlevels(as.factor(df_sdreport$season))

df_cog <-
  df_sdreport %>%
  dplyr::filter(variable %in% c("mean_Z_ctm"))%>%
  dplyr::mutate(variable = rep(c("Eastings", "Northings"),
                               each = n_yea*2,
                               times= n_season),
                size = rep(c("large", "small"),
                           each = 1,
                           times = n_yea* n_season*2),
                year   = rep(c(1993:2020),
                             each = 2,
                             times = 4)
  )



a = df_cog%>%
  filter(variable == "Northings")%>%
  group_by(size)%>%
  summarise(value2 = value - mean(value),
            year = year,
            sd = sd,
            size = size,
            variable = variable,
             Run = season)

b = df_cog%>%
  filter(variable == "Eastings")%>%
  group_by(size)%>%
  summarise(value2 = value - mean(value),
            year = year,
            sd = sd,
            size = size,
            variable = variable,
             Run = season)

c<-rbind(a,b)

c$size<-recode_factor(c$size, large = "Large (> 177 cm)", small = " Small (< 177 cm)")

ggplot(data = c,aes(x=year, y=value2, col=Run))+
  geom_line(size=1.15)+
  geom_point()+
  #geom_errorbar(aes(ymin=value2-sd, ymax=value2+sd))+
  facet_wrap(~size+variable, scales ="free")+
  ylab("Center of Gravity (Km)")+ xlab("Year")+
  theme_bw()

ggsave("CF.png", width = 20, height = 20, units = "cm", dpi=600)






# Exploration
load("C:/Users/alex.hansell/Documents/BFT/Bft_Vast/LPS/CPUE/Fixed/parameter_estimates.RData")
Sdreport_fixed <-parameter_estimates$SD

load("C:/Users/alex.hansell/Documents/BFT/Bft_Vast/LPS/CPUE/Fixed-no/parameter_estimates.RData")
Sdreport_fixed_no <-parameter_estimates$SD

load("C:/Users/alex.hansell/Documents/BFT/Bft_Vast/LPS/CPUE/Fixed-sst/parameter_estimates.RData")
Sdreport_fixed_sst <-parameter_estimates$SD

load("C:/Users/alex.hansell/Documents/BFT/Bft_Vast/LPS/CPUE/Fixed-sst+depth/parameter_estimates.RData")
Sdreport_fixed_sst_depth <-parameter_estimates$SD


load("C:/Users/alex.hansell/Documents/BFT/Bft_Vast/LPS/CPUE/Fixed-sst+depth+AMO/parameter_estimates.RData")
Sdreport_fixed_sst_depth_AMO <-parameter_estimates$SD

load("C:/Users/alex.hansell/Documents/BFT/Bft_Vast/LPS/CPUE/GMRF2/parameter_estimates.RData")
Sdreport_GMRF<-parameter_estimates$SD



nrow_sd <- length(Sdreport_fixed$value)

df_sdreport <-
  data.frame(variable = c(names(Sdreport_fixed$value),
                          names(Sdreport_fixed_no$value),
                          names(Sdreport_fixed_sst$value),
                          names(Sdreport_fixed_sst_depth$value),
                          names(Sdreport_fixed_sst_depth_AMO$value)),
             value = c(Sdreport_fixed$value,
                       Sdreport_fixed_no$value,
                       Sdreport_fixed_sst$value,
                       Sdreport_fixed_sst_depth$value,
                       Sdreport_fixed_sst_depth_AMO$value),
             sd = c(Sdreport_fixed$sd,
                    Sdreport_fixed_no$sd,
                    Sdreport_fixed_sst$sd,
                    Sdreport_fixed_sst_depth$sd,
                    Sdreport_fixed_sst_depth_AMO$sd),
             season = c(rep("All", times = nrow_sd),
                        rep("None", times = nrow_sd),
                        rep("+ SST", times = nrow_sd),
                        rep("+ Local", times = nrow_sd),
                        rep("+ Local + Regional", times = nrow_sd)))

n_yea <- length(unique(dat$Year))
n_spp <- max(dat$target)
n_spp<-max(as.numeric(dat$length))
n_season <- nlevels(as.factor(df_sdreport$season))


df_cog <-
  df_sdreport %>%
  dplyr::filter(variable %in% c("mean_Z_ctm"))%>%
  dplyr::mutate(variable = rep(c("Eastings", "Northings"),
                               each = n_yea*2,
                               times= n_season),
                size = rep(c("large", "small"),
                           each = 1,
                           times = n_yea* n_season*2),
                year   = rep(c(1993:2020),
                             each = 2,
                             times = 10)
  )


ggplot(data = a,aes(x=year, y=value2, col=season))+
  geom_line(size=1.15)+
  geom_point()+
  #geom_errorbar(aes(ymin=value2-sd, ymax=value2+sd))+
  facet_wrap(~size, scales ="free")+
  ylab("Center of Gravity (Km)")+ xlab("Year")+
  theme_bw()

a = df_cog%>%
  filter(variable == "Northings")%>%
  group_by(size, season)%>%
  summarise(value2 = value - mean(value),
            year = year,
            sd = sd,
            size = size,
            variable = variable,
            Run = season)

a$season <- factor(a$season, levels = c('None', '+ SST','+ Local', '+ Local + Regional', 'All'))


b = df_cog%>%
  filter(variable == "Eastings")%>%
  group_by(size, season)%>%
  summarise(value2 = value - mean(value),
            year = year,
            sd = sd,
            size = size,
            variable = variable,
            Run = season)

b$season <- factor(a$season, levels = c('None', '+ SST','+ Local', '+ Local + Regional', 'All'))

d<-rbind(a,b)

d$size<-recode_factor(d$size, large = "Large (> 177 cm)", small = " Small (< 177 cm)")

d$Run <- factor(d$Run, levels = c('None', '+ SST','+ Local', '+ Local + Regional', 'All'))

d<-d%>%
  filter(Run %in% c('None', '+ SST','+ Local', 'All'))

ggplot(data = d,aes(x=year, y=value2, col=Run))+
  geom_line(size=1.15)+
  geom_point()+
  scale_color_manual(values=c("red", "blue", "green", "orange"))+
  #geom_errorbar(aes(ymin=value2-sd, ymax=value2+sd))+
  facet_wrap(~size + variable, scales ="free")+
  ylab("Center of Gravity (Km)")+ xlab("Year")+
  theme_bw()

ggsave("sen.png", width = 20, height = 20, units = "cm", dpi=600)
#Covariates account for the majority of the deviations in spatial distribution


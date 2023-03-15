
# Load required packages ___________________________________________________________
require("knitr")
library(splines2)
library(TMB)
library(VAST) ## https://github.com/James-Thorson-NOAA/VAST
Version = get_latest_version(package="VAST")
library(tidyverse)

# Set working directory ____________________________________________________________
opts_knit$set(root.dir = "C:/Users/alex.hansell/Documents/BFT/Runs/200") 
setwd("C:/Users/alex.hansell/Documents/BFT/Runs/200")

DateFile = "C:/Users/alex.hansell/Documents/BFT/Runs/200"
dir.create(DateFile)  #only have to create this directory the first time
today <- lubridate::today()

# Load Data ________________________________________________________________________
NWA_bft_data<- read.csv("lpsAll.csv")
NWA_bft_data <- read.csv("~/BFT/Bft_Vast/index/lpsAll.csv")


NWA_bft_data$Lat<-NWA_bft_data$lat
NWA_bft_data$Lon<-NWA_bft_data$lon
NWA_bft_data$Year<-NWA_bft_data$year
NWA_bft_data$Length<-as.numeric(as.factor(NWA_bft_data$target))

NWA_bft_data<-NWA_bft_data%>%
  drop_na(lat) %>%
  drop_na(lon) %>%
  mutate(Year = year) %>%
 # filter(Year > 2002) %>%
  mutate(cpue=catch/fhours) %>%
 # filter(cpue<1)%>%
  filter(Lon < -69) %>%
    filter(Lat < 45)

  
#Plot
  NWA_bft_dataPOS<-NWA_bft_data%>%
    filter(cpue>0)

ggplot()+
  geom_point(data = dat, aes(x=Lon2,y=Lat2))

range(NWA_bft_data$Lon)
range(NWA_bft_data$Lat)


Data_Set <- NWA_bft_data
#NWA_bft_data <- na.exclude(NWA_bft_data)

# for presence absence 
for(i in 1:nrow(NWA_bft_data)){
  NWA_bft_data$PresJitter[i] = ifelse(NWA_bft_data$catch[i] == 0, 0, 1 + rnorm(n=1, mean=0, sd= 0.001)) #follow Thorson's recommendation to jitter presence by very small amount so VAST will estimate a logistic regression model
}

# spatial settings_______________________________________________________________________
Method = c("Grid", "Mesh")[2] #specify the way values of random effects are assigned -- "Mesh" allows for anisotropy
grid_size_km = 25  #this was 25 in the example; smaller number leads to slower run time
n_x =200   # Specify the number of knots (i.e. sample locations) to use in defining spatial variables. Start with 50-100 while tinkering with the model (to speed up run time) and increase knots to increase spatial resolution of the predictions.
Kmeans_Config = list("randomseed" = 1, "nstart" = 100, "iter.max" = 1e3 ) #

#model settings_________________________________________________________________________

# first linear predictor represents encounter probability or zero inflation
# second linear predictor = positive catch

##! temporarily turn off spatio-temporal factors to speed up initial run
FieldConfig = c("Omega1"=1, #spatial  variation in first linear predictor
                "Epsilon1"=1, # spatial temporal variation in first linear predictor
                "Omega2"=1, #spatial  variation in second linear predictor
                "Epsilon2"=1) # spatial temporal variation in second linear predictor

# spatial and temporal variation across years: 0 = fixed effect; 1 =independet among years; 2 = random walk; 3 = constant intercept

RhoConfig = c("Beta1"=0, #spatial and spatial temporal variation across years for first linear predictor
              "Beta2"=0,  #spatial and spatial temporal variation across years for second linear predictor
              "Epsilon1"=0, #spatial and spatial temporal variation across years for first linear predictor
              "Epsilon2"=0) #spatial and spatial temporal variation across years for second linear predictor

#RhoConfig=c("Beta1"=3,"Beta2"=0,"Epsilon1"=0,"Epsilon2"=0)

OverdispersionConfig = c("Eta1"=0, # random covariate for catchability on vessesl for first linear predictor
                         "Eta2"=0) # random covariate for catchability on vessesl for second linear predictor

ObsModel = c("PosDist" = 2, # distribution on positive catch
             "Link" = 0)  # distribution for proability of postitive catch

Options =  c("SD_site_density"=0,
             "SD_site_logdensity"=0,
             "Calculate_Range"=1,
             "Calculate_evenness"=0,
             "Calculate_effective_area"=1,
             "Calculate_Cov_SE"=0,
             'Calculate_Synchrony'=0,
             'Calculate_Coherence'=0,
             'normalize_GMRF_in_CPP'=TRUE)

#stratification
# For NEFSC indices, strata must be specified as a named list of area codes
# strata.limits = list('STRATA' = "All_areas")
Region = "user"
#Region = "northwest_atlantic"

Record = ThorsonUtilities::bundlelist(c("Data_Set"
                                        , "Version"
                                        , "Method"
                                        , "grid_size_km"
                                        , "n_x"
                                        , "Kmeans_Config"
                                        , "FieldConfig"
                                        , "RhoConfig"
                                        , "OverdispersionConfig" 
                                        , "ObsModel"
                                        , "Options"
))
save(Record, file=file.path(DateFile, "Record.RData"))
capture.output(Record, file=paste0(DateFile, "Record.txt"))


# use this one for pre-defined regions
 #Extrapolation_List <- make_extrapolation_info(Region=Region, 
 #                                             strata.limits=as.data.frame(NWA_BTS_stratalimits), 
 #                                            observations_LL = NWA_bft_data[,c('Lat','Lon')], 
 #                                           maximum_distance_from_sample=15)

# use this section for custom region and exrapolation grid ... see https://github.com/ceciliaOLearySBU/MakeGrids

grid_NWA_BTS <- readRDS("~/BFT/Bft_Vast/index/grid_NWA_BTS_2020_05_28.rds")

#grid_NWA_BTS <-grid_NWA_BTS %>%
  #filter(Lon > -72) %>% 
 # filter(Lat < 46)



NWA_BTS_boundaries <- c(range(grid_NWA_BTS$Lon), range(grid_NWA_BTS$Lat))
NWA_BTS_stratalimits <- data.frame(
  'STRATA' = c("All_areas")
  , 'west_border' = c(NWA_BTS_boundaries[1])
  , 'east_border' = c(NWA_BTS_boundaries[2])
  , 'north_border' = c(NWA_BTS_boundaries[4])
  
  , 'south_border' = c(NWA_BTS_boundaries[3])
)





NWA_BTS_extrap <- FishStatsUtils::make_extrapolation_info(Region = Region
                                                          , strata.limits = NWA_BTS_stratalimits
                                                          , input_grid = grid_NWA_BTS
                                                          #, grid_dim_km = c(10, 10)
                                                          , observations_LL = NWA_bft_data[,c("Lat", "Lon")]
                                                          , flip_around_dateline = FALSE
                                                          , zone =  NA # UTM zone; use NA for automatic detection 
                                                          , projargs = NA)
Extrapolation_List = NWA_BTS_extrap


#Extrapolation_List = NWA_BTS_extrap

Spatial_List <- make_spatial_info(grid_size_km = grid_size_km, 
                                  n_x = n_x, 
                                  Method = Method, 
                                  Lon = NWA_bft_data[,'Lon'], 
                                  Lat = NWA_bft_data[,'Lat'], 
                                  Extrapolation_List = Extrapolation_List,
                                  fine_scale = FALSE, ##!
                                  DirPath=DateFile, 
                                  Save_Results=TRUE ) ##!
# Add knots to data
NWA_bft_data <- cbind(NWA_bft_data, "knot_i"=Spatial_List$knot_i)

X <-  NWA_bft_data %>% select(Lat, Lon, Year, fhours, depth, temp)

X$depth <- (X$depth - mean(X$depth)) / sd(X$depth)
#X$slp <- (X$slp - mean(X$slp)) / sd(X$slp)
X$sst <- (X$temp - mean(X$temp)) / sd(X$temp)
#X$windSpeed <- (X$windSpeed - mean(X$windSpeed)) / sd(X$windSpeed)
X$Hours <- (X$fhours - mean(X$fhours)) / sd(X$fhours)
#X$zenith <- (X$zenith - mean(X$zenith)) / sd(X$zenith)

# covariate_data <- make_covariates(formula = ~ Depth_M + BottomTemp_C
#                                   , covariate_data = X
#                                   , Year_i = X$Year
#                                   , spatial_list = Spatial_List
#                                   , extrapolation_list = Extrapolation_List)
# 
settings = make_settings(n_x=100, #was 200 in example
                         Region= "User",
                         purpose = "index2",
                         use_anisotropy=FALSE,
                         strata.limits=NWA_BTS_stratalimits,
                         bias.correct=TRUE,
                         fine_scale=FALSE,
                         FieldConfig = FieldConfig,
                         RhoConfig = RhoConfig,
                         ObsModel = ObsModel,
                         OverdispersionConfig = OverdispersionConfig,
                         Options = Options,
                         Version = Version
)
# formula = ~ Depth_M + BottomTemp_C #+ zenith # zenith should be moved to a catchability variabile rather than environment


#Make the TMB-compatible data file, make a list of components needed to assemble the TMB function, configure settings, assign a destination to save the output. This part takes some time.


TmbData = make_data(
  ObsModel = ObsModel,  # distribution for proability of postitive catch, # first number = pos catch, second encounter prob
  "b_i" = NWA_bft_data[ ,'cpue'], 
   #"a_i" = NWA_bft_data[ ,'lines'], # sampled areas
  "a_i" = rep(1, nrow(NWA_bft_data)), 
  "t_i" = NWA_bft_data[ ,'Year'], # time step
  #"c_i" = rep(0, nrow(NWA_bft_data)), # error distribution per time step
  #"c_i" = as.numeric(NWA_bft_data[,'Length'])-1, # error distribution per time step
  #c_i = as.numeric(example$sampling_data[,'Length_bin'])-1
  # "e_i" = c_i, # number of different error distributions
  #"v_i" = as.numeric(NWA_bft_data[,'country2'])-1, # number of vessel types
  #"Q_ik" = as.matrix(NWA_bft_data[ , 'fhours']), #catchability covariate(s)
  "FieldConfig" = FieldConfig,
  # "s_i" = NWA_illex_data[ ,'knot_i']-1, 
  "spatial_list" = Spatial_List, 
  # "OverdispersionConfig" = OverdispersionConfig,
  "RhoConfig" = RhoConfig,
  # "VamConfig" = c(,,) #option to estimate interactions,
  #"Aniso" = 1, # use 0 for isotropy or 1 for geometic anisotropy
  # "PredTF_i" = c() #optional each observation i is included in the likelihood (0) or in the predictive probability (1)
  #"covariate_data" = X,
  #"catchability_data" = X,
  "X1_formula" =  ~  bs(sst), # + bs(depth),
  "X2_formula" =  ~ bs(sst),# + bs(depth), # covarites that influence density
 # "Q1_formula" =  ~    bs(Hours), # covariates that influence catchability of zeros 
 # "Q2_formula" =  ~   bs(Hours), # covariates that influence catchability of positive catch
  # "MeshList" = Spatial_List$MeshList, 
  # "GridList" = Spatial_List$GridList, 
  # "Method" = Spatial_List$Method, 
  "Options" = Options,
  "Version" = Version
)


TmbList = VAST::make_model("TmbData"=TmbData, 
                           "RunDir"=DateFile, 
                           "Version"=Version, 
                           "RhoConfig"=RhoConfig, 
                           "loc_x"=Spatial_List$loc_x, 
                           "Method"=Method)

Obj = TmbList[["Obj"]]

Opt = fit_model(obj=Obj, 
                         lower=TmbList[["Lower"]], 
                         upper=TmbList[["Upper"]], 
                         getsd=TRUE, ##! Turn off for faster processing to check on structure. Turn it back on if this works. 
                         savedir=DateFile, 
                         bias.correct=FALSE, ##! Turn off for faster processing to check on structure. Turn it back on if this works.
                         newtonsteps=1,
                         # bias.correct.control=list(sd=FALSE, split=NULL, nsplit=1, vars_to_correct="Index_cyl") 
)

# Plot results
results = plot( Opt,
                check_residuals = FALSE )

Report = Obj$report()
Save = list("Opt"=Opt, "Report"=Report, "ParHat"=Obj$env$parList(Opt$par), "TmbData"=TmbData)
save(Save, file=paste0(DateFile, "/Save_VAST_output_env_2020-09-04.RData"))

# Run some basic diagnostics, plot the location data, plot observed encounter frequency against predicted probability,
#QQ and other diagnostic plots for positive catch data.


#Convergence
pander::pandoc.table(Opt$diagnostics[,c('Param','Lower','MLE','Upper','final_gradient')])
write.csv(as.data.frame(Opt$diagnostics[,c('Param','Lower','MLE','Upper','final_gradient')]), file=file.path(DateFile, "parameterestimates.csv"), row.names = FALSE)
# The lower and upper values are the parameter bounds. Make sure that the MLE is not hitting them. Make sure final_gradient is near zero.
## -- looks good :)

#Plot location data
plot_data(Extrapolation_List=Extrapolation_List, 
          Spatial_List=Spatial_List, 
          Data_Geostat=NWA_bft_data, 
          PlotDir=DateFile )


#Diagnostic for encounter probability
#Enc_prob = plot_encounter_diagnostic(Report=Report, 
#                                     Data_Geostat=NWA_bft_data, 
#                                    DirName=DateFile)


#Diagnostic for positive catch rate component
Q = plot_quantile_diagnostic(TmbData=TmbData, 
                             Report=Report, 
                             FileName_PP="Posterior_Predictive",
                             FileName_Phist="Posterior_Predictive-Histogram", 
                             FileName_QQ="Q-Q_plot", 
                             FileName_Qhist="Q-Q_hist", 
                             DateFile=DateFile)


MapDetails_List = make_map_info(Region=Region
                                , NN_Extrap = Spatial_List$PolygonList$NN_Extrap
                                , spatial_list = Spatial_List
                                , Extrapolation_List = Extrapolation_List
                                , fine_scale = Spatial_List$fine_scale)

# Decide which years to plot                                                   
Year_Set = seq(min(NWA_bft_data[,'Year']),
               max(NWA_bft_data[,'Year']))
Years2Include = which(Year_Set %in% sort(unique(NWA_bft_data[,'Year'])))


plot_residuals(Lat_i=NWA_bft_data[,'Lat'], 
               Lon_i=NWA_bft_data[,'Lon'], 
               TmbData=TmbData, 
               Report=Report, 
               Q=Q, 
                projargs = "+proj=longlat",
               working_dir=DateFile,
               spatial_list = Spatial_List,
               extrapolation_list = Extrapolation_List,
               Year_Set = Year_Set, 
               Years2Include = Years2Include,   
               # PlotDF=MapDetails_List[["PlotDF"]], 
               # MapSizeRatio=MapDetails_List[["MapSizeRatio"]], 
               # Xlim=MapDetails_List[["Xlim"]], 
               # Ylim=MapDetails_List[["Ylim"]], 
               # FileName=DateFile, 
               # Rotate=MapDetails_List[["Rotate"]], 
               # Cex=MapDetails_List[["Cex"]], 
               # Legend=MapDetails_List[["Legend"]], 
               # zone=MapDetails_List[["Zone"]], 
               mar=c(0,0,2,0), 
               oma=c(3.5,3.5,0,0), 
               cex=1.8)


#How do the properties change across directions? e.g. are locations more similar north-to-south vs east-to-west?
plot_anisotropy( FileName=paste(DateFile,"Aniso.png", sep="/"), 
                 Report=Report, 
                 TmbData=TmbData )

Prob_xt = plot_maps(plot_set = 3 #set 1 is the probability of encounter
                    # , MappingDetails=MapDetails_List[["MappingDetails"]]
                    , Report = Report
                    , Sdreport = Opt$SD
                    # , Panel = "Category"
                    , Year_Set = Year_Set
                    , Years2Include = Years2Include
                    , MapSizeRatio = MapDetails_List[["MapSizeRatio"]]
                    , working_dir = DateFile
                    , PlotDF = MapDetails_List[["PlotDF"]]
                    # , Legend = MapDetails_List[["Legend"]]
                    , mar = c(0,0,2,0)
                    , oma = c(3.5,3.5,0,0)
                    , cex = 1.8
                    # , plot_legend_fig = FALSE
)

Index = plot_biomass_index( DirName=DateFile, 
                            TmbData=TmbData, 
                            Sdreport=Opt[["SD"]], 
                            Year_Set=Year_Set, 
                            Years2Include=Years2Include, 
                            use_biascorr=TRUE )
pander::pandoc.table( Index$Table[,c("Year","Fleet","Estimate_metric_tons","SD_log","SD_mt")] ) 
# 
# write.csv(as.data.frame(Index$Table[,c("Year","Fleet","Estimate_metric_tons","SD_log","SD_mt")]), file=file.path(DateFile, "diagnostics.csv"), row.names = FALSE)

plot_range_index(Report=Report, 
                 TmbData=TmbData, 
                 Sdreport=Opt[["SD"]], 
                 Znames=colnames(TmbData$Z_xm), 
                 PlotDir=DateFile, 
                 Year_Set=Year_Set)



save.image(file=paste0(DateFile, "/SaveAll.Rdata"))



#Save the probability values in a convenient format for plotting.

probmapval <- as.data.frame(Save$Report$R1_gcy)

colnames(probmapval) <- paste0(rep("Prob", ncol(probmapval)),Year_Set)

# probcoords <- filter(MapDetails_List$PlotDF, Include==TRUE)[,1:2]
probcoords <- MapDetails_List$PlotDF[,1:2]

probmap <- cbind(probcoords, probmapval)
coordinates(probmap) <- c("Lat", "Lon")

```

proportions = calculate_proportion( TmbData = Opt$data_list,
                                    Index = results$Index,
                                    Year_Set = Opt$year_labels )


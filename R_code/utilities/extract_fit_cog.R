### Post-fit VAST function to extract center of gravity values ###

extract_fit_cog <- function(fit) {
  # Create objects needed to plot
  Sdreport = fit$parameter_estimates$SD
  SD = TMB::summary.sdreport(Sdreport)
  TmbData = fit$data_list
  Report = fit$Report
  
  # Name where data are stored in report
  CogName = "mean_Z_ctm"
  EffectiveName = "effective_area_ctl"
  
  # Pull data
  SD_mean_Z_ctm = array(NA, dim = c(unlist(TmbData[c("n_c", 
                                                     "n_t", 
                                                     "n_m", 
                                                     "n_l"
  )]), 
  2), 
  dimnames = list(NULL, NULL, NULL, NULL,
                  c("Estimate", "Std. Error")))
  # Pull standard error
  SD_mean_Z_ctm[] = SD[which(rownames(SD) == CogName), 
                       c("Estimate", "Std. Error")]
  # Name dimensions      
  names(dim(SD_mean_Z_ctm)) <- c('Category',
                                 'Time', 
                                 'Location', 
                                 'Strata',
                                 'Est.Err')
  # Keep only US strata, drop category      
  SD_mean_Z_ctm <- SD_mean_Z_ctm[1,,,1,]
  
  # COG
  cog <- as.data.frame(SD_mean_Z_ctm[,,])
  colnames(cog) <- c('easting', 'northing', 'e.sd', 'n.sd')
  cog$Year <- fit$year_labels
  cog$Year <- as.numeric(cog$Year)
  
  # Convert to meters
  cog$easting <- cog$easting * 1000
  cog$northing <- cog$northing * 1000
  cog$e.sd <- cog$e.sd * 1000
  cog$n.sd <- cog$n.sd * 1000
  cog$units <- as_units('meter')
  
  write.csv(cog, row.names = F,
            'COG.csv')
}
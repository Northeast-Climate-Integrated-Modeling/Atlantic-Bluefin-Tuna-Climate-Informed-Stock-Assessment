### Post-fit VAST function to extract area occupied values ###

extract_fit_eff <- function(fit) {
  # Create objects needed to plot
  Sdreport = fit$parameter_estimates$SD
  SD = TMB::summary.sdreport(Sdreport)
  TmbData = fit$data_list
  Report = fit$Report
  
  # Name where data are stored in report
  EffectiveName = "effective_area_ctl"
  
  # Pull data
  SD_mean_Z_ctm = array(NA, dim = c(unlist(TmbData[c("n_c", 
                                                     "n_t", 
                                                     "n_l")]), 
  2), 
  dimnames = list(NULL, NULL, NULL,
                  c("Estimate", "Std. Error")))
  # Pull standard error
  SD_mean_Z_ctm[] = SD[which(rownames(SD) == EffectiveName), 
                       c("Estimate", "Std. Error")]
  # Name dimensions      
  names(dim(SD_mean_Z_ctm)) <- c('Category',
                                 'Time',
                                 'Strata',
                                 'ArrOc')
  # Keep only US strata, drop category      
  SD_mean_Z_ctm <- SD_mean_Z_ctm[1,,1,]
  
  # COG
  cog <- as.data.frame(SD_mean_Z_ctm[,])
  colnames(cog) <- c('area.occ', 'std.err')
  cog$Year <- fit$year_labels
  cog$Year <- as.numeric(cog$Year)
  
  # Convert to meters
  cog$area.occ <- cog$area.occ * 1000
  cog$std.err <- cog$std.err * 1000
  cog$units <- as_units('meter')
  
  write.csv(cog, row.names = F,
            'AreaOcc.csv')
}
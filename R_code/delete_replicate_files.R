# Script to delete files in model selection and covaraite selection process 
# that are not needed:
# * kmeans_extrapolation
# * kmeans_knots
# * packageDescription
# * parameter_estimates.TXT

# Please retain settings.txt and parameter_estimates.RDATA

# Clear workspace
rm(list=ls())

# Set libraries
library(here)

# Call flder and list files
files <- list.files(here('covar_selection'))

# Loop through subfolder
for(i in 1:length(files)){
  # Set wd
  folderopen <- paste0(here('covar_selection'),'/', files[i])
  setwd(folderopen)
  # List files in subfolder (wd)
  infiles <- list.files(folderopen)
  # Remove unnecessary files
  unlink('Kmeans_extrapolation-2000.Rdata')
  unlink('Kmeans_knots-200.RData')
  unlink('packageDescription.txt')
  unlink('parameter_estimates.txt')
}

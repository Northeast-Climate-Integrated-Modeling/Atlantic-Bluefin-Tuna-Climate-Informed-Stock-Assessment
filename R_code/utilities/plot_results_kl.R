### Post-fit VAST function to plot results when D_gct is unitless ###

plot_results_kl <- function (fit, settings = fit$settings, plot_set = 3, working_dir = getwd(), 
          year_labels = fit$year_labels, years_to_plot = fit$years_to_plot, 
          category_names = fit$category_names, strata_names = fit$strata_names, 
          use_biascorr = TRUE, map_list = NULL, check_residuals = TRUE, 
          cluster_results = TRUE, projargs = "+proj=longlat", zrange, 
          n_samples = 100, calculate_relative_to_average = FALSE, type = 1, 
          n_cells = NULL, n_cells_residuals = NULL, RotationMethod = "PCA", 
          quantiles = c(0.05, 0.5, 0.95), similarity_metric = c("hclust", 
                                                                "Correlation", "Dissimilarity", "Covariance")[1], ...) 
{
  if (is.null(fit$Report)) 
    stop("`fit$Report` is missing, please check inputs")
  if (is.null(category_names)) 
    category_names = paste0("Category_", 1:fit$data_list$n_c)
  dir.create(working_dir, showWarnings = FALSE, recursive = TRUE)
  message("\n### Creating plots in directory ", working_dir)
  message("\n### Making plots of data availability and knots")
  plot_data_args = list(...)
  plot_data_args = combine_lists(input = plot_data_args, args_to_use = formalArgs(plot_data), 
                                 default = list(Extrapolation_List = fit$extrapolation_list, 
                                                Spatial_List = fit$spatial_list, Lat_i = fit$data_frame[, 
                                                                                                        "Lat_i"], Lon_i = fit$data_frame[, "Lon_i"], 
                                                Year_i = fit$data_frame[, "t_i"], PlotDir = working_dir, 
                                                year_labels = year_labels, projargs = projargs))
  do.call(what = plot_data, args = plot_data_args)
  if (is.null(map_list)) {
    message("\n### Obtaining default settings for plotting maps")
    map_list = make_map_info(Region = settings$Region, spatial_list = fit$spatial_list, 
                             Extrapolation_List = fit$extrapolation_list)
  }
  message("\n### Making plot of anisotropy")
  plot_anisotropy(FileName = file.path(working_dir, "Aniso.png"), 
                  Obj = fit$tmb_list$Obj)
  plot_biomass_index_args = list(...)
  if (!is.null(fit$parameter_estimates$SD)) {
    message("\n### Making plot of abundance index")
    plot_biomass_index_args = combine_lists(input = plot_biomass_index_args, 
                                            args_to_use = formalArgs(plot_biomass_index), default = list(DirName = working_dir, 
                                                                                                         fit = fit, year_labels = year_labels, years_to_plot = years_to_plot, 
                                                                                                         use_biascorr = use_biascorr, category_names = category_names, 
                                                                                                         strata_names = strata_names))
    Index = do.call(what = plot_biomass_index, args = plot_biomass_index_args)
  }
  else {
    Index = "Not run"
    message("\n### Skipping plot of abundance index; must re-run with standard errors to plot")
  }
  plot_similarity_args = list(...)
  message("\n### Making plot of covariance/dissimilarity matrices")
  plot_similarity_args = combine_lists(input = plot_similarity_args, 
                                       args_to_use = formalArgs(plot_similarity), default = list(fit = fit, 
                                                                                                 year_labels = year_labels, category_names = category_names, 
                                                                                                 similarity_metric = similarity_metric, working_dir = working_dir))
  do.call(what = plot_similarity, args = plot_similarity_args)
  if (!is.null(fit$parameter_estimates$SD) & fit$data_list$n_c > 
      1) {
    message("\n### Making plot of composition data")
    calculate_proportion_args = list(...)
    calculate_proportion_args = combine_lists(input = calculate_proportion_args, 
                                              args_to_use = formalArgs(calculate_proportion), default = list(fit = fit, 
                                                                                                             TmbData = fit$data_list, Index = Index, year_labels = year_labels, 
                                                                                                             years_to_plot = years_to_plot, use_biascorr = use_biascorr, 
                                                                                                             category_names = category_names, DirName = working_dir, 
                                                                                                             n_samples = n_samples))
    Proportions = do.call(what = calculate_proportion, args = calculate_proportion_args)
  }
  else {
    Proportions = "Not run"
    message("\n### Skipping plot of composition data; must re-run with standard errors and multiple categories to plot")
  }
  if (!is.null(fit$parameter_estimates$SD)) {
    message("\n### Making plot of spatial indices")
    Range = plot_range_index(Report = fit$Report, TmbData = fit$data_list, 
                             Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), 
                             PlotDir = working_dir, year_labels = year_labels, 
                             years_to_plot = years_to_plot, use_biascorr = use_biascorr, 
                             category_names = category_names)
  }
  else {
    Range = "Not run"
    message("\n### Skipping plot of spatial indices; must re-run with standard errors to plot")
  }
  if ("jointPrecision" %in% names(fit$parameter_estimates$SD) & 
      n_samples > 0 & fit$data_list$Options_list$Options["Calculate_Range"] == 
      TRUE) {
    message("\n### Making plot of range edges")
    Edge = plot_range_edge(Obj = fit$tmb_list$Obj, Sdreport = fit$parameter_estimates$SD, 
                           working_dir = working_dir, year_labels = year_labels, 
                           years_to_plot = years_to_plot, category_names = category_names, 
                           n_samples = n_samples, quantiles = quantiles, calculate_relative_to_average = calculate_relative_to_average)
  }
  else {
    Edge = "Not run"
    message("\n### Skipping plot of range edge; only possible if `getJointPrecision=TRUE`, `Options['Calculate_Range']=TRUE`, and `n_samples`>0")
  }
  message("\n### Making plots of spatial predictions")
  plot_maps_args = list(...)
  plot_maps_args = combine_lists(input = plot_maps_args, default = list(fit = fit, 
                                                                        plot_set = plot_set, category_names = category_names, 
                                                                        PlotDF = map_list[["PlotDF"]], MapSizeRatio = map_list[["MapSizeRatio"]], 
                                                                        working_dir = working_dir, year_labels = year_labels, 
                                                                        years_to_plot = years_to_plot, legend_x = map_list[["Legend"]]$x/100, 
                                                                        legend_y = map_list[["Legend"]]$y/100, projargs = projargs, 
                                                                        n_cells = n_cells))
  Dens_xt = do.call(what = plot_maps_kl, args = plot_maps_args)
  message("\n### Making plots for factors (if present)")
  plot_factors_args = list(...)
  plot_factors_args = combine_lists(input = plot_factors_args, 
                                    default = list(fit = fit, mapdetails_list = map_list, 
                                                   projargs = projargs, n_cells = n_cells, RotationMethod = RotationMethod, 
                                                   plotdir = working_dir, category_names = category_names))
  Factors = do.call(what = plot_factors, args = plot_factors_args)
  if (cluster_results == TRUE) {
    message("\n### Making plots for spatial cluster analysis")
    plot_clusters_args = list(...)
    plot_clusters_args = combine_lists(input = plot_clusters_args, 
                                       args_to_use = c(formalArgs(plot_clusters), formalArgs(plot_variable)), 
                                       default = list(fit = fit, year_labels = year_labels, 
                                                      category_names = category_names, map_list = map_list, 
                                                      working_dir = working_dir, n_cells = n_cells, 
                                                      projargs = projargs))
    Clusters = do.call(what = plot_clusters, args = plot_clusters_args)
  }
  else {
    Clusters = NULL
  }
  if (check_residuals == TRUE) {
    message("\n### Making quantile residuals using conditional simulation and package DHARMa")
    dharmaRes = summary(fit, what = "residuals", working_dir = working_dir, 
                        type = type, ...)
    message("\n### Plotting quantile residuals ")
    dharma_raster = plot_quantile_residuals(dharmaRes = dharmaRes, 
                                            fit = fit, working_dir = working_dir, year_labels = year_labels, 
                                            years_to_plot = years_to_plot, n_cells_residuals = n_cells_residuals, 
                                            projargs = projargs, ...)
    message("\n### Skipping plot of semivariance for normal-transformed quantile residuals")
    residual_semivariance = NULL
  }
  else {
    message("\n### Skipping quantile residuals using conditional simulation and package DHARMa")
    message("\n### Skipping plot of quantile residuals ")
    message("\n### Skipping plot of semivariance for normal-transformed quantile residuals")
    dharmaRes = NULL
    dharma_raster = NULL
    residual_semivariance = NULL
  }
  Return = list(dharmaRes = dharmaRes, dharma_raster = dharma_raster, 
                residual_semivariance = residual_semivariance, Index = Index, 
                Proportions = Proportions, Range = Range, Dens_xt = Dens_xt, 
                Edge = Edge, map_list = map_list, plot_maps_args = plot_maps_args, 
                plot_biomass_index_args = plot_biomass_index_args, Factors = Factors, 
                Clusters = Clusters)
  return(invisible(Return))
}
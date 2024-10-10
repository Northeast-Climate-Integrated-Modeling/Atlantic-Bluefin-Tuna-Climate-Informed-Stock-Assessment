### Post-fit VAST function to plot maps when D_gct is unitless ###

plot_maps_kl <- function (plot_set = 3, fit, PlotDF, projargs = "+proj=longlat", 
          Panel = "Category", year_labels = NULL, years_to_plot = NULL, 
          category_names = NULL, quiet = FALSE, working_dir = getwd(), 
          MapSizeRatio = NULL, n_cells, plot_value = "estimate", n_samples = 100, 
          country = NULL, sample_fixed = TRUE, Report = fit$Report, 
          TmbData = fit$data_list, Obj = fit$tmb_list$Obj, extrapolation_list = fit$extrapolation_list, 
          Sdreport = fit$parameter_estimates$SD, Map = fit$tmb_list$Map, 
          zlim = NULL, ...) 
{
  extract_value = function(Sdreport, Report, Obj, variable_name, 
                           plot_value = "estimate", f = identity, n_samples, sample_fixed = TRUE) {
    if (missing(Report)) {
      Report = Obj$report()
    }
    if (is.function(plot_value)) {
      if (missing(Obj)) 
        stop("Must provide `Obj` for `extract_value(.)` in `plot_maps(.)` when specifying a function for argument `plot_value`")
      Var_r = sample_variable(Sdreport = Sdreport, Obj = Obj, 
                              variable_name = variable_name, n_samples = n_samples, 
                              sample_fixed = sample_fixed)
      Var_r = f(Var_r)
      Return = apply(Var_r, MARGIN = 1:(length(dim(Var_r)) - 
                                          1), FUN = plot_value)
      if (any(dim(Return) != dim(Report[[variable_name]]))) {
        stop("Check `extract_value(.)` in `plot_maps(.)`")
      }
      dimnames(Return) = dimnames(Report[[variable_name]])
    }
    else if (plot_value == "estimate") {
      Return = f(Report[[variable_name]])
    }
    else stop("Check input `plot_value` in `plot_maps(.)`")
    return(Return)
  }
  if (!is.null(Obj)) {
    if (is.null(Report)) 
      Report = Obj$report()
  }
  else {
    if (plot_value != "estimate") 
      stop("Must provide `Obj` to `plot_maps` when using function for `plot_value`")
  }
  if (is.null(MapSizeRatio)) {
    MapSizeRatio = c(3, 3)
  }
  Report = amend_output(Report = Report, TmbData = TmbData, 
                        Map = Map, year_labels = year_labels, category_names = category_names, 
                        extrapolation_list = extrapolation_list)
  Return = NULL
  for (plot_num in plot_set) {
    Array_xct = NULL
    plot_code <- c("encounter_prob", "pos_catch", "ln_density", 
                   "", "", "epsilon_1", "epsilon_2", "linear_predictor_1", 
                   "linear_predictor_2", "density_CV", "covariates_1", 
                   "covariates_2", "total_density", "covariate_effects_1", 
                   "covariate_effects_2", "omega_1", "omega_2", "xi_1", 
                   "xi_2", "phi_1", "phi_2")[plot_num]
    if (plot_num == 1) {
      if (quiet == FALSE) 
        message(" # plot_num ", plot_num, ": Plotting presence/absense maps")
      if ("D_xt" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "R1_xt")
      if ("D_xct" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "R1_xct")
      if ("D_xcy" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "R1_xcy")
      if ("D_gcy" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "R1_gcy")
      if ("D_gct" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "R1_gct")
      if (any(c("dhat_ktp", "dpred_ktp") %in% names(Report))) 
        stop("Not implemented for SpatialVAM")
      message("`plot_num=1` doesn't work well when using ObsModel[2]==1, because average area-swept doesn't generally match area of extrapolation-grid cells")
    }
    if (plot_num == 2) {
      if (quiet == FALSE) 
        message(" # plot_num ", plot_num, ": Plotting positive catch rate maps")
      if ("D_xt" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  f = log, variable_name = "R2_xt")
      if ("D_xct" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  f = log, variable_name = "R2_xct")
      if ("D_xcy" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  f = log, variable_name = "R2_xcy")
      if ("D_gcy" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  f = log, variable_name = "R2_gcy")
      if ("D_gct" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  f = log, variable_name = "R2_gct")
      if (any(c("dhat_ktp", "dpred_ktp") %in% names(Report))) 
        stop("Not implemented for SpatialVAM")
      message("`plot_num=2` doesn't work well when using ObsModel[2]==1, because average area-swept doesn't generally match area of extrapolation-grid cells")
    }
    if (plot_num == 3) {
      if (quiet == FALSE) 
        message(" # plot_num ", plot_num, ": Plotting density maps (in log-space)")
      if ("D_xt" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  f = log, variable_name = "D_xt")
      if ("D_xct" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  f = log, variable_name = "D_xct")
      if ("D_xcy" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  f = log, variable_name = "D_xcy")
      if ("D_gcy" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  f = log, variable_name = "D_gcy")
      if ("D_gct" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  f = identity, variable_name = "D_gct")
      if ("dhat_ktp" %in% names(Report)) 
        Array_xct = aperm(extract_value(Sdreport = Sdreport, 
                                        Obj = Obj, Report = Report, plot_value = plot_value, 
                                        sample_fixed = sample_fixed, n_samples = n_samples, 
                                        variable_name = "dhat_ktp")[, , cI], c(1, 3, 
                                                                               2))
      if ("dpred_ktp" %in% names(Report)) 
        Array_xct = aperm(extract_value(Sdreport = Sdreport, 
                                        Obj = Obj, Report = Report, plot_value = plot_value, 
                                        sample_fixed = sample_fixed, n_samples = n_samples, 
                                        variable_name = "dpred_ktp")[, , cI], c(1, 
                                                                                3, 2))
      Array_xct = strip_units(Array_xct)
      Array_xct = log(Array_xct)
    }
    if (plot_num == 4) {
      stop("`plot_num=4` is deprecated")
    }
    if (plot_num == 5) {
      stop("`plot_num=5` is deprecated")
    }
    if (plot_num == 6) {
      if (quiet == FALSE) 
        message(" # plot_num ", plot_num, ": Plotting spatio-temporal effects (Epsilon) in 1st linear predictor")
      if ("D_xt" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "Epsilon1_st")
      if ("D_xct" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "Epsilon1_sct")
      if ("D_xcy" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "Epsilon1_sct")
      if (any(c("D_gcy", "D_gct") %in% names(Report))) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, 
                                  Report = Report, 
                                  plot_value = plot_value, 
                                  sample_fixed = sample_fixed, 
                                  n_samples = n_samples, 
                                  variable_name = "Epsilon1_gct")
      if (any(c("dhat_ktp", "dpred_ktp") %in% names(Report))) 
        stop("Not implemented for SpatialVAM")
    }
    if (plot_num == 7) {
      if (quiet == FALSE) 
        message(" # plot_num ", plot_num, ": Plotting spatio-temporal effects (Epsilon) in 2nd linear predictor")
      if ("D_xt" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "Epsilon2_st")
      if ("D_xct" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "Epsilon2_sct")
      if ("D_xcy" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "Epsilon2_sct")
      if (any(c("D_gcy", "D_gct") %in% names(Report))) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "Epsilon2_gct")
      if (any(c("dhat_ktp", "dpred_ktp") %in% names(Report))) 
        stop("Not implemented for SpatialVAM")
    }
    if (plot_num == 8) {
      if (quiet == FALSE) 
        message(" # plot_num ", plot_num, ": Plotting 1st predictor after action of link function")
      if ("D_xt" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "P1_xt")
      if ("D_xct" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "P1_xct")
      if ("D_xcy" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "P1_xcy")
      if (any(c("D_gcy", "D_gct") %in% names(Report))) 
        stop("`plot_maps` not implemented for requested plot_num")
      if (any(c("dhat_ktp", "dpred_ktp") %in% names(Report))) 
        stop("Not implemented for SpatialVAM")
    }
    if (plot_num == 9) {
      if (quiet == FALSE) 
        message(" # plot_num ", plot_num, ": Plotting 2nd predictor after action of link function")
      if ("D_xt" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "P2_xt")
      if ("D_xct" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "P2_xct")
      if ("D_xcy" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Obj = Obj, Report = Report, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "P2_xcy")
      if (any(c("D_gcy", "D_gct") %in% names(Report))) 
        stop("`plot_maps` not implemented for requested plot_num")
      if (any(c("dhat_ktp", "dpred_ktp") %in% names(Report))) 
        stop("Not implemented for SpatialVAM")
    }
    if (plot_num == 10) {
      stop("`plot_num=10` is deprecated")
    }
    if (plot_num == 11) {
      if (quiet == FALSE) 
        message(" # plot_num ", plot_num, ": Plotting covariates for 1st linear predictor")
      if (plot_value != "estimate") 
        stop("Must use `plot_value='estimate'` for `plot_num=11`")
      if (is.null(TmbData)) 
        stop("Must provide `TmbData` to plot covariates")
      if ("X_xtp" %in% names(TmbData)) 
        Array_xct = aperm(TmbData$X_xtp, perm = c(1, 
                                                  3, 2))
      if ("X_gtp" %in% names(TmbData)) 
        Array_xct = aperm(TmbData$X_gtp, perm = c(1, 
                                                  3, 2))
      if ("X_gctp" %in% names(TmbData)) 
        Array_xct = aperm(array(TmbData$X_gctp[, 1, , 
        ], dim(TmbData$X_gctp)[c(1, 3, 4)]), perm = c(1, 
                                                      3, 2))
      if ("X1_gctp" %in% names(TmbData)) 
        Array_xct = aperm(array(TmbData$X1_gctp[, 1, 
                                                , ], dim(TmbData$X1_gctp)[c(1, 3, 4)]), perm = c(1, 
                                                                                                 3, 2))
      category_names = seq_len(dim(Array_xct)[2])
    }
    if (plot_num == 12) {
      if (quiet == FALSE) 
        message(" # plot_num ", plot_num, ": Plotting covariates for 2nd linear predictor")
      if (plot_value != "estimate") 
        stop("Must use `plot_value='estimate'` for `plot_num=12`")
      if (is.null(TmbData)) 
        stop("Must provide `TmbData` to plot covariates")
      if ("X2_gctp" %in% names(TmbData)) 
        Array_xct = aperm(array(TmbData$X2_gctp[, 1, 
                                                , ], dim(TmbData$X2_gctp)[c(1, 3, 4)]), perm = c(1, 
                                                                                                 3, 2))
      category_names = seq_len(dim(Array_xct)[2])
    }
    if (plot_num == 13) {
      if (quiet == FALSE) 
        message(" # plot_num ", plot_num, ": Plotting total density")
      if (plot_value != "estimate") 
        stop("Must use `plot_value='estimate'` for `plot_num=13`")
      if ("D_xt" %in% names(Report)) 
        Array_xct = log(Report$D_xt)
      if ("D_xct" %in% names(Report)) 
        Array_xct = log(apply(Report$D_xct, FUN = sum, 
                              MARGIN = c(1, 3)))
      if ("D_xcy" %in% names(Report)) 
        Array_xct = log(apply(Report$D_xcy, FUN = sum, 
                              MARGIN = c(1, 3)))
      if ("D_gcy" %in% names(Report)) 
        Array_xct = log(apply(Report$D_gcy, FUN = sum, 
                              MARGIN = c(1, 3)))
      if ("D_gct" %in% names(Report)) 
        Array_xct = log(apply(Report$D_gct, FUN = sum, 
                              MARGIN = c(1, 3)))
      logsum = function(vec) {
        max(vec) + log(sum(exp(vec - max(vec))))
      }
      if ("dhat_ktp" %in% names(Report)) 
        Array_xct = apply(aperm(Report$dhat_ktp, c(1, 
                                                   3, 2)), FUN = logsum, MARGIN = c(1, 3))
      if ("dpred_ktp" %in% names(Report)) 
        Array_xct = apply(aperm(Report$dpred_ktp, c(1, 
                                                    3, 2)), FUN = logsum, MARGIN = c(1, 3))
    }
    if (plot_num == 14) {
      if (quiet == FALSE) 
        message(" # plot_num ", plot_num, ": Plotting covariate effects for 1st linear predictor")
      if ("D_xt" %in% names(Report)) 
        stop()
      if ("D_xct" %in% names(Report)) 
        stop()
      if ("D_xcy" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Report = Report, Obj = Obj, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "eta1_xct")
      if (any(c("D_gcy", "D_gct") %in% names(Report))) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Report = Report, Obj = Obj, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "eta1_gct")
      if ("dhat_ktp" %in% names(Report)) 
        stop()
      if ("dpred_ktp" %in% names(Report)) 
        stop()
    }
    if (plot_num == 15) {
      if (quiet == FALSE) 
        message(" # plot_num ", plot_num, ": Plotting covariate effects for 2nd linear predictor")
      if ("D_xt" %in% names(Report)) 
        stop()
      if ("D_xct" %in% names(Report)) 
        stop()
      if ("D_xcy" %in% names(Report)) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Report = Report, Obj = Obj, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "eta2_xct")
      if (any(c("D_gcy", "D_gct") %in% names(Report))) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Report = Report, Obj = Obj, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "eta2_gct")
      if ("dhat_ktp" %in% names(Report)) 
        stop()
      if ("dpred_ktp" %in% names(Report)) 
        stop()
    }
    if (plot_num == 16) {
      if (quiet == FALSE) 
        message(" # plot_num ", plot_num, ": plotting spatial effects (Omega) for 1st linear predictor")
      if ("D_xt" %in% names(Report)) 
        stop()
      if ("D_xct" %in% names(Report)) 
        stop()
      if ("D_xcy" %in% names(Report)) 
        Array_xct = Report$Omega1_sc %o% 1
      if (any(c("D_gcy", "D_gct") %in% names(Report))) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Report = Report, Obj = Obj, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "Omega1_gc") %o% 1
      if ("dhat_ktp" %in% names(Report)) 
        stop()
      if ("dpred_ktp" %in% names(Report)) 
        stop()
    }
    if (plot_num == 17) {
      if (quiet == FALSE) 
        message(" # plot_num ", plot_num, ": plotting spatial effects (Omega) for 2nd linear predictor")
      if ("D_xt" %in% names(Report)) 
        stop()
      if ("D_xct" %in% names(Report)) 
        stop()
      if ("D_xcy" %in% names(Report)) 
        Array_xct = Report$Omega2_sc %o% 1
      if (any(c("D_gcy", "D_gct") %in% names(Report))) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Report = Report, Obj = Obj, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "Omega2_gc") %o% 1
      if ("dhat_ktp" %in% names(Report)) 
        stop()
      if ("dpred_ktp" %in% names(Report)) 
        stop()
    }
    if (plot_num == 18) {
      if (quiet == FALSE) 
        message(" # plot_num ", plot_num, ": plotting spatially-varying response to density covariates (Xi) for 1st linear predictor")
      if ("D_xt" %in% names(Report)) 
        stop()
      if ("D_xct" %in% names(Report)) 
        stop()
      if ("D_xcy" %in% names(Report)) 
        stop()
      if (any(c("D_gcy", "D_gct") %in% names(Report))) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Report = Report, Obj = Obj, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "Xi1_gcp")
      if ("dhat_ktp" %in% names(Report)) 
        stop()
      if ("dpred_ktp" %in% names(Report)) 
        stop()
    }
    if (plot_num == 19) {
      if (quiet == FALSE) 
        message(" # plot_num ", plot_num, ": plotting spatially-varying response to density covariates (Xi) for 2nd linear predictor")
      if ("D_xt" %in% names(Report)) 
        stop()
      if ("D_xct" %in% names(Report)) 
        stop()
      if ("D_xcy" %in% names(Report)) 
        stop()
      if (any(c("D_gcy", "D_gct") %in% names(Report))) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Report = Report, Obj = Obj, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "Xi2_gcp")
      if ("dhat_ktp" %in% names(Report)) 
        stop()
      if ("dpred_ktp" %in% names(Report)) 
        stop()
    }
    if (plot_num == 20) {
      if (quiet == FALSE) 
        message(" # plot_num ", plot_num, ": plotting spatially-varying response to catchability covariates (Phi) for 1st linear predictor")
      if ("D_xt" %in% names(Report)) 
        stop()
      if ("D_xct" %in% names(Report)) 
        stop()
      if ("D_xcy" %in% names(Report)) 
        stop()
      if (any(c("Phi1_gk") %in% names(Report))) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Report = Report, Obj = Obj, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "Phi1_gk")
      if ("dhat_ktp" %in% names(Report)) 
        stop()
      if ("dpred_ktp" %in% names(Report)) 
        stop()
      Array_xct = aperm(Array_xct %o% 1, c(1, 3, 2))
    }
    if (plot_num == 21) {
      if (quiet == FALSE) 
        message(" # plot_num ", plot_num, ": plotting spatially-varying response to catchability covariates (Phi) for 2nd linear predictor")
      if ("D_xt" %in% names(Report)) 
        stop()
      if ("D_xct" %in% names(Report)) 
        stop()
      if ("D_xcy" %in% names(Report)) 
        stop()
      if (any(c("Phi2_gk") %in% names(Report))) 
        Array_xct = extract_value(Sdreport = Sdreport, 
                                  Report = Report, Obj = Obj, plot_value = plot_value, 
                                  sample_fixed = sample_fixed, n_samples = n_samples, 
                                  variable_name = "Phi2_gk")
      if ("dhat_ktp" %in% names(Report)) 
        stop()
      if ("dpred_ktp" %in% names(Report)) 
        stop()
      Array_xct = aperm(Array_xct %o% 1, c(1, 3, 2))
    }
    Array_xct = strip_units(Array_xct)
    Array_xct = ifelse(Array_xct == -Inf, NA, Array_xct)
    Ncategories = dim(Array_xct)[2]
    Nyears = dim(Array_xct)[3]
    if (is.null(Array_xct)) 
      stop("Problem with `plot_num` in `plot_maps(.)")
    Bad_xct = ifelse(is.na(Array_xct), FALSE, Array_xct == 
                       Inf)
    if (any(Bad_xct)) 
      stop("plot_maps(.) has some element of output that is Inf or -Inf, please check results")
    years_to_plot_modified = years_to_plot
    if (names(dimnames(fit$Report$D_gct))[3] != "Time") {
      years_to_plot_modified = NULL
    }
    if (!all(years_to_plot_modified %in% seq_len(dim(Array_xct)[3]))) {
      years_to_plot_modified = NULL
    }
    if (is.null(years_to_plot_modified)) 
      years_to_plot_modified = seq_len(dim(Array_xct)[3])
    year_labels_modified = dimnames(Array_xct)[[3]]
    category_names_modified = dimnames(Array_xct)[[2]]
    if (tolower(Panel) == "category" & all(dim(Array_xct) > 
                                           0)) {
      if (length(dim(Array_xct)) == 2) 
        Nplot = 1
      if (length(dim(Array_xct)) == 3) 
        Nplot = dim(Array_xct)[2]
      for (cI in 1:Nplot) {
        if (length(dim(Array_xct)) == 2) 
          Return = Mat_xt = Array_xct
        if (length(dim(Array_xct)) == 3) 
          Return = Mat_xt = array(as.vector(Array_xct[, 
                                                      cI, ]), dim = dim(Array_xct)[c(1, 3)])
        panel_labels = year_labels_modified[years_to_plot_modified]
        file_name = paste0(plot_code, ifelse(Nplot > 
                                               1, paste0("--", category_names_modified[cI]), 
                                             ""), ifelse(is.function(plot_value), "-transformed", 
                                                         "-predicted"))
        plot_args = plot_variable(Y_gt = Mat_xt[, years_to_plot_modified, 
                                                drop = FALSE], map_list = list(PlotDF = PlotDF, 
                                                                               MapSizeRatio = MapSizeRatio), projargs = projargs, 
                                  working_dir = working_dir, panel_labels = panel_labels, 
                                  file_name = file_name, n_cells = n_cells, zlim = zlim, 
                                  country = country, ...)
      }
    }
    if (tolower(Panel) == "year" & all(dim(Array_xct) > 0)) {
      Nplot = length(years_to_plot_modified)
      for (tI in 1:Nplot) {
        if (length(dim(Array_xct)) == 2) 
          Mat_xc = Array_xct[, years_to_plot_modified[tI], 
                             drop = TRUE]
        if (length(dim(Array_xct)) == 3) 
          Mat_xc = Array_xct[, , years_to_plot_modified[tI], 
                             drop = TRUE]
        Return = Mat_xc = array(as.vector(Mat_xc), dim = c(dim(Array_xct)[1], 
                                                           Ncategories))
        file_name = paste0(plot_code, ifelse(Nplot > 
                                               1, paste0("--", year_labels_modified[years_to_plot_modified][tI]), 
                                             ""), ifelse(is.function(plot_value), "-transformed", 
                                                         "-predicted"))
        plot_args = plot_variable(Y_gt = Mat_xc, map_list = list(PlotDF = PlotDF, 
                                                                 MapSizeRatio = MapSizeRatio), projargs = projargs, 
                                  working_dir = working_dir, panel_labels = category_names_modified, 
                                  file_name = file_name, n_cells = n_cells, zlim = zlim, 
                                  country = country, ...)
      }
    }
  }
  if (is.null(Return) & quiet == FALSE) 
    message(" # No available plots selected in `plot_set`")
  return(invisible(Return))
}
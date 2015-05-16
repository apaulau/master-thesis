# This function automatically fits a variogram to input_data
autofitVariogram = function(formula, input_data, 
  test_models = c("Nug", "Exp", "Sph", "Gau", "Cir", "Lin", "Bes", "Pen", "Per", "Wav", "Hol", "Log", "Spl"),
  kappa=c(0.05, seq(0.2, 2, 0.1), 5, 10), GLS.model=NA, 
  fix.values=c(NA,NA,NA), start_vals=c(NA,NA,NA),
  cutoff, width=1, cressie, verbose=FALSE, ...) {
  
  # If you specifiy a variogram model in GLS.model the Generelised Least Squares sample variogram is constructed
  if(!is(GLS.model, "variogramModel")) {
    experimental_variogram = variogram(formula, input_data, cutoff=cutoff, width=width, cressie=cressie,  ...)
  } else {
    g = gstat(NULL, "bla", formula, input_data, model=GLS.model, set=list(gls=1))
    experimental_variogram = variogram(g, cutoff=cutoff, width=width, cressie=TRUE, ...)
  }
  
  # set initial values
  if(is.na(start_vals[1])) {  # Nugget
    initial_nugget = min(experimental_variogram$gamma)
  } else {
    initial_nugget = start_vals[1]
  }
  if(is.na(start_vals[2])) { # Range
    diagonal = spDists(t(bbox(input_data)))[1,2]  # 0.35 times the length of the central axis through the area
    initial_range = 0.1 * diagonal   # 0.10 times the length of the central axis through the area
  } else {
    initial_range = start_vals[2]
  }
  if(is.na(start_vals[3])) { # Sill
    initial_sill = mean(c(max(experimental_variogram$gamma), median(experimental_variogram$gamma)))
  } else {
    initial_sill = start_vals[3]
  }
  
  # Determine what should be automatically fitted and what should be fixed
  # Nugget
  if(!is.na(fix.values[1])) {
    fit_nugget = FALSE
    initial_nugget = fix.values[1]
  } else {
    fit_nugget = TRUE
  }
  
  # Range
  if(!is.na(fix.values[2])) {
    fit_range = FALSE
    initial_range = fix.values[2]
  } else {
    fit_range = TRUE
  }
  
  # Partial sill
  if(!is.na(fix.values[3])) {
    fit_sill = FALSE
    initial_sill = fix.values[3]
  } else {
    fit_sill = TRUE
  }
  
  getModel <- function(psill, model, range, kappa, nugget, fit_range, fit_sill, fit_nugget) {
    if(model == "Pow") {
      if(is.na(start_vals[1])) nugget = 0
      if(is.na(start_vals[2])) range = 1    # If a power mode, range == 1 is a better start value
      if(is.na(start_vals[3])) sill = 1
    }
    if(model == "Nug") {
      if(is.na(start_vals[2])) range = 0
    }
    
    obj = try(fit.variogram(experimental_variogram,
      model = vgm(psill=psill, model=model, range=range,
        nugget=nugget,kappa = kappa),
      fit.ranges = c(fit_range), fit.sills = c(fit_nugget, fit_sill),
      debug.level=0), 
      silent=TRUE)
    if("try-error" %in% class(obj)) {
      #print(traceback())
      if (verbose) {
        warning("An error has occured during variogram fitting. Used:\n", 
          "\tnugget:\t", nugget, 
          "\n\tmodel:\t", model, 
          "\n\tpsill:\t", psill,
          "\n\trange:\t", range,
          "\n\tkappa:\t",ifelse(kappa == 0, NA, kappa),
          "\n  as initial guess. This particular variogram fit is not taken into account. \nGstat error:\n", obj)
      }
      return(NULL)
    } else return(obj)
  }
  
  # Automatically testing different models, the one with the smallest sums-of-squares is chosen
  SSerr_list = c()
  vgm_list = list()
  counter = 1
  
  for(m in test_models) {
    if(m != "Mat" && m != "Ste") {        # If not Matern and not Stein
      model_fit = getModel(initial_sill - initial_nugget, m, initial_range, kappa = 0, initial_nugget, fit_range, fit_sill, fit_nugget)
      if(!is.null(model_fit)) {  # skip models that failed
        vgm_list[[counter]] = model_fit
        SSerr_list = c(SSerr_list, attr(model_fit, "SSErr"))
      }
      counter = counter + 1
    } else {                 # Else loop also over kappa values
      for(k in kappa) {
        model_fit = getModel(initial_sill - initial_nugget, m, initial_range, k, initial_nugget, fit_range, fit_sill, fit_nugget)
        if(!is.null(model_fit)) {
          vgm_list[[counter]] = model_fit
          SSerr_list = c(SSerr_list, attr(model_fit, "SSErr"))
        }
        counter = counter + 1
      }
    }
  }
  
  # Check for negative values in sill or range coming from fit.variogram
  # and NULL values in vgm_list, and remove those with a warning
  strange_entries = sapply(vgm_list, function(v) any(c(v$psill, v$range) < 0) | is.null(v))
  if(any(strange_entries)) {
    if(verbose) {
      print(vgm_list[strange_entries])
      cat("^^^ ABOVE MODELS WERE REMOVED ^^^\n\n")
    }
    SSerr_list = SSerr_list[!strange_entries]
    vgm_list = vgm_list[!strange_entries]
  }
  
  if(verbose) {
    cat("Selected:\n")
    print(vgm_list[[which.min(SSerr_list)]])
    cat("\nTested models, best first:\n")
    tested = data.frame("Tested models" = sapply(vgm_list, function(x) as.character(x[2,1])), 
      kappa = sapply(vgm_list, function(x) as.character(x[2,4])), 
      "SSerror" = SSerr_list)
    tested = tested[order(tested$SSerror), ]
    print(tested)
  }
  
  result = list(exp_var = experimental_variogram, var_model = vgm_list[[which.min(SSerr_list)]], sserr = min(SSerr_list, na.rm=TRUE))
  class(result) = c("autofitVariogram", "list")    
  
  return(result)
}
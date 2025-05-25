# Application Programming Interface (API) for App
# This script defines a web app for the app.  
# Timothy Hackmann
# 13 May 25

#* @apiTitle Microbe Decoder API
#* @apiDescription Predicts traits of microbes from their taxonomy, with networks, or with machine learning

# === Load external R files ===
  # Define directories
    plumber_directory <- FileLocator::getCurrentFileLocation()
    assign("plumber_directory", FileLocator::getCurrentFileLocation(), envir = .GlobalEnv)
    main_directory <- gsub(paste0("/Plumber", "$"), "", plumber_directory)
    app_directory <- paste0(main_directory, "/Shiny/MicrobeDecoder/")
    
  # Load files from app
   setwd(app_directory)  

    source("functions/sourceFunctions.R")
    source_r_files(
      subdirs = c("install", "variables", "functions", "modules"),
      exclude = c("old"),
      verbose = TRUE,
      local = FALSE
    )

  # Load additional files for server
    setwd(plumber_directory)  
    source_r_files(
      subdirs = c("functions"),
      exclude = c("old"),
      verbose = TRUE,
      local = FALSE
    )
    
  # Set working directory again
  setwd(app_directory) 
    
# === Define API ===
  pr <- plumber::pr()
  pr <- plumber::pr_set_debug(pr)
  
  # --- Enable CORS ---
  pr$filter("cors", function(req, res, forward) {
    res$setHeader("Access-Control-Allow-Origin", "*")
    res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
    res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
    
    if (req$REQUEST_METHOD == "OPTIONS") {
      res$status <- 204
      return(res)
    }
    
    plumber::forward()
  })
  
  # --- Status Check ---
    pr$handle("GET", "/status", check_status)

  # --- Example requests ---  
    pr$handle("GET", "/examples", get_examples)
    
  # --- Compute ---
    pr$handle("POST", "/compute/taxonomy", compute_taxonomy)
    pr$handle("POST", "/compute/networks", compute_networks)
    pr$handle("POST", "/compute/ml", compute_ml)
  
  # --- Parameters ---
    pr$handle("GET", "/parameters/taxonomy", parameters_taxonomy)
    pr$handle("GET", "/parameters/networks", parameters_networks)
    pr$handle("GET", "/parameters/ml", parameters_ml)

# === Enable Swagger Docs ===
  pr <- pr %>% plumber::pr_set_docs("swagger")    
    
# === Run Server ===
  if (!interactive()) {
    pr$run(host = "0.0.0.0", port = 8000)
  } else {
    pr$run(host = "0.0.0.0", port = 8000)
  }
library(ForecastFramework)
library(R6)
library(forecast)
library(sarimaTD)

# SARIMATDModel Description:
# Implementation of the sarimaTD model created by Evan Ray
# More information about sarimaTD in https://github.com/reichlab/sarimaTD 
# Integrated into ForecastFramework by: Katie House, 6/22/2018

DL4EPI <- R6Class(
  inherit = ContestModel,
  private = list(
    .data = NULL,        ## every model should have this
    .models = list(),    ## specific to models that are fit separately for each location
    .nsim = 1000,        ## models that are simulating forecasts need this
    .period = integer(0) ## specific to SARIMA models
  ),
  public = list(
    ## data will be MatrixData
    fit = function(data) {
      if("fit" %in% private$.debug){browser()}
      #adjacency_matrix <- "./GER_states_adjacency.txt"
      adjacency_matrix <- training_data$metaData$adjacency_mat
      epochs <- 100
      train_size <- 0.6
      valid_size <- 0.2
      save_name <- "tmp"
      model_name <- "CNNRNN_Res"
      ## stores data for easy access and checks to make sure it's the right class
      private$.data <- IncidenceMatrix$new(data)
    
      ## for each location/row
      #for (row_idx in 1:private$.data$nrow) {
        ### need to create a y vector with incidence at time t
      #  y <- private$.data$subset(rows = row_idx, mutate = FALSE)
        
        ## convert y vector to time series data type
      #print (private$.data)
      y_ts <- ts(as.vector(private$.data$mat), frequency = private$.period)
      print (dim(y_ts))
        ## fit sarimaTD with 'fit_sarima()' from sarimaTD package
        ## fit_sarima() performs box-cox transformation and seasonal differencing
      private$.models[[1]] <- train_dl(y_ts,adjacency_matrix,train_size,valid_size,model_name,save_name)
      
    },
    forecast = function(newdata = private$.data, steps) {
      ## include for debugging
      if("forecast" %in% private$.debug){browser()} 
      
      ## number of models (provinces) to forecast
      nmodels <- length(private$.models)
      
      ## define an array to store the simulated forecasts
      sim_forecasts <- array(dim = c(nmodels, steps, private$.nsim))
      dimnames(sim_forecasts) <- list(newdata$rnames, 1:steps, NULL)
      
      sim_forecasts <- test(data,adjacency_matrix,train_size,valid_size,model_name,save_name,NULL,NULL)
      
      private$output <- SimulatedIncidenceMatrix$new(sim_forecasts)
      return(IncidenceForecast$new(private$output, forecastTimes = rep(TRUE, steps)))
    },
    initialize = function(period = 26, nsim=1000) { 
      ## this code is run during SARIMAModel$new()
      ## need to store these arguments within the model object
      private$.nsim <- nsim
      private$.period <- period
    },
    predict = function(newdata) {
      stop("predict method has not been written.")
    }
  ),
  active = list(
    ## This list determines how you can access pieces of your model object
    data = function(value) {
      ## use this form when you want this parameter to be un-modifiable
      if(!missing(value))
        stop("Writing directly to the data is not allowed.")
      return(private$.data)
    },
    models = function(value) {
      ## use this form when you want this parameter to be un-modifiable
      if(!missing(value))
        stop("Writing directly to the models is not allowed.")
      return(private$.models)
    },
    nsim = function(value) {
      ## use this form when you want to be able to change this parameter
      private$defaultActive(type="private", ".nsim", val=value)
    },
    period = function(value) {
      ## use this form when you want this parameter to be un-modifiable
      if(!missing(value))
        stop("Writing directly to the model period is not allowed.")
      return(private$.period)
    }
  )
)
# Load necessary packages
library(methods)
library(zoo)
library(ggplot2)
library(dplyr)
library(slider)

# Helper function to remove outliers using Z-score method
remove_outliers_z_score <- function(data, threshold = 3) {
  z_scores <- (data - mean(data, na.rm = TRUE)) / sd(data, na.rm = TRUE)
  data[abs(z_scores) > threshold] <- NA
  return(data)
}

# Seasonal Moving Average (SMA) function
sma <- function(data, lookback_win_size) { 
  data_clean <- remove_outliers_z_score(data)
  rolling_mean <- slide_dbl(
    .x = data_clean,
    .f = ~ mean(.x, na.rm = TRUE),
    .before = lookback_win_size - 1, # includes the current element and win-1 previous elements
    .after = 0,
    .complete = TRUE
  )

  deseasonalized <- data - rolling_mean
  
  sma_data <- data.frame(
    adjusted_obs = data,
    deseasonalized = deseasonalized,
    rolling_mean = rolling_mean
  )
  
  return(sma_data)
}

calculate_moving_var <- function(deseasonalized, lookback_win_size) {
  slide_dbl(
    .x = tail(deseasonalized, -lookback_win_size),
    .f = ~{
      clean_window <- remove_outliers_z_score(.x)
      var(clean_window, na.rm = TRUE)
    },
    .before = lookback_win_size - 1,
    .after = 0,
    .complete = TRUE
  )
}


# Function to calculate mu_w
mu_w_calc <- function(beta, obs_24h_mean, p) {
  w_mean <- -beta + (beta * obs_24h_mean) / p
  return(w_mean)
}

var_w_calc <- function(beta, obs_diff_var, obs_24h_var, p) {
  w_var <- obs_diff_var + (beta^2 * obs_24h_var) / p^2
  return(w_var)
}

z_var_calc <- function(w_var, lambda, n, num_new_zs) {
  indices_current_run <- tail(n, num_new_zs)
  w_var_current_run <- tail(w_var, num_new_zs)
  z_var <- w_var_current_run * (lambda / (2 - lambda)) *
    (1 - (1 - lambda)^(2 * indices_current_run))
  return(z_var)
}

limits_calc <- function(z_mean, L, z_var, num_new_zs) {
  z_mean_current_run <- tail(z_mean, num_new_zs)
  z_sigma <- sqrt(z_var)
  UL <- z_mean_current_run + (L * z_sigma)
  LL <- z_mean_current_run - (L * z_sigma)
  return(list(UL = UL, LL = LL))
}

check_signals <- function(z, UL, LL) {   
  signals <- (z > UL) | (z < LL)
  return(signals)
}

# Define the CHEWMA class
setClass(
  "CHEWMA",
  slots = list(
    obs = "numeric",
    timestamps = "POSIXct",
    obs_mean = "numeric",
    obs_diff = "numeric",
    obs_24hs = "numeric",
    z = "numeric",
    obs_diff_var = "numeric",
    L = "numeric",
    beta = "numeric",
    p = "numeric",
    mean_type = "numeric",
    UL = "numeric",
    LL = "numeric",
    signals = "logical",
    lambda = "numeric",
    var_data = "data.frame",
    phase1_data = "data.frame",
    phase1_process_data = "data.frame",
    phase2_data = "data.frame"
  ),
  prototype = list(
    obs = numeric(),
    timestamps = as.POSIXct(character()),
    obs_mean = numeric(),
    obs_diff = numeric(),
    obs_24hs = numeric(),
    z = numeric(),
    obs_diff_var = NA_real_,
    L = 3,
    beta = 0,
    p = 1,
    mean_type = 1,
    UL = numeric(),
    LL = numeric(),
    signals = logical(),
    lambda = 0.2,
    var_data = data.frame(),
    phase1_data = data.frame(),
    phase1_process_data = data.frame(),
    phase2_data = data.frame()
  )
)

# Initialize method for CHEWMA
setMethod("initialize", "CHEWMA",
          function(.Object, obs = numeric(), timestamps = as.POSIXct(character()),
                   L = 3, lambda = 0.2, beta = 0, p = 1, mean_type = 1, ...) {
            .Object@L <- L
            .Object@lambda <- lambda
            .Object@beta <- beta
            .Object@p <- p
            .Object@mean_type <- mean_type
            callNextMethod()
          })

# Function to create a CHEWMA object
createCHEWMA <- function(obs, timestamps, L = 3, lambda = 0.2, beta = 0, p = 1,
                         mean_type = 1) {
  # Create a new CHEWMA object
  chewma <- new("CHEWMA",
               L = L,
               lambda = lambda,
               beta = beta,
               p = p,
               mean_type = mean_type)
  
  chewma <- build_phase1(chewma, obs)
  
  return(chewma)
}

# Define generic for build_phase1
setGeneric("build_phase1", function(object, obs) { 
  standardGeneric("build_phase1")
})

setMethod("build_phase1", "CHEWMA", function(object, obs) {
  obs_diff <- diff(obs)
  obs_diff_clean <- remove_outliers_z_score(obs_diff)
  object@obs_diff_var <- var(obs_diff_clean, na.rm = TRUE)

  object@obs_mean <- mean(obs, na.rm = TRUE)
  
  return(object)
})

update_z_statistic <- function(last_z, num_new_zs, obs_diff, obs_24h_mean, lambda, beta, p) {
  obs_diff_current_run <- tail(obs_diff, num_new_zs)
  obs_24h_mean_current_run <- tail(obs_24h_mean, num_new_zs)

  is_first_run <- length(last_z) == 0
  if (is_first_run) {
    last_z <- mu_w_calc(beta, tail(obs_24h_mean_current_run, 1), p)
  }

  calc_new_z <- function(w, last_z) {
    if (is.na(w)) {
      return(NA)
    }
    lambda * w + (1 - lambda) * last_z
  }

  w <- obs_diff_current_run + beta * ((obs_24h_mean_current_run / p) - 1)

  new_zs <- Reduce(
    f = function(last_z, w_i) calc_new_z(w_i, last_z),
    x = w,
    init = last_z,
    accumulate = TRUE
  )

  new_zs <- new_zs[-1]

  return(new_zs)
}

calc_window_parameters <- function(obs, new_obs, obs_per_day, lookback_days) {
  if (!is.numeric(obs)) stop("`obs` debe ser un vector numérico.")
  if (!is.numeric(new_obs)) stop("`new_obs` debe ser un vector numérico.")
  if (!is.numeric(obs_per_day) || length(obs_per_day) != 1) stop("`obs_per_day` debe ser un número.")
  if (!is.numeric(lookback_days) || length(lookback_days) != 1) stop("`lookback_days` debe ser un número.")

  total_observations <- length(obs)
  lookback_win_size <- lookback_days * obs_per_day
  combined_win_size <- lookback_win_size * 2

  num_new_obs <- length(new_obs)
  num_prev_obs <- total_observations - num_new_obs
  num_obs_current_run <- combined_win_size + num_new_obs - max(0, combined_win_size - num_prev_obs - 1)
  
  num_new_zs <- num_obs_current_run - combined_win_size
  obs_current_run <- tail(obs, num_obs_current_run)

  return(list(total_observations = total_observations, combined_win_size = combined_win_size,
              lookback_win_size = lookback_win_size, num_new_zs = num_new_zs, obs_current_run = obs_current_run))
}

# TODO: Describe normal behaviour dealing with missing values
# TODO: Implement the inmediate start if beta = 0
update_phase2_data <- function(phase2_data, num_new_zs, obs, timestamps, obs_24h_mean,
                               obs_24h_var, z, z_mean, UL, LL, signals) {
  new_phase2_data <- data.frame(
    timestamps = tail(timestamps, num_new_zs),
    obs = tail(obs, num_new_zs),
    obs_24h_mean = tail(obs_24h_mean, num_new_zs),
    obs_24h_var = tail(obs_24h_var, num_new_zs),
    z = tail(z, num_new_zs),
    z_mean = tail(z_mean, num_new_zs),
    UL = tail(UL, num_new_zs),
    LL = tail(LL, num_new_zs),
    signals = tail(signals, num_new_zs)
  )
  
  phase2_data <- bind_rows(phase2_data, new_phase2_data)
  
  return(phase2_data)
}

# Define generic for update
#setGeneric("update", function(object, new_obs, new_timestamps, lookback_days = 14,
#                              obs_per_day = 24, info = FALSE) { 
#  standardGeneric("update") 
#})

setGeneric("update", function(object, ...) { 
  standardGeneric("update") 
})

# Función `update` recodificada
setMethod("update", "CHEWMA", function(object, new_obs, new_timestamps, lookback_days = 14,
                                       obs_per_day = 24, info = FALSE) {
  if (!is.numeric(new_obs)) stop("obs must be numeric")
  if (!inherits(new_timestamps, "POSIXct")) stop("timestamps must be POSIXct")
  if (length(new_obs) != length(new_timestamps)) stop("obs and timestamps must have the same length")

  lambda <- object@lambda
  beta <- object@beta
  p <- object@p
  L <- object@L
  obs_diff_var <- object@obs_diff_var
  obs_mean <- object@obs_mean
  
  object@obs <- c(object@obs, new_obs)
  object@timestamps <- c(object@timestamps, new_timestamps)

  params <- calc_window_parameters(object@obs, new_obs, obs_per_day, lookback_days)

  total_observations <- params$total_observations
  combined_win_size <- params$combined_win_size
  lookback_win_size <- params$lookback_win_size
  num_new_zs <- params$num_new_zs
  obs_current_run <- params$obs_current_run

  is_enough_for_run <- combined_win_size <= total_observations
  if (!is_enough_for_run) {
    warning("Not enough observations for update. Need at least ", combined_win_size, " observations.")
    return(object)
  }  

  obs_diff <- c(0, diff(object@obs))

  sma_data <- sma(obs_current_run, lookback_win_size)
  
  obs_24h_mean <- sma_data$rolling_mean
  obs_24h_var <- calculate_moving_var(sma_data$deseasonalized, lookback_win_size)

  w_mean_modified <- mu_w_calc(beta, obs_24h_mean, obs_mean) #p)
  w_var <- var_w_calc(beta, obs_diff_var, obs_24h_var, p)
  
  z_mean <- w_mean_modified
  n <- seq_len(total_observations - combined_win_size + 1)
  z_var <- z_var_calc(w_var, lambda, n, num_new_zs)
  
  limits <- limits_calc(z_mean, L, z_var, num_new_zs)
  UL <- limits$UL
  LL <- limits$LL

  last_z <- tail(object@z, 1)
  new_zs <- update_z_statistic(last_z, num_new_zs, obs_diff, obs_24h_mean, lambda, beta, p)
  object@z <- c(object@z, new_zs)

  signals <- check_signals(new_zs, UL, LL)
  object@signals <- c(object@signals, signals)

  object@phase2_data <- update_phase2_data(object@phase2_data, num_new_zs, object@obs, object@timestamps, 
                                           obs_24h_mean, obs_24h_var, object@z, z_mean, UL, LL, signals)
  
  return(object) 
})
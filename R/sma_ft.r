sma_ft <- function(data, date_time, lookback_days, obs_per_day, percentil, UL = NULL, LL = NULL) {
  if (length(data) != length(date_time)) {
    stop("Data and date_time must have the same length")
  }
  if (!is.numeric(obs_per_day) || length(obs_per_day) != 1) {
    stop("obs_per_day must be a single numeric value.")
  }
  if (!is.na(percentil) && (percentil < 0 || percentil > 100)) { 
    stop("percentil must be between 0 and 100.")
  }

  percentil <- percentil / 100

  date_time <- as.POSIXct(date_time)
  
  width <- lookback_days * obs_per_day
  
  rolling_mean <- rollapply(data, width = width, FUN = mean, align = "right", fill = NA)

  date_time <- date_time[-(1:width)]
  deseasonalized <- data - rolling_mean
  deseasonalized <- deseasonalized[-(1:width)]
  adjusted_ts <- data[-(1:width)]
  rolling_mean <- rolling_mean[-(1:width)]
  
  if (is.null(UL) || is.null(LL)) {
    UL <- quantile(deseasonalized, 1 - percentil, na.rm = TRUE)
    LL <- quantile(deseasonalized, percentil, na.rm = TRUE)
  }
  
  rolling_mean_vector <- unlist(rolling_mean)
  UL_seas <- UL + rolling_mean_vector
  LL_seas <- LL + rolling_mean_vector
  
  signals <- deseasonalized > UL | deseasonalized < LL
  
  sma_data <- data.frame(
    date_time = date_time,
    metric = adjusted_ts,
    deseasonalized = deseasonalized,
    rolling_mean = rolling_mean,
    signals = signals,
    UL = rep(UL, length(date_time)),
    LL = rep(LL, length(date_time)),
    UL_seas = UL_seas,
    LL_seas = LL_seas
  )
  
  return(sma_data)
}
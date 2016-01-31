full_time <- function(date, time, base){
  return(difftime(date, base, units = "mins") + 60 * time)
}

daily_time <- function(t) (t / 60) %% 24

date.ix <- function(date, base){
  return(1 + as.integer(difftime(date, base, units = "days")))
}

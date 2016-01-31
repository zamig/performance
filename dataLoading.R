
readShifts <- function(file_name){
  
  require(dplyr)
  require(lubridate)
  #file_name = "data/Shifts_Mario_01-15_21-01-16.csv"
  
  d <- read.csv(file_name, stringsAsFactor = F)
  d <- d[, c(1, 3, 4)]
  d <- d %>% mutate(
    start_date = mdy_hms(StartDate),
    end_date = mdy_hms(EndDate)
  ) %>% arrange(start_date)
  d$StartDate = NULL; d$EndDate = NULL

  return(d)
}

readShiftsFiles <- function(files_name){
  bind_rows(lapply(files_name, readShifts))
}

readTripsFile <- function(file_name){
  
  require(dplyr)
  require(lubridate)
  #file_name = "data/Trips_Mario_1_15.csv"
  
  d <- read.csv(file_name, stringsAsFactor = F)
  d <- d[, c(2, 3, 9)]
  d <- d %>% mutate(
    start_date = mdy_hms(STARTDATE),
    end_date = mdy_hms(ENDDATE),
    inc = Trips
    ) %>% arrange(start_date)
  d$STARTDATE = NULL; d$ENDDATE = NULL; d$Trips = NULL
  return(d)
  }

readMonthlyTrips <- function(m, y){
  
  file_name = paste0("data/Trips_Mario_", m, "_", y, ".csv")
  return(readTripsFile(file_name))
  
}

readTrips <- function(months_list, s_data){
  
  t = bind_rows(apply(months_list, 1, function(x) readMonthlyTrips(x[1], x[2])))
  
  t$next_shift = rbind(data.frame(next_shift = t$start_date[-1]), data.frame(next_shift = t$end_date[nrow(t)]))[[1]]
  err = (t$end_date > t$next_shift)
  t$end_date[err] = t$next_shift[err]
  t$shift_id = s_data$ShiftID[findInterval(t$start_date, s_data$start_date)]
  t$shift_start = s_data$start_date[findInterval(t$start_date, s_data$start_date)]
  t$shift_end = s_data$end_date[findInterval(t$start_date, s_data$start_date)]
  t$end_err = t$end_date > t$shift_end
  
  return(t)
}

fixShifts <- function(s_data, t_data){
  
  e_data = t_data[, c("shift_id", "end_date")]
  colnames(e_data) = c("ShiftID", "trip_end_date")
  e_data = e_data %>% group_by(ShiftID) %>% summarise(trip_end_date = max(trip_end_date))
  s_data = merge(s_data, e_data, all.x =T)
  err = !is.na(s_data$trip_end_date) & s_data$trip_end_date > s_data$end_date
  s_data$end_date[err] = s_data$trip_end_date[err]
  #s_data$trip_end_date = NULL
  s_data$next_shift = rbind(data.frame(next_shift = s_data$start_date[-1]), 
                            data.frame(next_shift = s_data$end_date[nrow(s_data)]))[[1]]
  err = (s_data$end_date > s_data$next_shift)
  s_data$end_date[err] = s_data$next_shift[err]
  s_data$extra_time = difftime(s_data$end_date, s_data$trip_end_date, units = "mins")
  outliers = chk_outliers(as.numeric(s_data$extra_time), ICQ.DIFF)
  s_data$end_date[outliers$outliers] = s_data$trip_end_date[outliers$outliers] + outliers$up_th * 60
  s_data$extra_time = NULL
  return(s_data)  
}

extraVars <- function(d){
  
  d$s_date = as.Date(d$start_date)
  d$e_date = as.Date(d$end_date)
  d$duration = difftime(d$end_date, d$start_date, units = "mins")
  d$weekday = factor(weekdays(d$s_date), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  return(d)
}

addTimes <- function(d, base){
  
  d$start_time = difftime(d$start_date, base, units = "mins")
  d$end_time = difftime(d$end_date, base, units = "mins")
  return(d)
}


hour_ind_func <- function(h, data_point, base, window = 0){
  tot = 0 
  d = data_point$s_date
  start_time = data_point$start_time
  end_time = data_point$end_time
  t = full_time(d, h, base)
  if (start_time <= t + window & end_time >= t - window){
    tot = tot +1
  } 
  if (data_point$e_date > d){
    d = data_point$e_date
    t = full_time(d, h, base)
    if (start_time <= t + window & end_time >= t - window){
      tot = tot +1
    } 
  }
  return(tot)
}


readData <- function(shifts_files_names, trips_months){
  
  s_data = readShiftsFiles(shifts_files_names)
  t_data = readTrips(trips_months, s_data)
  s_data = fixShifts(s_data, t_data)
  s_data = extraVars(s_data)
  t_data = extraVars(t_data)
  base.date = min(min(s_data$s_date), min(t_data$s_date))
  s_data = addTimes(s_data, base.date)
  t_data = addTimes(t_data, base.date)
  
  shifts_hours = sapply(0:23, function(h) 
    sum(by(s_data, 1:nrow(s_data), function(x) hour_ind_func(h, x, base.date, 30))))
  trips_hours = sapply(0:23, function(h) 
    sum(by(t_data, 1:nrow(t_data), function(x) hour_ind_func(h, x, base.date, 30))))
  names(shifts_hours) = 0:23
  names(trips_hours) = 0:23
  
  return(list(shifts = s_data, trips = t_data, base.date = base.date, shifts_hours = shifts_hours, trips_hours = trips_hours))
  
}
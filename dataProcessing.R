
#################################################################################
#############  Value Functions                                 ##################
#################################################################################

windowed_length <- function(t){
  
  t = as.numeric(t)
  H = USED.TIMES$window[USED.TIMES$time == daily_time(t)]
  function(data_point){
    lower = as.numeric(data_point$start_time)
    upper = as.numeric(data_point$end_time)
#     print(t)
#     print(lower)
#     print(upper)
#     print(min(t + H, upper))
#     print(max(t - H, lower))
#     print(H)
    return(max(0, min(t + H, upper)- max(t - H, lower)))
  }
}

windowed_inc <- function(t){
  
  t = as.numeric(t)
  H = USED.TIMES$window[USED.TIMES$time == daily_time(t)]
  function(data_point){
    lower = as.numeric(data_point$start_time)
    upper = as.numeric(data_point$end_time)
    inc = data_point$inc
    if (upper <= lower){
      return(0)
    } else {
      return(max(0, min(t + H, upper)- max(t - H, lower))/(upper - lower) * inc)
    }
  }
}

#################################################################################

create_mat <- function(data, base, FUN){
  
  require(zoo)
  timesSeq = USED.TIMES$time
  n.times = length(timesSeq)
  datesSeq = seq.Date(data$s_date[1], tail(data$s_date,1), by = "day")
  n.dates = length(datesSeq)
  mat = matrix(0, nrow = n.dates, ncol = n.times)
  mat = zoo(mat, datesSeq)
  colnames(mat) = timesSeq
  for (d in datesSeq){
    for (t in 1:n.times){
      rel.data = data[data$s_date <= d & data$e_date >= d, ]
      d = as.Date(d)
      if (nrow(rel.data)){
        mat[date.ix(d, base), t] = sum(by(rel.data, 1:nrow(rel.data), FUN(full_time(d, timesSeq[t], base))))
      }
    }
  }
  return(mat)
}

create_time_mats <- function(data, names = c("shifts_len", "trips_len", "total_inc"), 
                             d_source = c("shifts", "trips", "trips"), 
                             functions = c(windowed_length, windowed_length, windowed_inc)){
  n = length(names)
  mats = lapply(1:n, function(x) create_mat(data[[d_source[x]]], data$base.date, functions[[x]]))
  names(mats) = names
  return(mats)
}

create_rates <- function(mats, data = c("trips_len", "total_inc"), 
                         bases = c("shifts_len", "shifts_len"),
                         names = c("occup", "inc"), app.date = as.Date("2015-11-23")){
  
  div_mats <- function(data, base){
    ind = index(data)
    m = data/base
    m = ts(m)
    m[is.nan(m)] = NA
    m = zoo(m, ind)
    return(m)
  }
  
  calc_ecdf <- function(mat, sd = 0.01){
    n_mat = matrix(rnorm(nrow(mat) * ncol(mat), sd = sd), nrow = nrow(mat))
    apply(mat + n_mat, 2, ecdf)
  }
  

  calc_rates <- function(mat, ecdf_fun, dates){
    n.times = length(USED.TIMES$time)
    zoo(sapply(1:n.times, function(x) ecdf_fun[[x]](mat[, x])), dates)
  }
  
  vect_data <- function(mat){
    v = as.vector(t(mat))
    names(v) = as.vector(t(outer(index(mat), USED.TIMES$time, paste, sep = ".")))
    return(v)
  }
  
  n = length(data)
  datesSeq = index(mats[[1]])
  dates_weekdays = !wday(datesSeq) %in% c(1,7)
  dates_weekends = wday(datesSeq) %in% c(1,7)
  dates_pre = datesSeq < app.date
  dates_post = datesSeq >= app.date
  
  raw_data = lapply(1:n, function(x) div_mats(mats[[data[x]]], mats[[bases[x]]]))
  names(raw_data) = paste0(names, "_raw")
  
  raw_pre = lapply(raw_data, function(x) x[dates_pre])
  raw_post = lapply(raw_data, function(x) x[dates_post])
  names(raw_pre) = paste0(names, "_raw_pre")
  names(raw_post) = paste0(names, "_raw_post")
  
  raw_weekdays = lapply(raw_data, function(x) x[dates_weekdays])
  raw_weekends = lapply(raw_data, function(x) x[dates_weekends])
  names(raw_weekdays) = paste0(names, "_raw_weekdays")
  names(raw_weekends) = paste0(names, "_raw_weekends")
  
  raw_pre_weekdays = lapply(raw_data, function(x) x[dates_pre & dates_weekdays])
  raw_pre_weekends = lapply(raw_data, function(x) x[dates_pre & dates_weekends])
  raw_post_weekdays = lapply(raw_data, function(x) x[dates_post & dates_weekdays])
  raw_post_weekends = lapply(raw_data, function(x) x[dates_post & dates_weekends])
  names(raw_pre_weekdays) = paste0(names, "_raw_pre_weekdays")
  names(raw_pre_weekends) = paste0(names, "_raw_pre_weekends")
  names(raw_post_weekends) = paste0(names, "_raw_post_weekends")
  names(raw_post_weekdays) = paste0(names, "_raw_post_weekdays")
  
  #ecdf_all = lapply(raw_pre, calc_ecdf)
  ecdf_weekdays = lapply(raw_pre_weekdays, calc_ecdf)
  ecdf_weekends = lapply(raw_pre_weekends, calc_ecdf)
  
  rates_weekdays = lapply(1:n, function(x) calc_rates(raw_weekdays[[x]], ecdf_weekdays[[x]], datesSeq[dates_weekdays]))
  rates_weekends = lapply(1:n, function(x) calc_rates(raw_weekends[[x]], ecdf_weekends[[x]], datesSeq[dates_weekends]))
  names(rates_weekdays) = paste0(names, "_rates_weekdays")
  names(rates_weekends) = paste0(names, "_rates_weekends")
  
  rates_pre_weekdays = lapply(rates_weekdays, function(x) x[datesSeq[dates_weekdays] < app.date])
  rates_pre_weekends = lapply(rates_weekends, function(x) x[datesSeq[dates_weekends] < app.date])
  rates_post_weekdays = lapply(rates_weekdays, function(x) x[!datesSeq[dates_weekdays] < app.date])
  rates_post_weekends = lapply(rates_weekends, function(x) x[!datesSeq[dates_weekends] < app.date])
  names(rates_pre_weekdays) = paste0(names, "_rates_pre_weekdays")
  names(rates_pre_weekends) = paste0(names, "_rates_pre_weekends")
  names(rates_post_weekdays) = paste0(names, "_rates_post_weekdays")
  names(rates_post_weekends) = paste0(names, "_rates_post_weekends")
  
  vecrates_weekdays = lapply(rates_weekdays, vect_data)
  vecrates_weekends = lapply(rates_weekends, vect_data)
  names(vecrates_weekdays) = paste0(names, "_vec_rates_weekdays")
  names(vecrates_weekends) = paste0(names, "_vec_rates_weekends")
  
  vecrates_pre_weekdays = lapply(rates_pre_weekdays, vect_data)
  vecrates_pre_weekends = lapply(rates_pre_weekends, vect_data)
  vecrates_post_weekdays = lapply(rates_post_weekdays, vect_data)
  vecrates_post_weekends = lapply(rates_post_weekends, vect_data)
  names(vecrates_pre_weekdays) = paste0(names, "_vec_rates_pre_weekdays")
  names(vecrates_pre_weekends) = paste0(names, "_vec_rates_pre_weekends")
  names(vecrates_post_weekdays) = paste0(names, "_vec_rates_post_weekdays")
  names(vecrates_post_weekends) = paste0(names, "_vec_rates_post_weekends")
  
  results = c(raw_data, raw_pre, raw_post, raw_pre_weekdays, raw_pre_weekends, raw_post_weekdays, raw_post_weekends,
              rates_weekdays, rates_weekends, rates_pre_weekdays, rates_pre_weekends, rates_post_weekdays, 
              rates_post_weekends, vecrates_weekdays, vecrates_weekends, vecrates_pre_weekdays, vecrates_pre_weekends, 
              vecrates_post_weekdays, vecrates_post_weekends)
  
}

processData <- function(data){
  
  mats = create_time_mats(data)
  rates = create_rates(mats)
  rates$matrices = mats
  
  return(rates)
  
}

#####################################################################################

create_rates2 <- function(mats, data = c("trips_len", "total_inc"), 
                         bases = c("shifts_len", "shifts_len"),
                         names = c("occup", "inc"), app.date = as.Date("2015-11-23")){
  
  div_mats <- function(data, base){
    ind = index(data)
    m = data/base
    m = ts(m)
    m[is.nan(m)] = NA
    m = zoo(m, ind)
    return(m)
  }
  
  calc_ecdf <- function(mat, sd = 0.01){
    n_mat = matrix(rnorm(nrow(mat) * ncol(mat), sd = sd), nrow = nrow(mat))
    apply(mat + n_mat, 2, ecdf)
  }
  
  
  calc_rates <- function(mat, ecdf_fun, dates){
    n.times = length(USED.TIMES$time)
    zoo(sapply(1:n.times, function(x) ecdf_fun[[x]](mat[, x])), dates)
  }
  
  vect_data <- function(mat){
    v = as.vector(t(mat))
    names(v) = as.vector(t(outer(index(mat), USED.TIMES$time, paste, sep = ".")))
    return(v)
  }
  
  split_dates <- function(data, name){
    
    pre = data[dates_pre]
    post = data[dates_post]
    names(pre) = paste0(name, "_pre")
    names(post) = paste0(name, "_post")
    
    weekdays = data[dates_weekdays]
    weekends = data[dates_weekends]
    names(weekdays) = paste0(name, "_w-days")
    names(weekends) = paste0(name, "_w-ends")
    
    pre_weekdays = data[dates_pre & dates_weekdays]
    pre_weekends = data[dates_pre & dates_weekends]
    post_weekdays = data[dates_post & dates_weekdays]
    post_weekends = data[dates_post & dates_weekends]
    names(pre_weekdays) = paste0(name, "_pre_w-days")
    names(pre_weekends) = paste0(name, "_pre_w-ends")
    names(post_weekends) = paste0(name, "_post_w-ends")
    names(post_weekdays) = paste0(name, "_post_w-days")
    
    return(c(pre, post, weekdays, weekends, pre_weekdays, pre_weekends, post_weekdays, post_weekends))
  }
  
  n = length(data)
  datesSeq = index(mats[[1]])
  dates_weekdays = !wday(datesSeq) %in% c(1,7)
  dates_weekends = wday(datesSeq) %in% c(1,7)
  dates_pre = datesSeq < app.date
  dates_post = datesSeq >= app.date
  dates_weekdays_pre = datesSeq[dates_weekdays] < app.date
  dates_weekdays_post = datesSeq[dates_weekdays] >= app.date
  dates_weekends_pre = datesSeq[dates_weekends] < app.date
  dates_weekends_post = datesSeq[dates_weekends] >= app.date
  
  res = list()
  
  for (i in 1:n){
    
    raw_data = div_mats(mats[[data[i]]], mats[[bases[i]]])
    names(raw_data) = "raw"
    
    c_res = list(raw_data, split(raw_data))
    
    #ecdf_all = lapply(raw_pre, calc_ecdf)
    ecdf_weekdays = lapply(raw_pre_weekdays, calc_ecdf)
  ecdf_weekends = lapply(raw_pre_weekends, calc_ecdf)
  
  rates_weekdays = lapply(1:n, function(x) calc_rates(raw_weekdays[[x]], ecdf_weekdays[[x]], datesSeq[dates_weekdays]))
  rates_weekends = lapply(1:n, function(x) calc_rates(raw_weekends[[x]], ecdf_weekends[[x]], datesSeq[dates_weekends]))
  names(rates_weekdays) = paste0(names, "_rates_weekdays")
  names(rates_weekends) = paste0(names, "_rates_weekends")
  
  rates_pre_weekdays = lapply(rates_weekdays, function(x) x[datesSeq[dates_weekdays] < app.date])
  rates_pre_weekends = lapply(rates_weekends, function(x) x[datesSeq[dates_weekends] < app.date])
  rates_post_weekdays = lapply(rates_weekdays, function(x) x[!datesSeq[dates_weekdays] < app.date])
  rates_post_weekends = lapply(rates_weekends, function(x) x[!datesSeq[dates_weekends] < app.date])
  names(rates_pre_weekdays) = paste0(names, "_rates_pre_weekdays")
  names(rates_pre_weekends) = paste0(names, "_rates_pre_weekends")
  names(rates_post_weekdays) = paste0(names, "_rates_post_weekdays")
  names(rates_post_weekends) = paste0(names, "_rates_post_weekends")
  
  vecrates_weekdays = lapply(rates_weekdays, vect_data)
  vecrates_weekends = lapply(rates_weekends, vect_data)
  names(vecrates_weekdays) = paste0(names, "_vec_rates_weekdays")
  names(vecrates_weekends) = paste0(names, "_vec_rates_weekends")
  
  vecrates_pre_weekdays = lapply(rates_pre_weekdays, vect_data)
  vecrates_pre_weekends = lapply(rates_pre_weekends, vect_data)
  vecrates_post_weekdays = lapply(rates_post_weekdays, vect_data)
  vecrates_post_weekends = lapply(rates_post_weekends, vect_data)
  names(vecrates_pre_weekdays) = paste0(names, "_vec_rates_pre_weekdays")
  names(vecrates_pre_weekends) = paste0(names, "_vec_rates_pre_weekends")
  names(vecrates_post_weekdays) = paste0(names, "_vec_rates_post_weekdays")
  names(vecrates_post_weekends) = paste0(names, "_vec_rates_post_weekends")
  
  results = c(raw_data, raw_pre, raw_post, raw_pre_weekdays, raw_pre_weekends, raw_post_weekdays, raw_post_weekends,
              rates_weekdays, rates_weekends, rates_pre_weekdays, rates_pre_weekends, rates_post_weekdays, 
              rates_post_weekends, vecrates_weekdays, vecrates_weekends, vecrates_pre_weekdays, vecrates_pre_weekends, 
              vecrates_post_weekdays, vecrates_post_weekends)
  
}
setwd("~/streetsmart/performanceAnalysis")
library(forecast)
library(dplyr)
library(lubridate)
library(zoo)
library(psych)
library(trend)
library(ggplot2)

#base.date = mdy_hms("1-1-2014 0:0:0")

####################################################
# Constants:
#----------
# Outlier in the time between the last trip and the shift's end. 
# Using IQR (Interquartile range) difference from upper quartile as a threshold for an outlier.

ICQ.DIFF = 3

# Times and windows' sizes

WINDOW.SIZE = 240

ALL.TIMES = data.frame(time = 0:23, window = rep(WINDOW.SIZE, 24))
PART.TIMES = data.frame(time = seq(0, 23, 4), window = rep(WINDOW.SIZE, 6))
SPECIFIC.TIMES = data.frame(time = c(0, 4, 8, 10, seq(11, 21, 1), 23), window = c(rep(240, 3), 120, rep(60, 11), 120))

USED.TIMES = ALL.TIMES

# app start date

app.date = as.Date("2015-11-23")

####################################################


source("utils.R")
source("timeUtils.R")
source("dataLoading.R")
source("dataProcessing.R")

shifts_files = list("data/Shifts_Mario_01-14_12-14.csv", "data/Shifts_Mario_01-15_31-01-16.csv")
trips_months = data.frame(m = c(rep(1:12, 2), 1), y = c(rep(14, 12), rep(15, 12), 16))

shifts_files_12.14 = list("data/Shifts_Mario_12-14.csv", "data/Shifts_Mario_01-15_21-01-16.csv")
trips_months_12.14 = data.frame(m = c(12, 1:12, 1), y = c(14, rep(15, 12), 16))


mat_names = c("shifts_len", "trips_len", "total_inc")
mat_source = c("shifts", "trips", "trips")
mat_func = c(windowed_length, windowed_length, windowed_inc)
ratios_data = c("trips_len", "total_inc")
ratios_bases = c("shifts_len", "shifts_len")
ratios_names = c("occup", "inc")


data = readData(shifts_files, trips_months)
rates = processData(data)



#time_mats = create_time_mats(data, mat_names, mat_source, mat_func)
#rates = create_rates(time_mats, ratios_data, ratios_bases, ratios_names, app.date)

#rates = create_rates(time_mats)
#rates3 = create_rates2(time_mats)


data_12.14 = readData(shifts_files_12.14, trips_months_12.14)
time_mats_12.14 = create_time_mats(data_12.14, mat_names, mat_source, mat_func)
rates_12.14 = create_rates(time_mats_12.14, ratios_data, ratios_bases, ratios_names, app.date)

#############################

mean_hours_inc_rates_pre = apply(rates$inc_rates_pre_weekdays, 2, mean, na.rm = T)
mean_hours_inc_rates_post = apply(rates$inc_rates_post_weekdays, 2, mean, na.rm = T)
write.csv(mean_hours_inc_rates_pre, "res files/mean_hours_inc_rates_pre.csv")
write.csv(mean_hours_inc_rates_post, "res files/mean_hours_inc_rates_post.csv")
mean(mean_hours_inc_rates_pre)
mean(rates$inc_vec_rates_pre_weekdays, na.rm = T)

mean(mean_hours_inc_rates_post)
mean(rates$inc_vec_rates_post_weekdays, na.rm = T)


vec_pre = tail(rates$inc_vec_rates_pre_weekdays, 6200)
vec_post = rates$inc_vec_rates_post_weekdays

set.seed(1)
pre_samp = sample(length(vec_pre), 200)
pre_samp = pre_samp[order(pre_samp)]
post_samp = sample(length(vec_post), 50)
post_samp = post_samp[order(post_samp)]

vec = c(vec_pre[pre_samp], vec_post[post_samp])
plot(vec, type ="l")
abline(v = 200, col = "red")
abline(h = mean(vec_pre, na.rm = T), col = "green")
abline(h = mean(vec_post, na.rm = T), col = "blue")
mean(vec_pre, na.rm = T)
mean(vec_post, na.rm = T)

##############

mean_hours_inc_rates_pre_12.14 = apply(rates_12.14$inc_rates_pre_weekdays, 2, mean, na.rm = T)
mean_hours_inc_rates_post_12.14 = apply(rates_12.14$inc_rates_post_weekdays, 2, mean, na.rm = T)
#write.csv(mean_hours_inc_rates_pre, "res files/mean_hours_inc_rates_pre.csv")
#write.csv(mean_hours_inc_rates_post, "res files/mean_hours_inc_rates_post.csv")
mean(mean_hours_inc_rates_pre_12.14)
mean(rates_12.14$inc_vec_rates_pre_weekdays, na.rm = T)

mean(mean_hours_inc_rates_post_12.14)
mean(rates_12.14$inc_vec_rates_post_weekdays, na.rm = T)


vec_pre = tail(rates_12.14$inc_vec_rates_pre_weekdays, 6200)
vec_post = rates_12.14$inc_vec_rates_post_weekdays

set.seed(1)
pre_samp = sample(length(vec_pre), 200)
pre_samp = pre_samp[order(pre_samp)]
post_samp = sample(length(vec_post), 50)
post_samp = post_samp[order(post_samp)]

vec = c(vec_pre[pre_samp], vec_post[post_samp])
plot(vec, type ="l")
abline(v = 200, col = "red")
abline(h = mean(vec_pre, na.rm = T), col = "green")
abline(h = mean(vec_post, na.rm = T), col = "blue")
mean(vec_pre, na.rm = T)
mean(vec_post, na.rm = T)

set.seed(1)
pre_samp = sample(length(vec_pre) - 5, 40)
pre_samp = pre_samp[order(pre_samp)]
post_samp = sample(length(vec_post) - 5, 10)
post_samp = post_samp[order(post_samp)]

sampled_pre = 


#####

vec_pre = rates$inc_vec_rates_pre_weekdays
prev_year = (substr(names(vec_pre), 1, 4) == "2014" & substr(names(vec_pre), 6, 7) == "12") | 
  (substr(names(vec_pre), 1, 4) == "2015" & substr(names(vec_pre), 6, 7) == "1")
vec_pre = vec_pre[prev_year]
vec_post = rates$inc_vec_rates_post_weekdays

set.seed(1)
pre_samp = sample(length(vec_pre), 150)
pre_samp = pre_samp[order(pre_samp)]
post_samp = sample(length(vec_post), 150)
post_samp = post_samp[order(post_samp)]

vec = c(vec_pre[pre_samp], vec_post[post_samp])
plot(vec, type ="l")
abline(v = 150, col = "red")
abline(h = mean(vec_pre, na.rm = T), col = "green")
abline(h = mean(vec_post, na.rm = T), col = "blue")
mean(vec_pre, na.rm = T)
mean(vec_post, na.rm = T)


####################################################3


daily_inc_raw = apply(rbind(rates$inc_raw_pre_weekdays, rates$inc_raw_post_weekdays), 1, mean, na.rm = T)
daily_inc_rates = apply(rbind(rates$inc_rates_pre_weekdays, rates$inc_rates_post_weekdays), 1, mean, na.rm = T)
daily_inc_rates[!is.finite(daily_inc_rates)] = NA
daily_inc_rates_pre = daily_inc_rates[names(daily_inc_rates) < app.date]
daily_inc_rates_post = daily_inc_rates[names(daily_inc_rates) >= app.date]
mean(daily_inc_rates_pre, na.rm = T)
mean(daily_inc_rates_post, na.rm = T)

daily_occ_rates = apply(rbind(rates$occup_rates_pre_weekdays, rates$occup_rates_post_weekdays), 1, mean, na.rm = T)
daily_occ_rates[!is.finite(daily_occ_rates)] = NA
daily_occ_rates_pre = daily_occ_rates[names(daily_occ_rates) < app.date]
daily_occ_rates_post = daily_occ_rates[names(daily_occ_rates) >= app.date]
mean(daily_occ_rates_pre, na.rm = T)
mean(daily_occ_rates_post, na.rm = T)
mean(daily_occ_rates_pre[names(daily_occ_rates_pre) >= "2014-12-01"], na.rm = T)
mean(daily_occ_rates_pre[names(daily_occ_rates_pre) < "2014-12-01"], na.rm = T)
mean(daily_occ_rates_pre[names(daily_occ_rates_pre) >= "2015-01-01"], na.rm = T)
mean(daily_occ_rates_pre[names(daily_occ_rates_pre) < "2015-01-01"], na.rm = T)
mean(daily_occ_rates_post[names(daily_occ_rates_post) < "2015-12-25"], na.rm = T)
mean(daily_occ_rates_post[names(daily_occ_rates_post) >= "2015-12-25"], na.rm = T)

mean(rbind(rates$occup_rates_pre_weekdays, rates$occup_rates_post_weekdays), na.rm = T)
mean(rates$occup_rates_pre_weekdays, na.rm = T)
mean(rates$occup_rates_post_weekdays, na.rm = T)
mean(rates$occup_rates_pre_weekdays[index(rates$occup_rates_pre_weekdays) >= "2014-12-01"], na.rm = T)
mean(rates$occup_rates_pre_weekdays[index(rates$occup_rates_pre_weekdays) < "2014-12-01"], na.rm = T)


mean(rates$occup_rates_post_weekdays[index(rates$occup_rates_post_weekdays) < "2015-12-25"], na.rm = T)
mean(rates$occup_rates_post_weekdays[index(rates$occup_rates_post_weekdays) >= "2015-12-25"], na.rm = T)
mean(rates$occup_rates_pre_weekdays[index(rates$occup_rates_pre_weekdays) <= "2015-01-20" &
                                      index(rates$occup_rates_pre_weekdays) >= "2014-12-25"], na.rm = T)
mean(rates$occup_rates_pre_weekdays[index(rates$occup_rates_pre_weekdays) < "2014-12-25" &
                                      index(rates$occup_rates_pre_weekdays) >= "2014-11-23"], na.rm = T)

mean(rates$inc_rates_post_weekdays[index(rates$inc_rates_post_weekdays) < "2015-12-25"], na.rm = T)
mean(rates$inc_rates_post_weekdays[index(rates$inc_rates_post_weekdays) >= "2015-12-25"], na.rm = T)
mean(rates$inc_rates_pre_weekdays[index(rates$inc_rates_pre_weekdays) <= "2015-01-20" &
                                      index(rates$inc_rates_pre_weekdays) >= "2014-12-25"], na.rm = T)
mean(rates$inc_rates_pre_weekdays[index(rates$inc_rates_pre_weekdays) < "2014-12-25" &
                                      index(rates$inc_rates_pre_weekdays) >= "2014-11-23"], na.rm = T)


inc_rates_23 = rbind(rates$inc_raws_pre_weekdays, rates$inc_rates_post_weekdays)[, 24]

idx = c(index(rates$inc_raw_pre_weekdays), index(rates$inc_raw_post_weekdays))
names(daily_inc_raw) = idx
names(daily_inc_rates) = idx

par(mfrow=c(1,1))
tail_data = tail(daily_inc_raw[!is.na(daily_inc_raw)], 80)
dec_starts = sum(month(names(tail_data)) >= 2 & month(names(tail_data)) < 12) + 1
cris_day = 66
plot(tail_data[dec_starts:80], type = "l")
abline(v = cris_day - dec_starts + 1, col = "red")
#abline(v = dec_starts, col = "red")
abline(b = 0, a =mean(tail_data[dec_starts:cris_day - 1]), col = "green")
abline(b = 0, a =mean(tail_data[cris_day:80]), col = "blue")

mean(tail_data[dec_starts:cris_day - 1])
mean(tail_data[cris_day:80])

tail_data = tail(daily_inc_rates[!is.na(daily_inc_rates)], 80)
dec_starts = sum(month(names(tail_data)) >= 2 & month(names(tail_data)) < 12) + 1
cris_day = 66
plot(tail_data[dec_starts:80], type = "l")
abline(v = cris_day - dec_starts + 1, col = "red")
#abline(v = dec_starts, col = "red")
abline(b = 0, a =mean(tail_data[dec_starts:cris_day - 1]), col = "green")
abline(b = 0, a =mean(tail_data[cris_day:80]), col = "blue")

mean(tail_data[dec_starts:cris_day - 1])
mean(tail_data[cris_day:80])

tail_data = tail(inc_rates_23, 80)
dec_starts = sum(month(index(tail_data)) >= 2 & month(index(tail_data)) < 12) + 1
cris_day = which(index(tail_data) == "2015-12-28")
plot(as.vector(tail_data)[dec_starts:length(tail_data)], type = "l")
abline(v = cris_day - dec_starts + 1, col = "red")
#abline(v = dec_starts, col = "red")
abline(b = 0, a =mean(tail_data[dec_starts:cris_day - 1], na.rm = T), col = "green")
abline(b = 0, a =mean(tail_data[cris_day:length(tail_data)], na.rm = T), col = "blue")

mean(tail_data[dec_starts:cris_day - 1])
mean(tail_data[cris_day:80])



prev_data = daily_inc_raw[names(daily_inc_raw) >= "2014-12-01" & names(daily_inc_raw) <= "2015-01-20"]
plot(prev_data, type ="b")
mean(prev_data[1:19], na.rm =T)
mean(prev_data[20:37], na.rm =T)

plot_chg <- function(data, h, date){
  pre_data = data[index(data) < date, h + 1]
  post_data = data[index(data) >= date, h + 1]
  y_min = min(data[, h + 1], na.rm = T) - 0.05; y_max = max(data[, h + 1], na.rm = T) + 0.05
  plot(pre_data, type ="b", ylim = c(y_min, y_max), xlim = c(min(index(data)), max(index(data))), main = paste0("hour = ", h))
  lines(post_data, col = "red")
  abline(a = mean(pre_data, na.rm = T), b = 0, col = "blue")
  abline(a = mean(post_data, na.rm = T), b = 0, col = "green")
}




####################################################3
####################################################3
####################################################3
####################################################3

base.date = mdy_hms("1-1-2014 0:0:0")

timesSeq = 0:23
n.times = length(timesSeq)
datesSeq = seq.Date(shifts_data$s_date[1], tail(shifts_data$s_date,1), by = "day")
n.dates = length(datesSeq)



#count_hours1 = sapply(0:23, function(x) sum(hour(shifts_data$start_date) <= x & hour(shifts_data$end_date) >= x))
#count_hours2 = sapply(0:23, function(x) sum(hour(trips_data$start_date) <= x & hour(trips_data$end_date) >= x))
#plot(count_hours2, type = "l")
#lines(count_hours1, col = "red")

shifts_data = data$shifts

trips_data = data$trips

shiftsMat = create_mat_old(shifts_data)
tripsMat = create_mat_old(trips_data)
occMat = tripsMat / shiftsMat

occTS = apply(occMat, 2, ts)
occTS[is.nan(occTS)] = NA

trips_data$Trips = trips_data$inc  
earningsMat = create_trips_mat(trips_data, windowTrips())
earnMat = earningsMat / shiftsMat
earnTS = apply(earnMat, 2, ts)
earnTS[is.nan(earnTS)] = NA

datesStats = data.frame(date = datesSeq)
datesStats$app = datesStats$date >= app.date
datesStats$weekday = factor(weekdays(datesStats$date), 
                            levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
datesStats$weekend = datesStats$weekday == "Saturday" | datesStats$weekday == "Sunday"
datesStats$month = factor(months(datesStats$date))

########################
occTS = earnTS
########################

preOcc = occTS[!datesStats$app, ]
postOcc = occTS[datesStats$app, ]
preDates = datesStats[!datesStats$app, ]
postDates = datesStats[datesStats$app, ]

# multi.hist(preOcc[, 1:9])
# multi.hist(preOcc[, 8:16])
# multi.hist(preOcc[, c(17:24, 1)])
# multi.hist(preOcc[!preDates$weekend, 1:9], freq = T)
# multi.hist(preOcc[!preDates$weekend, 8:16], freq = T)
# multi.hist(preOcc[!preDates$weekend, c(17:24, 1)], freq = T)
# multi.hist(preOcc[preDates$weekend, 1:9], freq = T)
# multi.hist(preOcc[preDates$weekend, 8:16], freq = T)
# multi.hist(preOcc[preDates$weekend, c(17:24, 1)], freq = T)
# 
# multi.hist(postOcc[, 1:9], freq = T, breaks = 5)
# multi.hist(postOcc[, 8:16], freq = T, breaks = 5)
# multi.hist(postOcc[, c(17:24, 1)], freq = T, breaks = 5)
# 
# apply(postOcc, 2, function(x) sum(!is.na(x)))
# apply(postOcc[!postDates$weekend,], 2, function(x) sum(!is.na(x)))
# apply(postOcc[postDates$weekend,], 2, function(x) sum(!is.na(x)))

source("calsRates.R")

weekday_pre_rates = lapply(timesSeq, weekday_hour_rates, pre = TRUE)
weekday_post_rates = lapply(timesSeq, weekday_hour_rates, pre = FALSE)

weekday_pre_rates2 = lapply(timesSeq, weekday_hour_rates2, pre = TRUE)
weekday_post_rates2 = lapply(timesSeq, weekday_hour_rates2, pre = FALSE)
weekend_pre_rates = lapply(timesSeq, weekend_hour_rates, pre = TRUE)
weekend_post_rates = lapply(timesSeq, weekend_hour_rates, pre = FALSE)
raw_weekday_pre_rates = lapply(timesSeq, weekday_raw_rates, pre = TRUE)
raw_weekday_post_rates = lapply(timesSeq, weekday_raw_rates, pre = FALSE)
raw_weekend_pre_rates = lapply(timesSeq, weekend_raw_rates, pre = TRUE)
raw_weekend_post_rates = lapply(timesSeq, weekend_raw_rates, pre = FALSE)


analyze_raw <- function(h, weekday = TRUE){
  if(weekday){
    raw_pre = raw_weekday_pre_rates[[h + 1]]
    raw_post = raw_weekday_post_rates[[h + 1]]
    title = paste0("Weekdays' raw occupancy rates; time = ", h)
  } else {
    raw_pre = raw_weekend_pre_rates[[h + 1]]
    raw_post = raw_weekend_post_rates[[h + 1]]
    title = paste0("Weekends' raw occupancy rates; time = ", h)
  }
  avg_pre = mean(raw_pre)
  avg_post = mean(raw_post)
  plot_pre_post(raw_pre, raw_post, avg_pre, avg_post, title)
  print(c("post data size: ", length(raw_post)))
  print (c("average pre: ", avg_pre))
  print (c("average post: ", avg_post))
  wl = wilcox.test(raw_pre, raw_post)
  print(wl)
  t = t.test(raw_pre, raw_post)
  print(t)
  return(list(avg_pre = avg_pre, sd_pre = sd(raw_pre), avg_post = avg_post, avg_chg = avg_post - avg_pre, p.value = wl$p.value,
              t = t$p.value))
}

analyze_raw_tail <- function(h, weekday = TRUE, n = 30){
  if(weekday){
    raw_pre = tail(raw_weekday_pre_rates[[h + 1]], n)
    raw_post = raw_weekday_post_rates[[h + 1]]
    title = paste0("Weekdays' raw occupancy rates; time = ", h)
  } else {
    raw_pre = tail(raw_weekend_pre_rates[[h + 1]], n)
    raw_post = raw_weekend_post_rates[[h + 1]]
    title = paste0("Weekends' raw occupancy rates; time = ", h)
  }
  avg_pre = mean(raw_pre)
  avg_post = mean(raw_post)
  plot_pre_post(raw_pre, raw_post, avg_pre, avg_post, title)
  print(c("post data size: ", length(raw_post)))
  print (c("average pre: ", avg_pre))
  print (c("average post: ", avg_post))
  wl = wilcox.test(raw_pre, raw_post)
  print(wl)
  return(list(avg_chg = avg_post - avg_pre, p.value = wl$p.value))
}


analyze_rates <- function(h, weekday = TRUE){
  if(weekday){
    rates_pre = weekday_pre_rates[[h + 1]]
    rates_post = weekday_post_rates[[h + 1]]
    title = paste0("Weekdays' relative occupancy rates; time = ", h)
  } else {
    rates_pre = weekend_pre_rates[[h + 1]]
    rates_post = weekend_post_rates[[h + 1]]
    title = paste0("Weekends' relative occupancy rates; time = ", h)
  }
  avg_pre = mean(rates_pre)
  avg_post = mean(rates_post)
  plot_pre_post(rates_pre, rates_post, avg_pre, avg_post, title)
  print(c("post data size: ", length(rates_post)))
  print (c("average pre: ", avg_pre))
  print (c("average post: ", avg_post))
  wl = wilcox.test(rates_pre, rates_post)
  print(wl)
  return(list(avg_pre = avg_pre, sd_pre = sd(rates_pre), avg_post = avg_post, avg_chg = avg_post - avg_pre, p.value = wl$p.value))
}


analyze_rates2 <- function(h, weekday = TRUE){
  if(weekday){
    rates_pre = weekday_pre_rates2[[h + 1]]
    rates_post = weekday_post_rates2[[h + 1]]
    title = paste0("Weekdays' relative occupancy rates; time = ", h)
  } else {
    rates_pre = weekend_pre_rates[[h + 1]]
    rates_post = weekend_post_rates[[h + 1]]
    title = paste0("Weekends' relative occupancy rates; time = ", h)
  }
  avg_pre = mean(rates_pre)
  avg_post = mean(rates_post)
  plot_pre_post(rates_pre, rates_post, avg_pre, avg_post, title)
  print(c("post data size: ", length(rates_post)))
  print (c("average pre: ", avg_pre))
  print (c("average post: ", avg_post))
  wl = wilcox.test(rates_pre, rates_post)
  print(wl)
  return(list(avg_pre = avg_pre, sd_pre = sd(rates_pre), avg_post = avg_post, avg_chg = avg_post - avg_pre, p.value = wl$p.value))
}

forecast_analysis <- function(h, weekday = TRUE){
  if(weekday){
    rates_pre = raw_weekday_pre_rates[[h + 1]]
    rates_post = raw_weekday_post_rates[[h + 1]]
    title = paste0("Weekdays' forecast values; time = ", h)
  } else {
    rates_pre = raw_weekend_pre_rates[[h + 1]]
    rates_post = raw_weekend_post_rates[[h + 1]]
    title = paste0("Weekends' forecast values; time = ", h)
  }
  
  n.post = length(rates_post)
  fit = auto.arima(rates_pre)
  forec = forecast(fit, h = n.post)
  par(mfrow = c(1, 1))
  ts.plot(rates_post, forec$mean, col = 1:2, ylim = c(min(c(rates_post, forec$lower[,2])), max(c(rates_post, forec$upper[,2]))), 
       type = "l", main = title)
  lines(forec$upper[, 2], col = "blue", lty= "dashed")
  lines(forec$lower[, 2], col = "blue", lty= "dashed")
  abline(a = mean(rates_post), b = 0, col = "green")
  print(c("post data size: ", n.post))
  print(c("Arima model: ", summary(fit)))
  cat("\n")
  print (c("average forecast: ", mean(forec$mean)))
  print (c("average post: ", mean(rates_post)))
  wl = wilcox.test(forec$mean, rates_post)
  print(wl)
  return(list(avg_pre = mean(forec$mean), avg_post = mean(rates_post), 
              avg_chg = mean(rates_post) - mean(forec$mean), p.value = wl$p.value))
  #return(list(avg_chg = mean(rates_post) - mean(forec$mean), p.value = wl$p.value))
}

sss1 = sapply(timesSeq, forecast_analysis)
sss2 = sapply(timesSeq, analyze_rates)
sss2.2 = sapply(timesSeq, analyze_rates2)
sss3 = sapply(timesSeq, analyze_raw)

pre_rate_0 = weekday_pre_rates[[1]]
post_rate_0 = weekday_post_rates[[1]]
############################################


day_rates = apply(occTS, 1, mean, na.rm = T)
plot(day_rates[is.finite(day_rates)], type = "l")
find.freq(day_rates[is.finite(day_rates)])
auto.arima(day_rates[is.finite(day_rates)])

ecdf_vals = ecdf(day_rates[!datesStats$app])
ecdf_rates_pre = ecdf_vals(day_rates[!datesStats$app])
ecdf_rates_pre = ecdf_rates_pre[!is.na(ecdf_rates_pre)]
ecdf_rates_post = ecdf_vals(day_rates[datesStats$app])
ecdf_rates_post = ecdf_rates_post[!is.na(ecdf_rates_post)]
mean(ecdf_rates_pre)
mean(ecdf_rates_post)
wilcox.test(ecdf_rates_pre, ecdf_rates_post)

hours_usage_pre = apply(preOcc, 2, function(x) sum(!is.na(x)))
hours_usage_post = apply(postOcc, 2, function(x) sum(!is.na(x)))
hours_usage_pre = hours_usage_pre / sum(hours_usage_pre)
hours_usage_post = hours_usage_post / sum(hours_usage_post)

plot(hours_usage_post, type = "b")
lines(hours_usage_pre, col = "red")

pre_hour_rate = apply(preOcc, 2, mean, na.rm = T)
post_hour_rate = apply(postOcc, 2, mean, na.rm = T)
hour_rate = apply(occTS, 2, mean, na.rm = T)

plot(pre_hour_rate, type = "b", ylim = c(0, 0.8))
lines(post_hour_rate, col = "yellow")
lines(hour_rate, col = "green")
lines(hours_usage_pre * 10, col = "red")
lines(hours_usage_post * 10, col = "blue")

pre_dur = shifts_data %>% filter(s_date < app.date) %>% group_by(weekday) %>% 
  summarise(dur = sum(duration)) %>% mutate (rate = as.numeric(dur / sum(as.numeric(dur))))
post_dur = shifts_data %>% filter(s_date >= app.date) %>% group_by(weekday) %>% 
  summarise(dur = sum(duration)) %>% mutate (rate = as.numeric(dur / sum(as.numeric(dur))))
                                             

trips_data$Trips = trips_data$inc  
earningsMat = create_trips_mat(trips_data, windowTrips())
earnMat = earningsMat / shiftsMat
earnTS = apply(earnMat, 2, ts)
earnTS[is.nan(earnTS)] = NA

summary(occTS)
apply(occTS, 2, function(x) length(which(!is.na(x))))
ar = apply(occTS, 2, auto.arima)
fr = apply(occTS, 2, find.freq)



apply(occTS, 2, function(x) length(which(!is.na(x))))
apply(occTS[datesStats$weekend, ], 2, function(x) length(which(!is.na(x))))

plot(apply(occTS[datesStats$weekday == "Sunday", ], 2, mean, na.rm = T), type = "l")
lines(apply(occTS[datesStats$weekday == "Monday", ], 2, mean, na.rm = T), col = "red")
lines(apply(occTS[datesStats$weekday == "Tuesday", ], 2, mean, na.rm = T), col = "blue")
lines(apply(occTS[datesStats$weekday == "Wednesday", ], 2, mean, na.rm = T), col = "green")

plot(apply(occTS[datesStats$weekend, ], 2, mean, na.rm = T), type = "l")
lines(apply(occTS[!datesStats$weekend, ], 2, mean, na.rm = T), col = "red")




chk_means <- function(rates){
  weekday_pre_data = rates[is.finite(rates) & !datesStats$weekend & !datesStats$app]
  weekday_post_data = rates[is.finite(rates) & !datesStats$weekend & datesStats$app]
  weekend_pre_data = rates[is.finite(rates) & datesStats$weekend & !datesStats$app]
  weekend_post_data = rates[is.finite(rates) & datesStats$weekend & datesStats$app]
  print(mean(weekday_pre_data))
  print(mean(weekday_post_data))
  print(wilcox.test(weekday_pre_data, weekday_post_data))
  print(mean(weekend_pre_data))
  print(mean(weekend_post_data))
  if(length(weekend_post_data)){
    print(wilcox.test(weekend_pre_data, weekend_post_data))
  }
}


mean(day_rates[is.finite(day_rates) & datesStats$weekend & !datesStats$app])
mean(day_rates[is.finite(day_rates) & datesStats$weekend & datesStats$app])
wilcox.test(day_rates[is.finite(day_rates) & datesStats$weekend & !datesStats$app], 
            day_rates[is.finite(day_rates) & datesStats$weekend & datesStats$app])

mean(day_rates[is.finite(day_rates) & !datesStats$weekend & !datesStats$app])
mean(day_rates[is.finite(day_rates) & !datesStats$weekend & datesStats$app])
wilcox.test(day_rates[is.finite(day_rates) & !datesStats$weekend & !datesStats$app], 
            day_rates[is.finite(day_rates) & !datesStats$weekend & datesStats$app])


weekday_rates = apply(occTS[!datesStats$weekend,], 1, mean, na.rm = T)
plot(weekday_rates[is.finite(weekday_rates)], type = "l")
find.freq(weekday_rates[is.finite(weekday_rates)])
auto.arima(weekday_rates[is.finite(weekday_rates)])




plot_hour <- function(t, weekday = TRUE){
  plot(occTS[!is.na(occTS[, t + 1]) & datesStats$weekend != weekday, t + 1], type = "l", 
       x = datesStats$date[!is.na(occTS[, t + 1]) & datesStats$weekend != weekday])
  abline(a = mean(occTS[datesStats$weekend != weekday, t + 1], na.rm = T), b = 0, col = "red")
}

week_days = list("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

day_mean <- function(day, pre = TRUE){
  
  apply(occTS[datesStats$weekday == day & datesStats$app != pre, ], 2, mean, na.rm = T)
}
pre_means = lapply(week_days, day_mean)
post_means = lapply(week_days, day_mean, pre = FALSE)

chg = sapply(1:7, function(x) post_means[[x]] - pre_means[[x]])
colMeans(chg, na.rm = T)


vect_compare <- function(data, rates = TRUE, weekdays = TRUE, weekends = FALSE, all.pre = TRUE, prev.year = FALSE, 
                         pre_from = NULL, pre_to = NULL, post_from = NULL, post_to = NULL, plot = TRUE,
                         sample.size = 200, na.rm = FALSE){
  
  if (rates){
    d = data$vect_rates
  } else {
    d = data$vect_raw
  }

  if (weekdays){
    pre_d = d$pre_w.days
    post_d = d$post_w.days
  } else if (weekends){
    pre_d = d$pre_w.ends
    post_d = d$post_w.ends
  } else {
    pre_d = d$pre
    post_d = d$post
  }
  
  
  pre_dates = vect_date(pre_d)  
  post_dates = vect_date(post_d)  
  
  if (is.null(post_from)){
    post_from = as.Date(min(post_dates))
  }
  if (is.null(post_to)){
    post_to = as.Date(max(post_dates))
  }
  post_d = post_d[post_dates >= post_from & post_dates <= post_to]
  post_dates = vect_date(post_d)
  post_from = as.Date(min(post_dates))
  post_to = as.Date(max(post_dates))

  if (is.null(pre_from)){
    pre_from = as.Date(min(pre_dates))
  }
  if (is.null(pre_to)){
    pre_to = as.Date(max(pre_dates))
  }
  

  if (!all.pre | prev.year) {
    if (prev.year){
      pre_from = as.POSIXlt(post_from)
      pre_from$year = pre_from$year - 1
      pre_from = as.Date(pre_from)
      pre_to = as.POSIXlt(post_to)
      pre_to$year = pre_to$year - 1
      pre_to = as.Date(pre_to)
    }
    pre_d = pre_d[pre_dates >= pre_from & pre_dates <= pre_to]
  }
  
  pre_mean = mean(pre_d, na.rm = T)
  post_mean = mean(post_d, na.rm = T)
  print(paste0("pre start date : ", as.Date(min(vect_date(pre_d)))))
  print(paste0("pre end date : ", max(vect_date(pre_d))))
  print(paste0("post start date : ", post_from))
  print(paste0("post end date : ", post_to))
  print(c("pre mean :", pre_mean))
  print(c("post mean :", post_mean))
  print("Wicox test :")
  print(wilcox.test(pre_d, post_d))
  
<<<<<<< HEAD
  if (plot){
    
    if (na.rm){
      pre_d = pre_d[!is.na(pre_d)]
      post_d = post_d[!is.na(post_d)]
    }
    n.pre = length(pre_d)
    n.post = length(post_d)
    n = n.pre + n.post
    if (sample.size & sample.size < (n.pre + n.post)){
      pre_smp = sample(n.pre, n.pre / n * sample.size)
      pre_smp = pre_smp[order(pre_smp)]
      post_smp = sample(n.post, n.post / n * sample.size)
      post_smp = post_smp[order(post_smp)]
      pre_plot = pre_d[pre_smp]
      post_plot = post_d[post_smp]
    } else {
      pre_plot = pre_d
      post_plot = post_d
    }
    
    plot_d = c(pre_plot, post_plot)
    n_pre_plot = c(pre_plot, rep(NA, length(post_plot)))
    n_post_plot = c(rep(NA, length(pre_plot)), post_plot)
    col_t = ifelse(1:length(plot_d) > length(pre_plot), "blue", "green")
    x_labels = vect_date(plot_d)
    labels_at = seq(1, length(x_labels), 10)
    plot(n_pre_plot, xaxt = "n",type ="l", col = "orange")
    lines(n_post_plot, col= "blue")
    axis(1, at = labels_at, labels = vect_date(plot_d)[labels_at])
    abline(v = length(pre_plot), col = "green")
    abline(h = pre_mean, col = "orange", lty=2, lwd = 2)
    abline(h = post_mean, col = "blue", lty=2, lwd = 2)
=======
  #print(deparse(substitute(data)))

}
  
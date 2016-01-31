chk_outliers <- function(d, ratio){
  
  n = length(d)
  iqr = IQR(d, na.rm = T)
  up_th = quantile(d, na.rm = T)[4] + iqr * ratio
  lo_th = quantile(d, na.rm = T)[2] - iqr * ratio
  type = rep(0, n)
  type[!is.na(d) & d > up_th] = 1
  type[!is.na(d) & d < lo_th] = -1
  return(list(outliers = !is.na(d) & (d > up_th | d < lo_th), type = type, up_th = up_th, lo_th = lo_th))
}


safe.ifelse <- function(cond, yes, no) structure(ifelse(cond, yes, no), class = class(yes))

find.freq <- function(x)
{
  n <- length(x)
  if(var(c(na.contiguous(x))) == 0){
    period <- 1
  }
  else{
  spec <- spec.ar(c(na.contiguous(x)),plot=FALSE)
  if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
  {
    period <- round(1/spec$freq[which.max(spec$spec)])
    if(period==Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec)>0)
      if(length(j)>0)
      {
        nextmax <- j[1] + which.max(spec$spec[j[1]:500])
        if(nextmax <= length(spec$freq))
          period <- round(1/spec$freq[nextmax])
        else
          period <- 1
      }
      else
        period <- 1
    }
  }
  else
    period <- 1
  }
  return(period)
}

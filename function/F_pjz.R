F_pjz <- function(data,type) {
  n <- length(data)
  result <- switch(type,
                   arithmetic = mean(data),
                   geometric = exp(mean(log(data))),
                   rms = (sum(data*data)/n)^0.5,
                   harmonic = n/sum(1/data)
  )
  return(result)
}
library(boot)

meanfun <- function(data, i){
  d <- data[i, ]
  return(mean(d))   
}
results_interpersonal_fit <- boot(input_data[, "interpersonal_fit", drop = FALSE], statistic=meanfun, R=1000)
boot.ci(results_interpersonal_fit, conf=0.95, type="bca")

plot(results_interpersonal_fit)

library(boot)

meanfun <- function(data, i){
  d <- data[i, ]
  return(mean(d))   
}

t.test(input_data$interpersonal_fit, mu = 3, alternative = "greater")

t.test(input_data$thriving_at_work, mu = 3, alternative = "greater")

t.test(input_data$feeling_competency_at_work, mu = 3, alternative = "greater")

t.test(input_data$perceived_recognition_at_work, mu = 3, alternative = "greater")

t.test(input_data$desire_for_involvement_at_work, mu = 3, alternative = "greater")

results_interpersonal_fit <- boot(input_data[, "interpersonal_fit", drop = FALSE], statistic=meanfun, R=1000)
boot.ci(results_interpersonal_fit, conf=0.95, type="bca")

results_thriving_at_work <- boot(input_data[, "thriving_at_work", drop = FALSE], statistic=meanfun, R=1000)
boot.ci(results_thriving_at_work, conf=0.95, type="bca")

results_feeling_competency_at_work <- boot(input_data[, "feeling_competency_at_work", drop = FALSE], statistic=meanfun, R=1000)
boot.ci(results_feeling_competency_at_work, conf=0.95, type="bca")

results_perceived_recognition_at_work <- boot(input_data[, "perceived_recognition_at_work", drop = FALSE], statistic=meanfun, R=1000)
boot.ci(results_perceived_recognition_at_work, conf=0.95, type="bca")

results_desire_for_involvement_at_work <- boot(input_data[, "desire_for_involvement_at_work", drop = FALSE], statistic=meanfun, R=1000)
boot.ci(results_desire_for_involvement_at_work, conf=0.95, type="bca")
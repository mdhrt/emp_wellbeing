#interpersonal_fit
satisfied_count = input_data$interpersonal_fit>3 
satisfied_count = sum(satisfied_count)

all_count = input_data$interpersonal_fit>0
all_count = sum(all_count)

prop.test(satisfied_count,all_count, p = 0.5, alternative = "two.sided", correct = TRUE)

proportion_fun <- function(data, i){
  prop <- data[i, ] > 3
  all <- data[i,] > 0
  return(sum(prop)/sum(all))   
}
results_interpersonal_fit <- boot(input_data[, "interpersonal_fit", drop = FALSE], statistic=proportion_fun, R=1000)
boot.ci(results_interpersonal_fit, conf=0.95, type="bca")

plot(results_interpersonal_fit)


#thriving_at_work
satisfied_count = input_data$thriving_at_work>3 
satisfied_count = sum(satisfied_count)

all_count = input_data$thriving_at_work>0
all_count = sum(all_count)

prop.test(satisfied_count,all_count, p = 0.5, alternative = "two.sided", correct = TRUE)

proportion_fun <- function(data, i){
  prop <- data[i, ] > 3
  all <- data[i,] > 0
  return(sum(prop)/sum(all))   
}
results_thriving_at_work <- boot(input_data[, "thriving_at_work", drop = FALSE], statistic=proportion_fun, R=1000)
boot.ci(results_thriving_at_work, conf=0.95, type="bca")

plot(results_thriving_at_work)


#feeling_competency_at_work
satisfied_count = input_data$feeling_competency_at_work>3 
satisfied_count = sum(satisfied_count)

all_count = input_data$feeling_competency_at_work>0
all_count = sum(all_count)

prop.test(satisfied_count,all_count, p = 0.5, alternative = "two.sided", correct = TRUE)

proportion_fun <- function(data, i){
  prop <- data[i, ] > 3
  all <- data[i,] > 0
  return(sum(prop)/sum(all))   
}
results_feeling_competency_at_work <- boot(input_data[, "feeling_competency_at_work", drop = FALSE], statistic=proportion_fun, R=1000)
boot.ci(results_feeling_competency_at_work, conf=0.95, type="bca")

plot(results_feeling_competency_at_work)



#perceived_recognition_at_work
satisfied_count = input_data$perceived_recognition_at_work>3 
satisfied_count = sum(satisfied_count)

all_count = input_data$perceived_recognition_at_work>0
all_count = sum(all_count)

prop.test(satisfied_count,all_count, p = 0.5, alternative = "two.sided", correct = TRUE)

proportion_fun <- function(data, i){
  prop <- data[i, ] > 3
  all <- data[i,] > 0
  return(sum(prop)/sum(all))   
}
results_perceived_recognition_at_work <- boot(input_data[, "perceived_recognition_at_work", drop = FALSE], statistic=proportion_fun, R=1000)
boot.ci(results_perceived_recognition_at_work, conf=0.95, type="bca")

plot(results_perceived_recognition_at_work)

#desire_for_involvement_at_work
satisfied_count = input_data$desire_for_involvement_at_work>3 
satisfied_count = sum(satisfied_count)

all_count = input_data$desire_for_involvement_at_work>0
all_count = sum(all_count)

prop.test(satisfied_count,all_count, p = 0.5, alternative = "two.sided", correct = TRUE)

proportion_fun <- function(data, i){
  prop <- data[i, ] > 3
  all <- data[i,] > 0
  return(sum(prop)/sum(all))   
}
results_desire_for_involvement_at_work <- boot(input_data[, "desire_for_involvement_at_work", drop = FALSE], statistic=proportion_fun, R=1000)
boot.ci(results_desire_for_involvement_at_work, conf=0.95, type="bca")

plot(results_desire_for_involvement_at_work)
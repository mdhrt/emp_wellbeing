#difference in proportion

#interpersonal_fit
new_employees <- input_data$Working.experience.in.years<2
experienced_employees <- input_data$Working.experience.in.years>2 

satisfied_new_employees <- input_data$Working.experience.in.years<2 & input_data$interpersonal_fit>3
all_new_employees <- input_data$Working.experience.in.years<2

satisfied_experienced_employees <- input_data$Working.experience.in.years>2 & input_data$interpersonal_fit>3
all_experienced_employees <- input_data$Working.experience.in.years>2 

prop.test(x = c(sum(satisfied_new_employees), sum(satisfied_experienced_employees)),
          n=c(sum(all_new_employees), sum(all_experienced_employees)))

#thriving_at_work
new_employees <- input_data$Working.experience.in.years<2
experienced_employees <- input_data$Working.experience.in.years>2 

satisfied_new_employees <- input_data$Working.experience.in.years<2 & input_data$thriving_at_work>3
all_new_employees <- input_data$Working.experience.in.years<2

satisfied_experienced_employees <- input_data$Working.experience.in.years>2 & input_data$thriving_at_work>3
all_experienced_employees <- input_data$Working.experience.in.years>2 

prop.test(x = c(sum(satisfied_new_employees), sum(satisfied_experienced_employees)),
          n=c(sum(all_new_employees), sum(all_experienced_employees)))

#feeling_competency_at_work
new_employees <- input_data$Working.experience.in.years<2
experienced_employees <- input_data$Working.experience.in.years>2 

satisfied_new_employees <- input_data$Working.experience.in.years<2 & input_data$feeling_competency_at_work>3
all_new_employees <- input_data$Working.experience.in.years<2

satisfied_experienced_employees <- input_data$Working.experience.in.years>2 & input_data$feeling_competency_at_work>3
all_experienced_employees <- input_data$Working.experience.in.years>2 

prop.test(x = c(sum(satisfied_new_employees), sum(satisfied_experienced_employees)),
          n=c(sum(all_new_employees), sum(all_experienced_employees)))

#perceived_recognition_at_work
new_employees <- input_data$Working.experience.in.years<2
experienced_employees <- input_data$Working.experience.in.years>2 

satisfied_new_employees <- input_data$Working.experience.in.years<2 & input_data$perceived_recognition_at_work>3
all_new_employees <- input_data$Working.experience.in.years<2

satisfied_experienced_employees <- input_data$Working.experience.in.years>2 & input_data$perceived_recognition_at_work>3
all_experienced_employees <- input_data$Working.experience.in.years>2 

prop.test(x = c(sum(satisfied_new_employees), sum(satisfied_experienced_employees)),
          n=c(sum(all_new_employees), sum(all_experienced_employees)))

#desire_for_involvement_at_work
new_employees <- input_data$Working.experience.in.years<2
experienced_employees <- input_data$Working.experience.in.years>2 

satisfied_new_employees <- input_data$Working.experience.in.years<2 & input_data$desire_for_involvement_at_work>3
all_new_employees <- input_data$Working.experience.in.years<2

satisfied_experienced_employees <- input_data$Working.experience.in.years>2 & input_data$desire_for_involvement_at_work>3
all_experienced_employees <- input_data$Working.experience.in.years>2 

prop.test(x = c(sum(satisfied_new_employees), sum(satisfied_experienced_employees)),
          n=c(sum(all_new_employees), sum(all_experienced_employees)))

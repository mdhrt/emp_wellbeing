male_sample <- input_data[input_data$Gender=='Male',]
female_sample <- input_data[input_data$Gender=='Female',]
#calculate diff in means
t.test(x = male_sample$interpersonal_fit, y = female_sample$interpersonal_fit)
t.test(x = male_sample$thriving_at_work, y = female_sample$thriving_at_work)
t.test(x = male_sample$feeling_competency_at_work, y = female_sample$feeling_competency_at_work)
t.test(x = male_sample$perceived_recognition_at_work, y = female_sample$perceived_recognition_at_work)
t.test(x = male_sample$desire_for_involvement_at_work, y = female_sample$desire_for_involvement_at_work)

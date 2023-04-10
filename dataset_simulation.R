library(tidyverse)
library(lubridate)

# create vector of participant IDs
number_of_students <- 100
number_of_lectures <- 20
id <- rep(1:number_of_students, each = number_of_lectures)

# create vector of lecture dates
x <- seq(ymd("2022-09-19"),ymd("2022-12-02"),by="1 day")
lecture_date <- x[wday(x,label = TRUE) %in% c("Mon", "Wed", "Fri")][1:20]

# Make a df based on the two vectors
student_data <- tibble(id = id, lecture_date = rep(lecture_date, number_of_students))

# Add a column based on present/absent
set.seed(20)
attendance_options <- c("present", "absent")
student_data <- student_data %>% 
  mutate(attendance = sample(attendance_options, n(), prob = c(0.6, 0.4), 
                             replace = TRUE))

# Add a column with student mark

student_data_wide <- student_data %>% 
  pivot_wider(names_from = lecture_date, values_from = attendance) %>% 
  rowwise() %>% 
  mutate(lectures_attended = sum(c_across(`2022-09-19`:`2022-11-02`) == "present")) %>%
  mutate(error = rnorm(1, 0, 10)) %>% 
  mutate(final_mark = round(10 + 5*lectures_attended + error, 0)) %>% 
  ungroup() %>% 
  mutate(final_mark = ifelse(final_mark > 100, 98, final_mark))

# pivot to longer, so that the data are in a more complex format
student_data_long <- student_data_wide %>% 
  pivot_longer(cols = `2022-09-19`:`2022-11-02`, names_to = "date", values_to = "attendance") %>% 
  select(-lectures_attended, -error)

# sanity checks
# student_data_wide %>% 
#   ggplot(aes(x = final_mark)) +
#   geom_histogram()
# 
# student_data_long %>% 
#   group_by(date) %>% 
#   summarise(students_attended = sum(attendance == "present"))

write_csv(student_data_long, file = "student_dataset.csv")
         
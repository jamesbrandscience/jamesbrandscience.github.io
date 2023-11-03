#-----------------------
#ERCEL R minikurz
#
#Data processing using the Tidyverse
#-----------------------

#this line of code is important and sets the working directory
#(which is the folder that contains all the files required for your code to work)
#please make sure you run this line of code first
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(readxl)
library(skimr)
library(ggtext)

ercel_data_raw <- read_csv("Data/ercel_questionnaire_data.csv")

#data cleaning steps
ercel_data_clean <- ercel_data_raw %>%
  filter(Finished == TRUE) %>% #remove the participant who did not finish
  select(vaše_jméno, starts_with("day")) #only keep these variables

#make the ercel_data_clean data long
ercel_data_long <- ercel_data_clean %>%
  pivot_longer(cols = starts_with("day"),
               names_to = "day",
               values_to = "time")

#pivot from long to wide
ercel_data_long %>%
  pivot_wider(names_from = day,
              values_from = time)

#make the time variable have separate rows for each values
ercel_data_long_rows <- ercel_data_long %>%
  separate_rows(time,
                sep = ",")

#remove the "day_" prefix from the the day column"
ercel_data_long_rows <- ercel_data_long_rows %>%
  mutate(day = str_remove(string = day,
                          pattern = "day_"))

#remove the "day_" prefix from the the day column"
ercel_data_long_rows <- ercel_data_long_rows %>%
  mutate(time = fct_explicit_na(f = time,
                                na_level = "none"))

#relevel the day and time variables so they are in a specificed order (not alphabetical)
ercel_data_long_rows <- ercel_data_long_rows %>%
  mutate(day = fct_relevel(.f = day,
                           "pondělí",
                           "úterý",
                           "středa",
                           "čtvrtek",
                           "pátek"),
         time = fct_relevel(time,
                            "9.00-10.30am",
                            "10.30am-12.00pm",
                            "12.00-1.30pm",
                            "1.30-3.00pm",
                            "3.00-4.30pm",
                            "none"))

#create a new variable called time_category where any values that are "9.00-10.30am", "10.30am-12.00pm" are "morning" and every other value is "afternoon"
ercel_data_long_rows <- ercel_data_long_rows %>%
  mutate(time_category = ifelse(time %in% c("9.00-10.30am", "10.30am-12.00pm", "12.00-1.30pm"),
                                "morning",
                                "afternoon"))





#create the final data using different processing steps
ercel_data <- read_csv(file = "Data/ercel_questionnaire_data.csv") %>%
  filter(Finished == TRUE) %>%
  select(vaše_jméno, starts_with("day")) %>%
  pivot_longer(starts_with("day"), names_to = "day", values_to = "time") %>%
  separate_rows(time, sep = ",") %>%
  mutate(day = str_remove(day, "day_"),
         day = fct_relevel(day, "pondělí",
                           "úterý",
                           "středa",
                           "čtvrtek",
                           "pátek"),
         time = fct_explicit_na(time, na_level = "none"),
         time = fct_relevel(time,
                            "9.00-10.30am",
                            "10.30am-12.00pm",
                            "12.00-1.30pm",
                            "1.30-3.00pm",
                            "3.00-4.30pm",
                            "none"),
         time_category = ifelse(time %in% c("9.00-10.30am", "10.30am-12.00pm"),
                                "morning",
                                "afternoon"))

df <- data.frame(x = c(NA, "x.y", "x.z", "y.z"))

ercel_data_long %>%
  separate(time, c("A", "B", "C", "D", "E"), sep = ",") %>%
  View()


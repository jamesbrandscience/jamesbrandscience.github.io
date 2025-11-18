#-----------------------
#EHU R minikurz
#Data processing using the Tidyverse
#-----------------------

#this line of code is important and sets the working directory
#(which is the folder that contains all the files required for your code to work)
#please make sure you run this line of code first
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

install.packages("tidyverse")
library(tidyverse)

#install the packages
install.packages("skimr")
install.packages("ggtext")

#load the package
library(readxl)
library(skimr)
library(ggtext)

#load in the raw .csv file
EHU_data_raw <- read_csv("Data/EHU_questionnaire_data.csv")



library(tidyverse)

# load both internal/survey data
internal.data <- read_csv("./data_folder/internal_data.csv")
survey.data <- read_csv("./data_folder/survey_data.csv")

head(internal.data)
head(survey.data)

whole_data <- merge(internal.data, survey.data, by = "user_id")
head(whole_data)

# check data type
str(whole_data)
# change to correct data type
whole_data$user_id <- as.character(whole_data $user_id)
whole_data$credit_card_vendor <- as.factor(whole_data$credit_card_vendor)
whole_data$credit_card_bonus <- as.factor(whole_data$credit_card_bonus)
whole_data$register_method <- as.factor(whole_data $register_method) 
whole_data$class <- as.factor(whole_data$class)

# pre-process the data for model building - dummy
library(dummies)
class(whole_data)
dummy_whole_data <- dummy.data.frame(whole_data,
      sep = "_",
      dummy.classes = "factor")

head(dummy_whole_data)





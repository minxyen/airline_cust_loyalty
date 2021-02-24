library(tidyverse)

internal.data <- read_csv("./data_folder/internal_data.csv")
survey.data <- read_csv("./data_folder/survey_data.csv")

head(internal.data)
head(survey.data)

whole_data <- merge(internal.data, survey.data, by = "user_id")
head(whole_data)

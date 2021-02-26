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

# EDA
# find out the correlation between all variables - heatmap
library(reshape2)
head(melt(cor(dummy_whole_data[,2:ncol(dummy_whole_data)])))
ggplot(melt(cor(dummy_whole_data[,2:ncol(dummy_whole_data)])),
       aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = value), color = 'white') +
  scale_fill_gradient2(low = 'firebrick4',
                      high = 'steelblue',
                      mid = 'white', midpoint = 0) +
  guides(fill = guide_legend(title = "Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

# ------- Logistic Regression Model Building ...
# ------- Marketing Factor, Internal Data

mrkt_model <- glm(is_loyal ~ dm_message + dm_post + dm_email +              
      credit_card_vendor + credit_card_bonus + 
      tv_ad + youtube_ad_1 + youtube_ad_2 + youtube_ad_3,
    data = whole_data, family = binomial(link="logit"))

summary(mrkt_model)

# positive influence (α=0.05) variables：
# - dm_post, dm_email, credit_card_bonus3, credit_card_bonus4, tv_ad, youtube_ad_2
# negative influence (α=0.05) variables：
# - dm_message, youtube_ad_1

predict_prob <- predict(mrkt_model, whole_data, type = "response")
library("InformationValue")
# Calculate cut-off probability which minimized mis-classification error
opt_cutoff <- optimalCutoff(whole_data$is_loyal, predict_prob)[1]
# create confusion matrix
confusionMatrix(whole_data$is_loyal, predict_prob, threshold = opt_cutoff)

# mis-classification error (%)
misClassError(whole_data$is_loyal, predict_prob, threshold = opt_cutoff)
# 0.219 -> quite high

# ROC curve + AUC value
library("plotROC")
prediction_table <- data.frame(true_label = whole_data$is_loyal,
                            predict_prob = predict_prob)
roc_curve <- ggplot(prediction_table,
       aes(d = true_label, m = predict_prob)) + geom_roc() + 
  geom_roc() 

roc_curve + style_roc() +
  annotate("text", x = 0.75, y = 0.25, size = 5, 
           label = paste("AUC=", round(calc_auc(roc_curve)$AUC, 3)))



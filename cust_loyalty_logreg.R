library(tidyverse)

internal.data <- read_csv("./data_folder/internal_data.csv")
survey.data <- read_csv("./data_folder/survey_data.csv")
whole_data <- merge(internal.data, survey.data, by = "user_id")
head(whole_data)


str(whole_data)
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

# heatmap
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
# ------- 1) Marketing Factors

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
opt_cutoff <- optimalCutoff(whole_data$is_loyal, predict_prob)[1]

confusionMatrix(whole_data$is_loyal, predict_prob, threshold = opt_cutoff)
misClassError(whole_data$is_loyal, predict_prob, threshold = opt_cutoff)
precision(whole_data$is_loyal, predict_prob, threshold = opt_cutoff)
sensitivity(whole_data$is_loyal, predict_prob, threshold = opt_cutoff)
specificity(whole_data$is_loyal, predict_prob, threshold = opt_cutoff)

# ROC, AUC
library("plotROC")
predict_table <- data.frame(true_label = whole_data$is_loyal,
                            predict_prob = predict_prob)
roc_curve <- ggplot(predict_table,
       aes(d = true_label, m = predict_prob)) + geom_roc() + 
  geom_roc() 

roc_curve + style_roc() +
  annotate("text", x = 0.75, y = 0.25, size = 5, 
           label = paste("AUC=", round(calc_auc(roc_curve)$AUC, 3)))

# ------- Logistic Regression Model Building ...
# ------- Service Satisfaction Factors

service_model <- glm(is_loyal ~
                       depart_on_time + arrive_on_time +
                       register_method + register_rate +
                       class + seat_rate + meal_rate +
                       flight_rate + package_rate,
                     data = whole_data, family=binomial(link="logit"))
summary(service_model)

# positive influence (α=0.05) variables：
# - depart_on_time, arrive_on_time, seat_rate, meal_rate, flight_rate, package_rate
# negative influence (α=0.05) variables：
# - None

predict_prob <- predict(service_model, whole_data, type="response")

opt_cutoff <- optimalCutoff(whole_data$is_loyal, predict_prob)[1] 

confusionMatrix(whole_data$is_loyal, predict_prob, threshold = opt_cutoff)
misClassError(whole_data$is_loyal, predict_prob, threshold = opt_cutoff)
precision(whole_data$is_loyal, predict_prob, threshold = opt_cutoff)
sensitivity(whole_data$is_loyal, predict_prob, threshold = opt_cutoff)
specificity(whole_data$is_loyal, predict_prob, threshold = opt_cutoff)


prediction_table <- data.frame(true_label = whole_data$is_loyal,
                            predict_prob = predict_prob)

roc_curve <- ggplot(prediction_table, aes(d = true_label, m = predict_prob)) +
  geom_roc(n.cuts = 3, labelsize = 3, labelround = 2)
roc_curve + style_roc() +
  annotate("text", x = 0.75, y = 0.25, size = 5,
           label = paste("AUC=", round(calc_auc(roc_curve)$AUC, 3)))


# ------- Logistic Regression Model Building ...
# ------- Both Marketing + Service Satisfaction Factors

full_model <- glm(is_loyal ~ depart_on_time + arrive_on_time +
                    register_method + register_rate +
                    class + seat_rate + meal_rate +
                    flight_rate + package_rate +
                    dm_message + dm_post + dm_email +
                    credit_card_vendor + credit_card_bonus +
                    tv_ad + youtube_ad_1 + youtube_ad_2 + youtube_ad_3,
                  data = whole_data,
                  family = binomial(link="logit"))

summary(full_model)


predict_prob <- predict(full_model, whole_data, type="response")

opt_cutoff <- optimalCutoff(whole_data$is_loyal, predict_prob)[1] 

confusionMatrix(whole_data$is_loyal, predict_prob, threshold = opt_cutoff)
misClassError(whole_data$is_loyal, predict_prob, threshold = opt_cutoff)
precision(whole_data$is_loyal, predict_prob, threshold = opt_cutoff)
sensitivity(whole_data$is_loyal, predict_prob, threshold = opt_cutoff)
specificity(whole_data$is_loyal, predict_prob, threshold = opt_cutoff)


predict_table <- data.frame(true_label = whole_data$is_loyal,
                            predict_prob = predict_prob)

rov_plot <- ggplot(predict_table, aes(d = true_label, m = predict_prob)) +
  geom_roc(n.cuts = 3, labelsize = 3, labelround = 2)
rov_plot + style_roc() +
  annotate("text", x = 0.75, y = 0.25, size = 5,
           label = paste("AUC=", round(calc_auc(rov_plot)$AUC, 3)))

# Data Visualization - overall model
# Create a Variable-Coefficient Table
full_model_summary <- data.frame(variable_name = names(coefficients(full_model)),
           coefficient = coefficients(full_model))

# Filter out marketing variables
rownames(full_model_summary) <-NULL
full_model_summary <- full_model_summary %>%
  filter(variable_name %in% names(coefficients(mrkt_model)) & 
           variable_name != "(Intercept)")

# sort by coefficient and set variable type(factor)
full_model_summary <- full_model_summary[sort(full_model_summary$coefficient, index.return=T)$ix,]
full_model_summary$variable_name <- factor(full_model_summary$variable_name, levels = full_model_summary$variable_name)

ggplot(full_model_summary,
       aes(x=variable_name, y=coefficient)) +
  geom_bar(aes(fill=variable_name),
           stat = "identity",
           show.legend = FALSE,
           position = "dodge") +
  theme_bw() +
  labs(title = "Direct marketing approaching isn't useful.",
       x = "Marketing Strategy",
       y = "Impact on Customer Loyalty'") +
  coord_flip()

# Data Visualization - mrkt model
#  -> for 2 reasons: 1. the results are similar to the overall model.
#  -> 2. some variables may not be statistically important in  the overall model, 
#        but they have positive influence and they seem to still be important.
# Create a Variable-Coefficient Table
mrkt_model_summary <- data.frame(variable_name = names(coefficients(mrkt_model)),
                                  coefficient = coefficients(mrkt_model))
rownames(mrkt_model_summary) <- NULL

mrkt_model_summary <- mrkt_model_summary %>%
  filter(variable_name != "(Intercept)")

mrkt_model_summary <- mrkt_model_summary[sort(mrkt_model_summary$coefficient, index.return = T)$ix,]
mrkt_model_summary$variable_name <- factor(mrkt_model_summary$variable_name,
       levels = mrkt_model_summary$variable_name)

ggplot(mrkt_model_summary,
       aes(x=variable_name, y=coefficient)) +
  geom_bar(aes(fill=variable_name),
           stat = "identity",
           show.legend = FALSE,
           position = "dodge") +
  theme_bw() +
  labs(x = "Marketing Strategy", y = 'Impact on Customer Loyalty') +
  coord_flip()


# Charts for Marketing Manager
# Direct Marketing Method - message, email, post

dm_method <- c("dm_post", "dm_email", "dm_message")
dm_summary <- whole_data %>%
  group_by_(treatment = dm_method[1]) %>%
  summarise(num_of_members = length(user_id),
            num_of_loyal = sum(is_loyal))

dm_summary$method <- dm_method[1]
dm_summary

for(i in 2:3){
  temp <- whole_data %>%
    group_by_(treatment = dm_method[i]) %>%
    summarise(num_of_members = length(user_id),
              num_of_loyal = sum(is_loyal))
  
  temp$method <- dm_method[i]
  dm_summary <- rbind(dm_summary, temp)
}
dm_summary$loyal_proportion <- dm_summary$num_of_loyal / dm_summary$num_of_members
dm_summary$treatment <- as.factor(dm_summary$treatment)
dm_summary$method <- as.factor(dm_summary$method)
dm_summary

ggplot(dm_summary,
       aes(x = method,
           y = loyal_proportion)) +
  geom_bar(aes(fill = treatment),
           position = "dodge",
           stat = "identity") +
  theme_bw() +
  labs(x = "Direct Marketing",
       y = "Proportion of Loyal Members")

# Charts for BD(Business Development) Manager
# Partnership - credit card vendors 
creditcard_summary <- whole_data %>%
  group_by(credit_card_vendor) %>%
  summarize(num_of_members = length(user_id),
            num_of_loyal = sum(is_loyal))

creditcard_summary$loyal_proportion <- creditcard_summary$num_of_loyal / creditcard_summary$num_of_members
creditcard_summary

ggplot(creditcard_summary, 
       aes(x = credit_card_vendor, 
           y = loyal_proportion)) +
  geom_bar(aes(fill=credit_card_vendor), stat = "identity") +
  labs(x = "Credit Card Vendor",
       y = "Proportion of Loyal Members")

# credit card bonus -> bonus level is more important. Which vendor does not matter.
vendor_bonus_summary <- whole_data %>%
  group_by(credit_card_vendor, credit_card_bonus) %>%
  summarise(num_of_member = length(user_id),
            num_of_loyal = sum(is_loyal))

vendor_bonus_summary$loyal_proportion <- vendor_bonus_summary$num_of_loyal / vendor_bonus_summary$num_of_member
vendor_bonus_summary

ggplot(vendor_bonus_summary,
       aes( x = credit_card_vendor,
            y = loyal_proportion)) + 
  geom_bar(aes(fill = credit_card_bonus), 
           position = "dodge",
           stat = "identity") +
  theme_bw() +
  labs(x = "Credit Card Vendor",
       y = "Proportion of Loyal Members")


# Charts for Brand Manager -> brand image.
# tv_ad, youtuve_ad_1,2,3
ad_channel_list <- c("tv_ad", "youtube_ad_1", "youtube_ad_2", "youtube_ad_3")
ad_channel_summary <- whole_data %>%
  group_by_(treatment = ad_channel_list[1]) %>%
  summarise(num_of_member = length(user_id),
            num_of_loyal = sum(is_loyal))

ad_channel_summary$ad_channel <- ad_channel_list[1]
ad_channel_summary

for(i in 2:4){
  temp <- whole_data %>%
    group_by_(treatment = ad_channel_list[i]) %>%
    summarize(num_of_member = length(user_id), 
              num_of_loyal = sum(is_loyal))
  temp$ad_channel <- ad_channel_list[i] 
  ad_channel_summary <- rbind(ad_channel_summary, temp)
}
ad_channel_summary
ad_channel_summary$loyalty_proportion <- ad_channel_summary$num_of_loyal / ad_channel_summary$num_of_member

ad_channel_summary$treatment <- as.factor(ad_channel_summary$treatment)
ad_channel_summary$ad_channel <- as.factor(ad_channel_summary$ad_channel)

ggplot(ad_channel_summary,
       aes(x = ad_channel,, y = loyalty_proportion)) +
  geom_bar(aes(fill=treatment),
           position = "dodge",
           stat = "identity") +
  theme_bw() +
  labs(x = 'Ad', y = "Proportion of Loyal Members")

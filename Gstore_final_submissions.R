#get data in correct form
library(dplyr)
library(ranger)
library(tidyverse)
gstore <- gstore %>% group_by(fullVisitorId) %>% mutate(total_spend_visitor = sum(totals.transactionRevenue))
by_visitor_min <- gstore %>% select(fullVisitorId, total_spend_visitor, device.operatingSystem, device.browser,
                                    geoNetwork.city, geoNetwork.metro, date, geoNetwork.continent, geoNetwork.country,
                                    geoNetwork.region, geoNetwork.subContinent,totals.pageviews)


by_visitor_min$total_spend_visitor <- ifelse(by_visitor_min$total_spend_visitor < 100, 1, by_visitor_min$total_spend_visitor)

by_visitor_min <- by_visitor_min %>% group_by(fullVisitorId) %>% mutate(mean_page_view = mean(totals.pageviews))

by_visitor_min <- by_visitor_min %>% select(-date) %>% select(-totals.pageviews)

by_visitor_min <- by_visitor_min %>% group_by(fullVisitorId) %>% mutate(numVisits = n())

b <- ifelse(duplicated(by_visitor_min$fullVisitorId), by_visitor_min, NA)
b<-by_visitor_min %>% distinct(fullVisitorId, .keep_all = T)

####################################
#rf submission
####################################
rf_all <- ranger(log(total_spend_visitor) ~ geoNetwork.metro + device.operatingSystem +
                   device.browser + numVisits + geoNetwork.continent + 
                   geoNetwork.country + geoNetwork.region + 
                   geoNetwork.subContinent + mean_page_view + geoNetwork.city, data = b)
final_pred <- predict(rf_all, b)
f <- round(final_pred$predictions, digits = 10)
submit_df_rf <- data.frame(b$fullVisitorId, f)

names(submit_df_rf) <- c("fullVisitorId","PredictedLogRevenue")

submit_df_rf$PredictedLogRevenue <-format(submit_df_rf$PredictedLogRevenue, scientific = FALSE) 
write.csv(submit_df_rf,file="gstore_submission.csv",row.names = F) 
######################################

######################################
#second rf submission
######################################
rf_all <- ranger(log(total_spend_visitor) ~ geoNetwork.metro + device.operatingSystem +
                   device.browser + numVisits + geoNetwork.continent + 
                   geoNetwork.country + geoNetwork.region + 
                   geoNetwork.subContinent + geoNetwork.city, data = b)
final_pred <- predict(rf_all, b)
final_pred <- final_pred$predictions/(167/61)
f <- round(final_pred, digits = 10)
submit_df_rf <- data.frame(b$fullVisitorId, f)

names(submit_df_rf) <- c("fullVisitorId","PredictedLogRevenue")

submit_df_rf$PredictedLogRevenue <-format(submit_df_rf$PredictedLogRevenue, scientific = FALSE) 
write.csv(submit_df_rf,file="gstore_submission.csv",row.names = F) 


###########################
#elnet submission
###########################
y <- log(b$total_spend_visitor)
X <- model.matrix(log(total_spend_visitor) ~ geoNetwork.metro + device.operatingSystem +
                    device.browser + numVisits +
                    geoNetwork.country , data = b)
elnet <- glmnet(X, y, alpha = .5, lambda = .3036989)
elnet_pred <- predict(elnet, X)
elnet_pred

final_pred <- elnet_pred/(167/61)
f <- round(final_pred, digits = 10)
submit_df_rf <- data.frame(b$fullVisitorId, f)

names(submit_df_rf) <- c("fullVisitorId","PredictedLogRevenue")

submit_df_rf$PredictedLogRevenue <-format(submit_df_rf$PredictedLogRevenue, scientific = FALSE) 
write.csv(submit_df_rf,file="gstore_submission3.csv",row.names = F) 



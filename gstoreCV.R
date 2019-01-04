#create dataframe from gstore.csv to visitorID by row with only variables that could be known in future months. 
gstore <- gstore %>% group_by(fullVisitorId) %>% mutate(total_spend_visitor = sum(totals.transactionRevenue))
by_visitor_min <- gstore %>% select(fullVisitorId, total_spend_visitor, device.operatingSystem, device.browser,
                  geoNetwork.city, geoNetwork.metro, date, geoNetwork.continent, geoNetwork.country,
                  geoNetwork.region, geoNetwork.subContinent,totals.pageviews)
by_visitor_min$total_spend_visitor <- ifelse(by_visitor_min$total_spend_visitor < 100, 1, by_visitor_min$total_spend_visitor)

may_thru_aug <- by_visitor_min %>% filter(date <= "2018-08-15")
aug_thru_oct <- by_visitor_min %>% filter(date > "2018-08-15")
nrow(may_thru_aug) + nrow(aug_thru_oct)
##########################################################
#attempt to create mean time on page delete if bad
may_thru_aug <- may_thru_aug %>% group_by(fullVisitorId) %>% mutate(mean_page_view = mean(totals.pageviews))
aug_thru_oct <- aug_thru_oct %>% group_by(fullVisitorId) %>% mutate(mean_page_view = mean(totals.pageviews))
#check
id <- may_thru_aug$fullVisitorId[2]
a <- may_thru_aug %>% filter(fullVisitorId == id)
#it worked
##########################################################
may_thru_aug <- may_thru_aug %>% select(-date) %>% select(-totals.pageviews)
aug_thru_oct <- aug_thru_oct %>% select(-date) %>% select(-totals.pageviews)

may_thru_aug <- may_thru_aug %>% group_by(fullVisitorId) %>% mutate(numVisits = n())
aug_thru_oct <- aug_thru_oct %>% group_by(fullVisitorId) %>% mutate(numVisits = n())

may_thru_aug <- unique( may_thru_aug[ , 1:ncol(may_thru_aug)] )
aug_thru_oct <- unique( aug_thru_oct[ , 1:ncol(aug_thru_oct)] )

may_thru_aug %>% filter(fullVisitorId == may_thru_aug$fullVisitorId[1])

#completed above tast for a train set...may-aug/15(106 days) and a test set aug/15-oct/15(61 days)

#now want to create df with all users in may_thru_aug that has their spending in aug/15-oct/15
users_from_train_no_spend_in_test <- anti_join(may_thru_aug, aug_thru_oct, by = "fullVisitorId")
users_from_train_no_spend_in_test$total_spend_visitor <- 1
users_from_train_spend_in_test <- semi_join(may_thru_aug, aug_thru_oct, by = "fullVisitorId")
val_set <- rbind(users_from_train_no_spend_in_test, users_from_train_spend_in_test)
#val_set is the all users form may_thru_aug's spending in aug/15-oct15

#this line makes it so that when validating we are doing the correct vectorwise operations
may_thru_aug <- may_thru_aug[order(may_thru_aug$fullVisitorId),]
val_set <- val_set[order(val_set$fullVisitorId),]
#check 
table(may_thru_aug$fullVisitorId == val_set$fullVisitorId)

#######################################################
#Preliminary models
#######################################################

#starting with extremely simple models and building up
sqrt(sum((0 - log(val_set$total_spend_visitor))^2)/nrow(val_set))
#predict 0 for all model has rmse of .9819


#intercept only mod slightly better than 0 for everyone
int_mod <- mean(log(may_thru_aug$total_spend_visitor))
sqrt(sum((int_mod/(106/61) - log(val_set$total_spend_visitor))^2)/nrow(val_set))
#intercept rmse = .9858

#only using metro variable to predict
metro_mod <- lm(log(total_spend_visitor) ~ geoNetwork.city, data = may_thru_aug)
metro_pred <- predict(metro_mod, may_thru_aug)

sqrt(sum((metro_pred/(106/61) - log(val_set$total_spend_visitor))^2)/nrow(val_set))
#metro rmse barely better with .9896

#using all variables (but only one location) to predict
all_mod <- lm(log(total_spend_visitor) ~ geoNetwork.metro + device.operatingSystem +
                  device.browser + numVisits + mean_page_view, data = may_thru_aug)
all_pred <- predict(all_mod, may_thru_aug)

sqrt(sum((all_pred/(106/61) - log(val_set$total_spend_visitor))^2)/nrow(val_set))
#all variables rmse barely better with 1.0146

#now using all variables including redudant locations to predict
all_loc_mod <- lm(log(total_spend_visitor) ~ geoNetwork.metro + device.operatingSystem +
                    device.browser + numVisits + geoNetwork.continent + 
                    geoNetwork.country + geoNetwork.region + 
                    geoNetwork.subContinent + mean_page_view, data = may_thru_aug)
all_loc_pred <- predict(all_loc_mod, may_thru_aug)

sqrt(sum((all_loc_pred/(106/61) - log(val_set$total_spend_visitor))^2)/nrow(val_set))
#still barely decreases rmse...1.015368

#using elastic net to regularize
library(glmnet)
y <- log(may_thru_aug$total_spend_visitor)
X <- model.matrix(log(total_spend_visitor) ~ geoNetwork.metro + device.operatingSystem +
                    device.browser + numVisits +
                    geoNetwork.country , data = may_thru_aug)
elnet <- glmnet(X, y, alpha = .5)
elnet_pred <- predict(elnet, X)

rmse_elnet <- c()
for (i in 1:ncol(elnet_pred)){
  rmse_elnet[i] <- sqrt(sum((elnet_pred[,i]/(106/61) - log(val_set$total_spend_visitor))^2)/nrow(val_set))
}
min(rmse_elnet)
rmse_elnet
elnet$lambda
lambda <- 0.3036988681

#best is .97506

#LASSO
library(glmnet)
y <- log(may_thru_aug$total_spend_visitor)
X <- model.matrix(log(total_spend_visitor) ~ geoNetwork.metro + device.operatingSystem +
                    device.browser + numVisits +
                    geoNetwork.country , data = may_thru_aug)
las <- glmnet(X, y, alpha = 1)
las_pred <- predict(las, X)

rmse_las <- c()
for (i in 1:ncol(las_pred)){
  rmse_las[i] <- sqrt(sum((las_pred[,i]/(106/61) - log(val_set$total_spend_visitor))^2)/nrow(val_set))
}
min(rmse_las)
rmse_las
las$lambda

#LASSO

#RIDGE
library(glmnet)
y <- log(may_thru_aug$total_spend_visitor)
X <- model.matrix(log(total_spend_visitor) ~ geoNetwork.metro + device.operatingSystem +
                    device.browser + numVisits +
                    geoNetwork.country , data = may_thru_aug)
rid <- glmnet(X, y, alpha = 1)
rid_pred <- predict(las, X)

rmse_rid <- c()
for (i in 1:ncol(rid_pred)){
  rmse_rid[i] <- sqrt(sum((rid_pred[,i]/(106/61) - log(val_set$total_spend_visitor))^2)/nrow(val_set))
}
min(rmse_rid)
rmse_rid
rid$lambda

#RIDGE




#try randomForest with all variables
rf_all <- ranger(log(total_spend_visitor) ~ geoNetwork.metro + device.operatingSystem +
         device.browser + numVisits + 
         geoNetwork.country , data = may_thru_aug)
rf_all_pred <- predict(rf_all, may_thru_aug)
sqrt(sum((rf_all_pred$predictions/(106/61) - log(val_set$total_spend_visitor))^2)/nrow(val_set))
#rf significantly improves predictions...rmse .98198. Lets try with only one location

#now try gradient boosting
library(xgboost)
dat_XGB <- xgb.DMatrix(data = X, label = y)

?xgboost
Sys.time()
gb_1 <- xgboost(data =dat_XGB, nrounds = 2000,
        eta = .01, 
        max_depth = 2,
        subsample = .5,
        verbose = 1)
Sys.time()

gb_1_pred <- predict(gb_1, X)
gb_1_pred <- ifelse(gb_1_pred <0 , 0, gb_1_pred)
sqrt(sum((gb_1_pred/(106/61) - log(val_set$total_spend_visitor))^2)/nrow(val_set))

Sys.time()
gb_2 <- xgboost(data =dat_XGB, nrounds = 100,
                eta = .01, 
                max_depth = 2,
                subsample = .5,
                verbose = 1)
Sys.time()
gb_2_pred <- predict(gb_2, X)
gb_2_pred
gb_2_pred <- ifelse(gb_2_pred <0 , 0, gb_2_pred)
sqrt(sum((gb_2_pred - log(val_set$total_spend_visitor))^2)/nrow(val_set))

Sys.time()
gb_3 <- xgboost(data =dat_XGB, nrounds = 5000,
                eta = .01, 
                max_depth = 2,
                subsample = .5,
                verbose = 1)
Sys.time()
gb_2_pred <- predict(gb_2, X)
gb_2_pred
gb_2_pred <- ifelse(gb_2_pred <0 , 0, gb_2_pred)
sqrt(sum((gb_2_pred - log(val_set$total_spend_visitor))^2)/nrow(val_set))

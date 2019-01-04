#Notes about script: This script creates gstore.csv, a much smaller, much more 
#manageable dataframe than the test data given. Irrelevant variables have been removed and 
#variables with many levels have been condensed.  

##############################################################
##Dependencies##
##############################################################
library(dplyr)
library(tidyverse)
library(jsonlite)
library(skimr)
library(lubridate)
library(forcats)

##############################################################
#Import data#
##############################################################

test <- read_csv("Downloads/test_v2.csv")

#Flatten data

json_col <- c("device","geoNetwork","totals","trafficSource")
json_to_df <- function(col, df) {
  paste("[", paste(df[[col]], collapse = ","), "]") %>% # create valid JSON string
    fromJSON(flatten = T) %>% # convert JSON to data frames
    `colnames<-`(paste(col, colnames(.), sep = ".")) # add prefix to column names
}

test <- json_col %>%
  map_dfc(json_to_df, df=test) %>% 
  cbind(select(test, -one_of(json_col))) %>% 
  as_tibble

# "not available.." to NA
to_na <- function(col) ifelse(col %in% c("(not set)", "not available in demo dataset"), NA, col)
test <- test %>% mutate_if(is.character, to_na)

# delete columns with no variance
test <- test %>% select(-one_of(has_no_variance))

# save as csv
write_csv(test, "test_flat.csv")

##############################################################
#Clean data
##############################################################
library(dplyr)
library(forcats)
test_flat <- test_flat %>% select(-hits)
test_flatF <- test_flat
test_flatF <- test_flatF %>% select(-fullVisitorId) 
#test_flatF$fullVisitorId <- as.numeric(test_flat$fullVisitorId)
#clump factors
test_flatF <- as.data.frame(unclass(test_flatF))
test_flatF <- test_flatF %>% mutate_if(is.factor, fct_lump, prop = 0.01)
str(test_flatF)

fullVisitorId<-test_flat$fullVisitorId
test_flatF$fullVisitorId <- fullVisitorId
test_flat <- test_flatF

gstore <- test_flat %>% select(trafficSource.adContent, trafficSource.keyword, trafficSource.source,trafficSource.campaign,
                               trafficSource.referralPath, device.operatingSystem, device.browser, device.isMobile,
                               device.deviceCategory, geoNetwork.city, geoNetwork.continent, geoNetwork.country, geoNetwork.metro,
                               geoNetwork.region, geoNetwork.subContinent, totals.pageviews, totals.hits, totals.timeOnSite, 
                               totals.bounces, totals.newVisits, totals.pageviews, totals.sessionQualityDim, totals.totalTransactionRevenue,
                               totals.transactionRevenue, totals.visits, trafficSource.medium, trafficSource.adwordsClickInfo.page,
                               trafficSource.adwordsClickInfo.slot, trafficSource.adwordsClickInfo.adNetworkType, trafficSource.adwordsClickInfo.isVideoAd,
                               channelGrouping, customDimensions, date, fullVisitorId, visitId, visitNumber, visitStartTime)
dim(gstore)
gstore$fullVisitorId[1]
###############################################
#Gstore data made, now get rid of NAs
###############################################

col_na <- c()
for (i in 1:ncol(gstore)){
  col_na[i] <- sum(is.na(gstore[,i]))
}
col_na
names(gstore)
#only 11 columns with NA. Now will go one column at a time to figure out
#how imputation should be done

#trafficSource.keyword
names(gstore)[2]
table(gstore$trafficSource.keyword)
gstore$trafficSource.keyword <- ifelse(is.na(gstore$trafficSource.keyword), "N/A", gstore$trafficSource.keyword)

#totals.pageviews
names(gstore)[16]
table(gstore$totals.pageviews)
gstore$totals.pageviews <- ifelse(is.na(gstore$totals.pageviews), 1, gstore$totals.pageviews)

#imputing timeOnSite is harder. I find that an exponential distribution
# with lambda = 1/(mean(timeOnSite) + a little) fits pretty well as seen in the 
#histograms
names(gstore)[18]
table(gstore$totals.timeOnSite)
l <- 1/mean(gstore$totals.timeOnSite, na.rm = T)
v <- l^2 
1/var(gstore$totals.timeOnSite, na.rm = T)
hist(gstore$totals.timeOnSite, xlim = range(0,2000), breaks = 1000)
hist(rexp(1000, l + .0029), xlim = range(0,2000), breaks = 100)
#gstore$totals.timeOnSite_NoNA <- ifelse(is.na(gstore$totals.timeOnSite), rexp(1, l + .0029), gstore$totals.timeOnSite)
#using median for now
gstore$totals.timeOnSite_NoNA <- ifelse(is.na(gstore$totals.timeOnSite), 90, gstore$totals.timeOnSite)
summary(gstore$totals.timeOnSite)

#total bounds
names(gstore)[19]
table(gstore$totals.bounces)
gstore$totals.bounces <- ifelse(is.na(gstore$totals.bounces), 0, 1)

#totals.newVisits
names(gstore)[20]
table(gstore$totals.newVisits)
gstore$totals.newVisits <- ifelse(is.na(gstore$totals.newVisits), 0, 1)

#TotalTransactionRevenue
names(gstore)[22]
gstore$totals.totalTransactionRevenue <- ifelse(is.na(gstore$totals.totalTransactionRevenue), 1, gstore$totals.totalTransactionRevenue)

#totals.transactionRevenue
names(gstore)[23]
gstore$totals.transactionRevenue <- ifelse(is.na(gstore$totals.transactionRevenue), 1, gstore$totals.transactionRevenue)

#trafficSource.adwordsClickInfo.page
names(gstore)[26]
table(gstore$trafficSource.adwordsClickInfo.page)
gstore$trafficSource.adwordsClickInfo.page <- ifelse(is.na(gstore$trafficSource.adwordsClickInfo.page),
                                                     0, gstore$trafficSource.adwordsClickInfo.page)

#trafficSource.adwordsClickInfo.slot
names(gstore)[27]
table(gstore$trafficSource.adwordsClickInfo.slot)
gstore$trafficSource.adwordsClickInfo.slot <- ifelse(is.na(gstore$trafficSource.adwordsClickInfo.slot),
                                                     "other", gstore$trafficSource.adwordsClickInfo.slot)

#trafficSource.adwordsClickInfo.slot
names(gstore)[28]
table(gstore$trafficSource.adwordsClickInfo.adNetworkType)
gstore$trafficSource.adwordsClickInfo.adNetworkType <- ifelse(is.na(gstore$trafficSource.adwordsClickInfo.adNetworkType),
                                                              "other", gstore$trafficSource.adwordsClickInfo.adNetworkType)

#trafficSource.adwordsClickInfo.slot
names(gstore)[29]
table(gstore$trafficSource.adwordsClickInfo.isVideoAd)
gstore$trafficSource.adwordsClickInfo.isVideoAd <- ifelse(is.na(gstore$trafficSource.adwordsClickInfo.isVideoAd),
                                                          1,0)

gstore <- mutate_if(gstore, is.character, as.factor)

#################################################
#lubridate stuff
#################################################
library(lubridate)
gstore$date <- ymd(gstore$date)
?wday
gstore$day_of_week <- as.factor(wday(gstore$date))
gstore$visitStartTime[5]
gstore$date[5]
write_csv(gstore, "gstore.csv")
summary(gstore$date)
max(gstore$date) - min(gstore$date)

gstore_may <- gstore %>% filter(date <= "2018-06-25")
summary(gstore_may$date)
gstore_july <- gstore %>% filter(date <= "2018-08-20" & date > "2018-06-25")
summary(gstore_july$date)
gstore_sept <- gstore %>% filter(date > "2018-08-20")

holdout_id <- sample(gstore$fullVisitorId, size = length(unique(gstore$fullVisitorId))/2)
table(gstore_may$fullVisitorId %in% holdout_id)
may_train <- gstore_may[gstore_may$fullVisitorId %in% holdout_id,] 
may_test <- gstore_may[!(gstore_may$fullVisitorId %in% holdout_id),]
july_train <- gstore_july[gstore_july$fullVisitorId %in% holdout_id,] 
july_test <- gstore_july[!(gstore_july$fullVisitorId %in% holdout_id),]
sept_train <- gstore_sept[gstore_sept$fullVisitorId %in% holdout_id,] 
sept_test <- gstore_sept[!(gstore_sept$fullVisitorId %in% holdout_id),]

may_july_train <- rbind(may_train, july_train)
may_sept_train <- rbind(may_train, sept_train)
july_sept_train <- rbind(july_train, sept_train)

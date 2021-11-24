# Data Science Use Cases - Working with Time Series
# TS Modeling and Optimization in Ad Tech
#
# Contents
#         Part   I - Data Exploration
#         Part  II - Time Series Analysis
#         Part III - A/B Testing Over Time



###############################################
# Ad Optimization - Part I - Data Exploration #
###############################################

library(dplyr)
library(ggplot2)
library(tsibble) # Allows us to make a tsibble (tidy time series object)
library(fable)   # Contains timeseries modeling tools
library(feasts)  # Allows multiple ts models to be fit at once


###########################################################################
# A. Load the data from ts_analysis_2020/data/hourly_ad_category_data.csv #
###########################################################################

ad_data <- read.csv("data/hourly_ad_category_data.csv",
                    colClasses = c("ts" = "POSIXct"))


#####################################
# B. View and/or summarize the data #
#####################################

View(ad_data)
summary(ad_data)


############################################
# C. What problems do you see in the data? #
############################################

# Let's check for missing values
ad_data %>%
  summarize(across(everything(), list(function(c) {sum(is.na(c))} )))

# Why are there sometimes more impressions than clicks?
ad_data %>%
  filter(imps < clicks) %>%
  count()


##################################################################
# D. Try to summarize the data using statistics / visualizations #
##################################################################

# RPI and RPC by ad_type
ad_data %>%
  filter(!is.na(total_rev)) %>%
  group_by(ad_type) %>%
  summarize(
    rpc = sum(total_rev) / sum(clicks),
    rpi = sum(total_rev) / sum(imps)
  )

# Visualize RPC
ggplot(data = ad_data,
       aes(ts, rpc, col = ad_type)) +
  geom_point() +
  geom_smooth()

# Visualize RPC (just show one day)
ggplot(data = ad_data %>% filter(ts >= "2019-05-13"),
       aes(ts, rpc, col = ad_type)) +
  geom_point() +
  geom_smooth()




####################################################
# Ad Optimization - Part II - Time Series Analysis #
####################################################


#########################################################
# A. Convert rpc's for each ad into time-series objects #
#########################################################

ts_ad_data = ad_data %>% as_tsibble(key=ad_type, index=ts)


####################################################
# B. Create visualizations using STL decomposition #
####################################################

ts_ad_data %>%
  model(arima = ARIMA(rpc ~ trend())) %>%
  interpolate(ts_ad_data) %>%
  autoplot()

ts_ad_data %>%
  model(arima = ARIMA(rpc ~ trend())) %>%
  interpolate(ts_ad_data) %>%
  model(STL(rpc ~ trend() + season('day'))) %>%
  components() %>%
  autoplot()


#####################################
# C. Separate out the training data #
#####################################

summary(ts_ad_data) # last day is 2021-11-23

# Leave out the last day for testing
train <- ts_ad_data %>%
  filter(ts < as.Date("2021-11-23"))


#########################################
# D. Impute nulls in training data only #
#########################################

train <- train %>%
  model(arima = ARIMA(rpc ~ trend())) %>%
  interpolate(train)


#####################################
# E. Fit an ARIMA, ETS, etc. Models #
#####################################

models <- train %>%
  model(
    snaive = SNAIVE(rpc ~ lag("day")),
    ets = ETS(rpc),
    arima = ARIMA(rpc)
  ) %>%
  mutate(
    ets_arima = (ets + arima) / 2
  )


###########################
# F. Generate Predictions #
###########################

predictions <- models %>% forecast(h = "24 hours")


##############################
# G. Check Accuracy Measures #
##############################

accuracy(predictions, ts_ad_data) %>%
  arrange(ad_type, MASE)
# Based on the results here which model would you choose? Why?

# Check out some forecast plots

predictions %>%
  filter(ad_type == "dog_food") %>%
  autoplot(ts_ad_data)

predictions %>%
  filter(ad_type == "cat_toys") %>%
  autoplot(ts_ad_data)

predictions %>%
  filter(.model == "ets") %>%
  filter(ad_type == "phone_service") %>%
  autoplot(ts_ad_data)





#########################
# A/B Testing Over Time #
#########################

library(data.table)
library(ggplot2)


############################
# A. Day1 Model Comparison #
############################

# On day 1 we sent 20% of traffic to the new optimizer.

# Read in and check the data
day1 <- fread("data/day1.csv",
              colClasses = c("variation"="factor", "date"="Date"))
head(day1)

# Create confidence intervals for the RPS estimates
stats <- day1[, .(m = mean(rps),
                  sd = sd(rps), .N,
                  moe = 1.96*sd(rps)/sqrt(.N)
                  ),
              by = variation]
stats[, `:=`(lower = m - moe,
             upper = m + moe)]

print(stats)
ggplot(stats, aes(x=variation, y=m, ymin=lower, ymax=upper)) +
  geom_pointrange() +
  coord_flip()

# Does it seems safe to send more traffic to the new optimizer?



############################
# B. Day2 Model Comparison #
############################

# On day 2 we sent 50% of traffic to the new optimizer.

# Repeat the analysis for the second day
day2 <- fread("data/day2.csv",
              colClasses = c("variation"="factor", "date"="Date"))
stats <- day2[, .(m = mean(rps),
                  sd = sd(rps),
                  .N,
                  moe = 1.96*sd(rps)/sqrt(.N)
                  ),
              by = variation]
stats[, `:=`(lower = m - moe,
             upper = m + moe)]

print(stats)
ggplot(stats, aes(x=variation, y=m, ymin=lower, ymax=upper)) +
  geom_pointrange() +
  coord_flip()

# Does it make sense to increase the traffic % again?



############################
# C. Day3 Model Comparison #
############################

# On day 3 we sent 80% of traffic to the new optimizer.

# Repeat the analysis for the third day
day3 <- fread("data/day3.csv",
              colClasses = c("variation"="factor", "date"="Date"))
head(day3)

stats <- day3[, .(m = mean(rps),
                  sd = sd(rps),
                  .N,
                  moe = 1.96*sd(rps)/sqrt(.N)
                  ), by = variation]
stats[, `:=`(lower = m - moe,
             upper = m + moe)]
print(stats)
ggplot(stats, aes(x=variation, y=m, ymin=lower, ymax=upper)) +
  geom_pointrange() +
  coord_flip()


##########################
# D. Combined Comparison #
##########################

# Great things look good! Let's combine the three days, so we can report
# back to the team...
combined <- rbind(day1, day2, day3)

stats <- combined[, .(m = mean(rps),
                      sd = sd(rps),
                      .N,
                      moe = 1.96*sd(rps)/sqrt(.N)
                      ),
                  by = variation]
stats[, `:=`(lower = m - moe,
             upper = m + moe)]

print(stats)
ggplot(stats, aes(x=variation, y=m, ymin=lower, ymax=upper)) +
  geom_pointrange() +
  coord_flip()

# Wait... what?! What happened? Why?

# Visualize RPS
ggplot(data = combined[, .(mean_rps = mean(rps), .N), by = .(variation, date)],
       aes(variation, y = mean_rps, x = date, col = variation, size = N)) +
  geom_point()

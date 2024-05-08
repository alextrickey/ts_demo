# Data Science Use Cases - Working with Time Series
# TS Modeling and Optimization in Ad Tech
#
# Contents
#         Part   I - Data Exploration
#         Part  II - Time Series Modeling
#         Part III - Optimization [Optional]
#         Part  IV - A/B Testing Over Time


#############################
# Part I - Data Exploration #
#############################

# Instructions: 
#    Load the data using language/libraries of your choice. 
#    Check the data types of the loaded columns. 
#    Check for data quality issues. 
#    Summarize / visualize the time series. 





##################################
# Part II - Time Series Modeling #
##################################

# Instructions: 
#    Choose a modeling strategy and prepare the data.
#    Define training and testing sets. 
#    How does your strategy handle missing data?
#    Generate predictions for the test data. 
#    Calculate metrics to measure the prediction quality.





######################################
# Part III - Optimization [Optional] #
######################################

# Instructions: 
#    Consider how you could use your predictions to choose which ad to show. 
#    Define an algorithm implementing your approach. 
#    Assuming we need to re-train our model each day, what implications will 
#        your optimization have for tomorrow's training's model?





###################################
# Part IV - A/B Testing Over Time #
###################################

# Setup: 
#     Suppose, we implemented your optimization strategy from the previous 
#     part and use it to choose which ad to play based on the forecasts.
#     
#     We rolled this feature out over several days. On day1 we sent 20% of the
#     traffic to the new feature (and left 80% on baseline). On day2, we sent
#     the feature 50% of traffic and on day3 80%. 

# Instructions
#     Load and investigate the data for day1 in "data/day1.csv".
#     Which strategy is performing better?
#     Did we make the right decision to increase the traffic?
#     Repeat this analysis for day2 and day3. 
#     Considering all the performance on all three days, what effect do you 
#         think the feature is having on our revenue?





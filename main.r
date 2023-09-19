# set the working directory
setwd("/Users/guanrui/Desktop/S&DS625/Gymnastics")

# source the helper scripts
source("clean_data.r")
source("craft_variables.r")
source("fit_model.r")
source("select_competitor.r")

# clean the data for both Olympic and championship datasets
# cleaned_olympic_data <- clean_olympic_data()

data <- clean_championship_data()

# preview the cleaned data
head(data)

# craft variables for datasets
variables <- craft_variables(data)
# preview
head(variables)
tail(variables)

table(variables$Country)

model <- fit_full_logistic_model(variables)
variables$prob = predict(model, newdata=variables, type='response')
head(variables)

# Finding top 12 countries and the five athletes for each team
top_countries_w <- topn_countries(data = variables, 
                                  gender = 'w', 
                                  topn = 12)
top_countries_w
top_countries_m <- topn_countries(data = variables, 
                                  gender = 'm', 
                                  topn = 12) 
top_countries_m

top5_w <- lapply(top_countries_w$Country, top5, gender = 'w')
top5_w
top5_m <- lapply(top_countries_m$Country, top5, gender = 'm')
top5_m

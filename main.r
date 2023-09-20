# set the working directory
setwd("/Users/guanrui/Desktop/S&DS625/Yale-SDS625-Gymnastics")

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

# Q: the Round choices we now have: aa, AAfinal, AAqual, final qual TeamFinal
aa_variables <- craft_aa_variables(data)
# preview
head(variables)
tail(variables)
head(aa_variables)

table(variables$Country)

model <- fit_full_logistic_model(variables)
variables$prob = predict(model, newdata=variables, type='response')
head(variables)

aa_model <- fit_full_logistic_model(aa_variables)
aa_variables$prob = predict(aa_model, newdata=aa_variables, type='response')
head(aa_variables)

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


vaults = c("VT", "VT1", "VT2")
# Finding the 36 individuals from countries that did not qualify
individual_w <- top_individual(data = variables, gender = 'w', 
                      country_list = top_countries_w, bucket = 4, 
                      vaults = vaults)

individual_m <- top_individual(data = variables, gender = 'm', 
                               country_list = top_countries_m, bucket = 6, 
                               vaults = vaults)

individual_w %>% select(Name, Apparatus, Gender, Country, prob)
individual_m %>% select(Name, Apparatus, Gender, Country, prob)


# All unique names check (should be 36)
length(unique(individual_w$Name))
length(unique(individual_m$Name))

# None from top 12 countries check (should be 0)
sum(individual_w$Country %in% top_countries_w$Country)
sum(individual_m$Country %in% top_countries_m$Country)


# Countries are not being read in. See example below. Halvorsen should have 
# country of NOR according to the Excel file
# Solution: Excel file is missing country codes --> filter them out
y <- read.csv("data/data_2022_2023.csv", as.is = TRUE, stringsAsFactors = FALSE) %>% mutate_if(is.character, as.character)
y %>% filter(Country == "")
data %>% filter(LastName == "Halvorsen")

# Selection process of the 5-person team.
head(variables)
head(aa_variables)
candidates_w <- candidate_selection(variables, aa_variables, "w", 2, vaults)
candidates_w

candidates_m <- candidate_selection(variables, aa_variables, "m", 2, vaults)
candidates_m




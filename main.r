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


# # Filtering out people from combo list
# model <- model_name(data)
# data$score <- predict(model, newdata = data)
#
# #Initial pool from men and women for top 12 countries
# mens <- data %>%
#   filter(Gender == 'm', Country %in% top12countries_m) %>%
#   group_by(Country, Apparatus) %>%
#   arrange(desc(score)) %>%
#   slice_head(n = 7)
#
# womens <- data %>%
#   filter(Gender == 'w', Country %in% top12countries_w) %>%
#   group_by(Country, Apparatus) %>%
#   arrange(desc(score)) %>%
#   slice_head(n = 7)
#
# # Finding contenders for combos
# #top12 women
# contenders_w_USA <- contenders(data = data, gender == 'w', country == "USA")
# contenders_w_CHN <- contenders(data = data, gender == 'w', country == "CHN")
# contenders_w_GBR <- contenders(data = data, gender == 'w', country == "GBR")
# contenders_w_ITA <- contenders(data = data, gender == 'w', country == "ITA")
# contenders_w_BRA <- contenders(data = data, gender == 'w', country == "BRA")
# contenders_w_FRA <- contenders(data = data, gender == 'w', country == "FRA")
# contenders_w_JPN <- contenders(data = data, gender == 'w', country == "JPN")
# contenders_w_BEL <- contenders(data = data, gender == 'w', country == "BEL")
# contenders_w_GER <- contenders(data = data, gender == 'w', country == "GER")
# contenders_w_NED <- contenders(data = data, gender == 'w', country == "NED")
# contenders_w_CAN <- contenders(data = data, gender == 'w', country == "CAN")
# contenders_w_AUS <- contenders(data = data, gender == 'w', country == "AUS")
#
# #top12 men
# contenders_m_USA <- contenders(data = data, gender == 'm', country == "USA")
# contenders_m_JPN <- contenders(data = data, gender == 'm', country == "JPN")
# contenders_m_GBR <- contenders(data = data, gender == 'm', country == "GBR")
# contenders_m_CHN <- contenders(data = data, gender == 'm', country == "CHN")
# contenders_m_ITA <- contenders(data = data, gender == 'm', country == "ITA")
# contenders_m_ENG <- contenders(data = data, gender == 'm', country == "ENG")
# contenders_m_SUI <- contenders(data = data, gender == 'm', country == "SUI")
# contenders_m_GER <- contenders(data = data, gender == 'm', country == "GER")
# contenders_m_TUR <- contenders(data = data, gender == 'm', country == "TUR")
# contenders_m_ESP <- contenders(data = data, gender == 'm', country == "ESP")
# contenders_m_UKR <- contenders(data = data, gender == 'm', country == "UKR")
# contenders_m_BRA <- contenders(data = data, gender == 'm', country == "BRA")
#
# contenders <- function(data, gender, country){
#   if(gender == 'w'){
#     people <- data %>%
#       filter(Country == country, Name %in% womens$Name) %>%
#       pivot_wider(names_from = Apparatus, values_from = score) %>%
#       replace(is.na(.), 0)
#     return(people)
#   }
#
#   if(gender == 'm'){
#     people <- data %>%
#       filter(Country == country, Name %in% mens$Name) %>%
#       pivot_wider(names_from = Apparatus, values_from = score) %>%
#       replace(is.na(.), 0)
#     return(people)
#   }
# }

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



# Selection process of the 5-person team.
head(variables)
head(aa_variables)
candidates_w <- candidate_selection(variables, aa_variables, "w", 2, vaults)
candidates_w

candidates_m <- candidate_selection(variables, aa_variables, "m", 2, vaults)
candidates_m

# set the working directory
setwd("/Users/guanrui/Desktop/S&DS625/Yale-SDS625-Gymnastics")
source("select_competitor.r")
library(tools)

############# DATA CLEANING #################
source("clean_data.r")
# Note: if you want to do the same thing for olympic dataset, try this function
# data <- clean_olympic_data()

data <- clean_championship_data()
head(data)
# Note: we still have 26 entries that not have Country

############# MODEL BUILDING (LOGISTIC) #######
source("craft_variables.r")
source("fit_model.r")
prob_variables <- craft_prob_variables(data)
# Note: only part of the gymnasts have ever taken part in the AAfinal competition
# so this table does not contain every gymnast.
prob_aa_variables <- craft_prob_aa_variables(data)
# fit model
logit_model <- fit_full_logistic_model(prob_variables)
logit_aa_model <- fit_full_logistic_model(prob_aa_variables)

############# MODEL BUILDING (LINEAR) ##########
source("craft_variables.r")
source("fit_model.r")
score_variables <- craft_score_variables(data, "final")
score_aa_variables <- craft_score_variables(data, "AAfinal")
# Note: we have Round: aa, AAfinal, AAqual, final, qual, and TeamFinal

# fit model
linear_model <- fit_full_linear_model(score_variables)
linear_aa_model <- fit_full_linear_model(score_aa_variables)

############# PREDICTION #################
olympic_difficulty <- mean(score_variables$difficulty)
prob_variables <- prob_variables %>% mutate(difficulty = olympic_difficulty)
prob_variables$pred = predict(logit_model, newdata = prob_variables, type='response')
gymnast_prob_predicts <- prob_variables %>% select(Name, Apparatus, Gender, Country, pred) %>% unique()
gymnast_prob_predicts

score_variables <- score_variables %>% mutate(difficulty = olympic_difficulty)
score_variables$pred = predict(linear_model, newdata = score_variables, type='response')
gymnast_score_predicts <- score_variables %>% select(Name, Apparatus, Gender, Country, pred) %>% unique()
gymnast_score_predicts

############### SIMULATION FOR OTHER COUNTRIES #######
# Finding top 12 countries and the five athletes for each team
top_countries_w <- topn_countries(data = score_variables,
                                  gender = 'w',
                                  topn = 12)
top_countries_w
top_countries_m <- topn_countries(data = score_variables,
                                  gender = 'm',
                                  topn = 12)
top_countries_m
# Filtering out people from combo list

# Initial pool from men and women for top 12 countries
mens <- score_variables %>%
  filter(Gender == 'm', Country %in% top_countries_m) %>%
  group_by(Country, Apparatus) %>%
  arrange(desc(pred)) %>%
  slice_head(n = 7)

womens <- score_variables %>%
  filter(Gender == 'w', Country %in% top_countries_w) %>%
  group_by(Country, Apparatus) %>%
  arrange(desc(pred)) %>%
  slice_head(n = 7)

contenders <- function(data, gender, country){
  if (gender == 'w'){
    people <- data %>%
      filter(Country == country, Name %in% womens$Name) %>%
      pivot_wider(names_from = Apparatus, values_from = pred) %>%
      replace(is.na(.), 0)
    return(people)
  }
  if (gender == 'm'){
    people <- data %>%
      filter(Country == country, Name %in% mens$Name) %>%
      pivot_wider(names_from = Apparatus, values_from = pred) %>%
      replace(is.na(.), 0)
    return(people)
  }
}
# Finding contenders for combos
#top12 women

# women gymnasts in top12 countries: example
contenders_w_CHN <- contenders(data = score_variables, gender = 'w', country = "DOM")

# men gymnasts in top12 countries: example
contenders_w_CHN <- contenders(data = score_variables, gender = 'm', country = "CHN")

# five gymnasts that are participants for the 12 selected countries
top5_w <- lapply(top_countries_w$Country, top5, gender = 'w')
top5_w
top5_m <- lapply(top_countries_m$Country, top5, gender = 'm')
top5_m

# Finding the 36 individuals from countries that did not qualify
vaults = c("VT", "VT1", "VT2")
individual_w <- top_individual(data = score_variables, gender = 'w',
                      country_list = top_countries_w, bucket = 4,
                      vaults = vaults)

individual_m <- top_individual(data = score_variables, gender = 'm',
                               country_list = top_countries_m, bucket = 6,
                               vaults = vaults)

individual_w %>% select(Name, Apparatus, Gender, Country, pred)
individual_m %>% select(Name, Apparatus, Gender, Country, pred)


# All unique names check (should be 36)
length(unique(individual_w$Name))
length(unique(individual_m$Name))

# None from top 12 countries check (should be 0)
sum(individual_w$Country %in% top_countries_w$Country)
sum(individual_m$Country %in% top_countries_m$Country)


# Selection process of the 5-person team.
head(score_variables)
head(score_aa_variables)
candidates_w <- candidate_selection(score_variables, score_aa_variables, "w", 2, vaults)
candidates_w

candidates_m <- candidate_selection(score_variables, score_aa_variables, "m", 2, vaults)
candidates_m


########## DATA VISUALIZATION

## Top Counties Competitors:

# Men’s qual: china, Japan, Great Britain
# Women: US, Great Britain, Canada

#### Comparing the probabilities for women and men

### Women

## US women
USA_candidates_w_plot <- candidates_w %>% group_by(Name) %>%
  summarise(prob = mean(prob))

ggplot(USA_candidates_w_plot, aes(x = reorder(Name, -prob), y = prob)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='skyblue',fill='steelblue') +
  xlab("Female contestants") + ylab("probability of winning finals")

## US men
USA_candidates_m_plot <- candidates_m %>% group_by(Name) %>%
  summarise(prob = mean(prob))

ggplot(USA_candidates_m_plot, aes(x = reorder(Name, -prob), y = prob)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='skyblue',fill='steelblue') +
  xlab("Female contestants") + ylab("probability of winning finals")



top_countries_w

ENG_top5_women <- top5("ENG", "w")
CAN_top5_women <- top5("CAN", "w")

ENG_candidates_w_plot <- ENG_top5_women %>% group_by(Name) %>%
  summarise(prob = mean(prob))
ggplot(ENG_candidates_w_plot, aes(x = reorder(Name, -prob), y = prob)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='dodgerblue4',fill='dodgerblue3') +
  xlab("Female contestants") + ylab("probability of winning finals")

CAN_candidates_w_plot <-  CAN_top5_women %>% group_by(Name) %>%
  summarise(prob = mean(prob))
ggplot(CAN_candidates_w_plot, aes(x = reorder(Name, -prob), y = prob)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='burlywood',fill='red4') +
  xlab("Female contestants") + ylab("probability of winning finals")

# Combine all together
USA_candidates_w_plot$country <- "USA"
ENG_candidates_w_plot$country <- "ENG"
CAN_candidates_w_plot$country <- "CAN"

all_women_simulation <- rbind(USA_candidates_w_plot, ENG_candidates_w_plot, CAN_candidates_w_plot)


ggplot(all_women_simulation, aes(x= reorder(Name, -prob), y= prob, fill=country)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity") + xlab("Female contestants") +
  ylab("Probability of winning finals")


## Comments
# It's very clear from the graph that the US has a higher variance in many
# of the players but they overall score higher than the other countries.
# In order from strongest to weakest of the top countries, we have: USA, ENG,
# and CAN. I'm wondering if Zoe Miller may be an outlier since having over 60%
# chance of winning and being higher than everyone else is very unlikely.

### Men

CHN_top5_men <- top5("CHN", "m")
JPN_top5_men <- top5("JPN", "m")
ENG_top5_men <- top5("ENG", "m")

# Plotting the probabilities of top players in competitor country
ENG_candidates_m_plot <- ENG_top5_men %>% group_by(Name) %>%
  summarise(prob = mean(prob))
ggplot(ENG_candidates_m_plot, aes(x = reorder(Name, -prob), y = prob)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='dodgerblue4',fill='dodgerblue3') +
  xlab("Male contestants") + ylab("probability of winning finals")

JPN_candidates_m_plot <- JPN_top5_men %>% group_by(Name) %>%
  summarise(prob = mean(prob))
ggplot(JPN_candidates_m_plot, aes(x = reorder(Name, -prob), y = prob)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='darkred',fill='darkred') +
  xlab("Male contestants") + ylab("probability of winning finals")

CHN_candidates_m_plot <- CHN_top5_men %>% group_by(Name) %>%
  summarise(prob = mean(prob))
ggplot(CHN_candidates_m_plot, aes(x = reorder(Name, -prob), y = prob)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='red2',fill='red2') +
  xlab("Male contestants") + ylab("probability of winning finals")

# Combine all together
USA_candidates_m_plot$country <- "USA"
ENG_candidates_m_plot$country <- "ENG"
JPN_candidates_m_plot$country <- "JPN"
CHN_candidates_m_plot$country <- "CHN"

all_men_simulation <- rbind(USA_candidates_m_plot, ENG_candidates_m_plot,
                            JPN_candidates_m_plot, CHN_candidates_m_plot)


ggplot(all_men_simulation, aes(x= reorder(Name, -prob), y= prob, fill=country)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity") + xlab("Male contestants") +
  ylab("Probability of winning finals")


## Comments
# Interestingly enough, in this graph, the US does not trump all countries-
# although we are working with more countries and players in this simulation
# set. We matched the top 5 players from the other countries alongside a
# potential team of US male players. In this case, China trumps above all with
# Jingyuan Zou having a probability of above 75% in winning a medal. Unlike the
# women's team, China's team is less variable and has a smoother descent in the
# bars. The US is also one of the weakest teams in comparison with the top
# countries here.
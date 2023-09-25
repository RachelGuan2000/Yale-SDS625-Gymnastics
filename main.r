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


########## GGPLOT

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








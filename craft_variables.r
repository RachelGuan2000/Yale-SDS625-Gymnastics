craft_prob_variables <- function(data) {
  library(dplyr)

  # Check if required columns exist
  if(!all(c('Name', 'Score', 'Round', 'Gender', 'Apparatus', 'Rank') %in% colnames(data))){
    stop("Some important columns are not found in data.")
  }

  # craft variables based on each gymnast in each apparatus
  crafted_variables <- data %>%
    group_by(Name, Apparatus, Gender, Country) %>%
    summarise(mean_score = mean(Score, na.rm = TRUE),
              consistency = var(Score, na.rm = TRUE),
              mean_difficulty = mean(D_Score, na.rm = TRUE),
              failure = mean(Penalty, na.rm = TRUE)) %>%
    ungroup()

  # craft response variable indicating if this person got a gold medal in this apparatus
  female_data <- data %>%
    filter(Gender == "w") %>%
    filter(Round == "final") %>%
    mutate(y = if_else(Rank == 1, 1, 0)) %>%
    group_by(Name, Apparatus) %>%
    mutate(y = if_else(sum(y) > 0, 1, 0)) %>%
    ungroup()

  male_data <- data %>%
    filter(Gender == "m") %>%
    filter(Round == "final") %>%
    mutate(y = if_else(Rank == 1, 1, 0)) %>%
    group_by(Name, Apparatus) %>%
    mutate(y = if_else(sum(y) > 0, 1, 0)) %>%
    ungroup()

  data <- rbind(female_data, male_data) %>% select(Name, Gender, Round, Apparatus, Country, y)

  data <- crafted_variables %>%
  left_join(data, by=c('Name', 'Apparatus', 'Gender', 'Country')) %>%
  mutate(consistency = if_else(is.na(consistency), 0, consistency)) %>%
  mutate(Round = if_else(is.na(Round), 'qual', Round)) %>%
  mutate(y = if_else(is.na(y), 0, y))

  return(data)
}


craft_prob_aa_variables <- function(data) {
  library(dplyr)

  # Check if required columns exist
  if(!all(c('Name', 'Score', 'Round', 'Gender', 'Apparatus', 'Rank') %in% colnames(data))){
    stop("Some important columns are not found in data.")
  }

  # craft variables based on each gymnast in each apparatus
  crafted_variables <- data %>%
    filter(Round == "AAfinal") %>%
    group_by(Name, Gender, Country) %>%
    summarise(mean_score = mean(Score, na.rm = TRUE),
              consistency = var(Score, na.rm = TRUE),
              mean_difficulty = mean(D_Score, na.rm = TRUE),
              failure = mean(Penalty, na.rm = TRUE)) %>%
    ungroup()

  # craft response variable indicating if this person got a gold medal in this apparatus
  female_data <- data %>%
    filter(Gender == "w") %>%
    filter(Round == "AAfinal") %>%
    group_by(Name) %>%
    mutate(TScore = sum(Score, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(Competition) %>%
    mutate(y = if_else(TScore == max(TScore), 1, 0)) %>%
    ungroup() %>%
    group_by(Name) %>%
    mutate(y = if_else(sum(y) > 0, 1, 0)) %>%
    ungroup()

  male_data <- data %>%
    filter(Gender == "m") %>%
    filter(Round == "AAfinal") %>%
    group_by(Name) %>%
    mutate(TScore = sum(Score, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(Competition) %>%
    mutate(y = if_else(TScore == max(TScore), 1, 0)) %>%
    ungroup() %>%
    group_by(Name) %>%
    mutate(y = if_else(sum(y) > 0, 1, 0)) %>%
    ungroup()

  data <- rbind(female_data, male_data) %>% select(Name, Gender, Round, Apparatus, Country, y)

  data <- crafted_variables %>%
    left_join(data, by=c('Name', 'Gender', 'Country')) %>%
    mutate(consistency = if_else(is.na(consistency), 0, consistency)) %>%
    mutate(Round = if_else(is.na(Round), 'qual', Round)) %>%
    mutate(y = if_else(is.na(y), 0, y))

  return(data)
}


craft_score_variables <- function(data, round) {
  library(dplyr)

  # Check if required columns exist
  if(!all(c('Name', 'Score', 'Round', 'Gender', 'Apparatus', 'Rank', 'Competition') %in% colnames(data))){
    stop("Some important columns are not found in data.")
  }

  # Craft difficulty
  difficulty_lookup <- data %>%
    group_by(Competition) %>%
    arrange(desc(Score)) %>%
    slice(1:20) %>%
    mutate(difficulty = mean(Score)) %>%
    select(Competition, difficulty) %>%
    distinct(Competition, difficulty)

  # craft variables based on each gymnast in each apparatus
  gymnast_lookup <- data %>%
    group_by(Name, Apparatus, Gender, Country) %>%
    summarise(mean_score = mean(Score, na.rm = TRUE),
              consistency = var(Score, na.rm = TRUE),
              mean_difficulty = mean(D_Score, na.rm = TRUE),
              failure = mean(Penalty, na.rm = TRUE)) %>%
    mutate(consistency = if_else(is.na(consistency), 0, consistency)) %>%
    ungroup()

  data <- data %>%
    filter(Round == round) %>%
    left_join(difficulty_lookup, by = "Competition") %>%
    left_join(gymnast_lookup, by = c('Name', 'Apparatus', 'Gender', 'Country')) %>%
    select(Name, Apparatus, Gender, Country, Competition, difficulty, mean_score, consistency, mean_difficulty, failure, Score)

  return(data)
}
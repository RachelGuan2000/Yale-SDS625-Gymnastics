# function to find the top countries

topn_countries <- function(data, gender, topn) {
  x <- data %>%
    filter(!(Country == "USA"), !(Country == ""), Gender == gender) %>%
    distinct() %>%
    group_by(Country) %>%
    summarise(probs = mean(prob), .groups = 'drop') %>% # Q: unfair.
    arrange(desc(probs)) %>%
    slice(1:topn) %>%
    select(Country)
  return(x)
}


# Function to find top 5 athletes for input countries
top5 <- function(country, gender){
  x <- variables %>%
    filter(Country == country, Gender == gender) %>%
    arrange(desc(prob)) %>%
    distinct(Name, .keep_all = TRUE) %>%
    slice(1:5)

  return (x)
}


top_individual <- function(data, gender, country_list, bucket, vaults) {
  x <-  data %>% 
    filter(!(data$Country %in% country_list$Country),
           !Country == "", Gender == gender, !Apparatus %in% vaults) %>% 
    group_by(Apparatus) %>% 
    arrange(desc(prob)) %>% 
    distinct(Name, .keep_all = TRUE) %>% 
    slice(1:(36/bucket))
  
  y <-  data %>% filter(!data$Name %in% x$Name, 
                        !data$Country %in% country_list$Country,!Country == "", 
                        Gender == gender, Apparatus %in% vaults) %>% 
    arrange(desc(prob)) %>% 
    distinct(Name, .keep_all = TRUE) %>% 
    slice(1:(36/bucket))
  
  z <-  rbind(x,y)
  
  return(z)
}


candidate_selection <- function(data, aa_data, gender, ntop, vaults) {
  selection <- data %>% 
    filter(Country == "USA", Gender == gender, !Apparatus %in% vaults) %>% 
    distinct() %>%
    group_by(Apparatus) %>%
    arrange(desc(prob)) %>%
    slice(1:ntop) %>%
    select(Name, Apparatus, y, prob)
  
  selection_v <- data %>% 
    filter(Country == "USA", Gender == gender, Apparatus %in% vaults) %>% 
    distinct(Name, .keep_all = TRUE) %>% 
    arrange(desc(prob)) %>%
    slice(1:ntop) %>%
    select(Name, Apparatus, y, prob)
  
  selection_aa <- aa_data %>%
    filter(Country == "USA", Gender == gender) %>%
    distinct(Name, .keep_all = TRUE) %>% 
    arrange(desc(prob)) %>%
    slice(1:ntop) %>%
    select(Name, Apparatus, y, prob)
  
  selection_aa$Apparatus <- "AA"
  
  return(rbind(selection, selection_v, selection_aa))
}
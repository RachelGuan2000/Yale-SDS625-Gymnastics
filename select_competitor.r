# function to find the top countries

topn_countries <- function(data, gender, topn) {
  x <- data %>%
    filter(!(Country == "USA"), !(Country == ""), Gender == gender) %>%
    distinct() %>%
    group_by(Country) %>%
    summarise(probs = sum(prob), .groups = 'drop') %>%
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
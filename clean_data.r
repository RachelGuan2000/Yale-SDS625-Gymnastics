# function to clean the olympic data
clean_olympic_data <- function() {
  library(dplyr)
  library(lubridate)
  library(tidyverse)
  library(tools)
  library(stringr)

  x <- read.csv("data/data_2017_2021.csv", as.is = TRUE)
  formats = c("%d/ %b/ %Y", "%d/ %B/ %Y", "%a/ %d/ %b/ %Y")
  x$Date <- parse_date_time(x$Date, formats)
  x <- x %>%
    distinct() %>%
    drop_na(Score) %>%
    mutate(Penalty = ifelse(is.na(Penalty), 0, Penalty)) %>%
    mutate(Name = paste(toTitleCase(tolower(FirstName)),
                        toTitleCase(tolower(LastName)))) %>%
    mutate(Apparatus = toupper(Apparatus)) %>%
    select(Name, Gender, Round, Apparatus, D_Score, E_Score, Penalty, Score, Rank, Country)

  return(x)
}

# function to clean the championship data
clean_championship_data <- function() {
  library(dplyr)
  library(lubridate)
  library(tidyverse)
  library(stringr)


  y <- read.csv("data/data_2022_2023.csv", as.is = TRUE)
  y <- y %>%
    distinct() %>%
    drop_na(Score) %>%
    mutate(Penalty = ifelse(is.na(Penalty), 0, Penalty)) %>%
    mutate(Name = paste(toTitleCase(tolower(FirstName)),
                        toTitleCase(tolower(LastName)))) %>%
    mutate(first_name = word(Name, 1)) %>%
    mutate(last_name = word(Name, -1)) %>%
    mutate(Name = ifelse(first_name == '', last_name,
                         paste(first_name, last_name))) %>%
    mutate(Apparatus = ifelse(Apparatus == "VT_1", "VT1", Apparatus)) %>%
    mutate(Apparatus = ifelse(Apparatus == "VT_2", "VT2", Apparatus)) %>%
    mutate(Apparatus = toupper(Apparatus)) %>%
    select(Name,Gender, Round, Apparatus, D_Score, E_Score,
           Penalty, Score, Rank, Country, Competition)

  # Create a table that contains contry code for each gymnast
  country_lookup <- y %>%
    select(Name, Country) %>%
    filter(!Country == '') %>%
    distinct()

  # Findout the indices where someone's country is currently blank
  rtr <-  which(y$Country == '')
  for (i in rtr) {
    if(y$Name[i] %in% country_lookup$Name) {
      country_i <- (country_lookup$Country[country_lookup$Name == y$Name[i]])
      y$Country[i] <- country_i
    }
  }

  # Gymnast with blank contry code
  no_name <- y %>% filter(Country == '')


  return(y)



}
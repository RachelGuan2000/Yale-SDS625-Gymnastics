# function to clean the olympic data
clean_olympic_data <- function() {
  library(dplyr)
  library(lubridate)
  library(tidyverse)
  library(tools)

  x <- read.csv("data/data_2017_2021.csv", as.is = TRUE)
  formats = c("%d/ %b/ %Y", "%d/ %B/ %Y", "%a/ %d/ %b/ %Y")
  x$Date <- parse_date_time(x$Date, formats)
  x <- x %>%
    distinct() %>%
    drop_na(Score) %>%
    mutate(Penalty = ifelse(is.na(Penalty), 0, Penalty)) %>%
    mutate(Name = paste(FirstName, toupper(LastName))) %>%
    select(Name, Gender, Round, Apparatus, D_Score, E_Score, Penalty, Score, Rank, Country)

  return(x)
}

# function to clean the championship data
clean_championship_data <- function() {
  library(dplyr)
  library(lubridate)
  library(tidyverse)

  y <- read.csv("data/data_2022_2023.csv", as.is = TRUE)
  y <- y %>%
    distinct() %>%
    drop_na(Score) %>%
    mutate(Penalty = ifelse(is.na(Penalty), 0, Penalty)) %>%
    mutate(Name = paste(FirstName, toupper(LastName))) %>%
    mutate(Apparatus = ifelse(Apparatus == "VT_1", "VT1", Apparatus)) %>%
    mutate(Apparatus = ifelse(Apparatus == "VT_2", "VT2", Apparatus)) %>%
    select(Name, Gender, Round, Apparatus, D_Score, E_Score, Penalty, Score, Rank, Country, Competition)

  return(y)
}
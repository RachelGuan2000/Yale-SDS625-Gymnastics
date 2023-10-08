# function to fit logistic model for different apparatus and genders
fit_logistic_model <- function(train) {
  genders = unique(train$Gender)
  apparatus = unique(train$Apparatus)
  for (g in genders) {
    for (a in apparatus) {
      event_train <- train %>%
        filter(Apparatus == a, Gender == g) %>%
        select(Name, Apparatus, Gender, mean_score, consistency, mean_difficulty, failure, y)
      print(g)
      print(a)
      print("Data: \n")
      print(event_train)
      model <- glm(y ~ mean_score + consistency + mean_difficulty + failure, data = event_train, family=binomial())

      print("Model summary: \n")
      print(summary(model))
    }
  }

  return()
}

# function to fit a full logistic regression model
fit_full_logistic_model <- function(train) {
   model <- glm(y ~ mean_score + consistency + mean_difficulty + failure + difficulty, data = train, family=binomial())
   print(summary(model))
   return(model)
}

fit_full_linear_model <- function(train) {
  model <- lm(Score ~ difficulty + mean_score + consistency + failure, data=train)
  print(summary(model))

  return(model)
}
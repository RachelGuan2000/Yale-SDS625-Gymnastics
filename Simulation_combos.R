########################### Creating the for_loop

library(ggplot2)

# Sample dataframe
df <- data.frame(Name = c("Alice", "Bob", "Charlie", "David", "Eve", "Frank", "Grace", "Hank", "Ivy", "Jack"),
                 Score = c(90, 85, 78, 92, 88, 76, 89, 94, 82, 87))

num_players <- 5
all_combinations <- combn(df$Name, num_players, simplify = FALSE)
head(all_combinations)
used_combinations <- list()
result_df <- data.frame(GroupNames = character(0), 
                        TotalScore = numeric(0)) #holder
head(result_df)
# Iterate through all unique combinations




# if(J%%100){cat(j)} --> to count every 100 iterations


for (i in 1:length(all_combinations)) {
  current_combination <- all_combinations[[i]]
  #unused combos
  if (!(identical(current_combination, unlist(used_combinations)))) {
    used_combinations <- append(used_combinations, list(current_combination))
    group_df <- df[df$Name %in% current_combination, ]
    total_score <- sum(group_df$Score)
    print(current_combination)
    #if(i%%100){cat(i)}
    #print(total_score)
    result_df <- rbind(result_df, data.frame(GroupNames = paste(current_combination, collapse = ", "), TotalScore = total_score))
  }
}

# Order the dataframe by the "Score" column in descending order
result_df <- result_df[order(-result_df$TotalScore), ]
nrow(result_df)
head(result_df)

ggplot(data = result_df, mapping = aes(GroupNames, TotalScore)) + geom_point()

# does nrow(result_df) have 756 rows 
# but there's 1000+ items as shown in results_df??


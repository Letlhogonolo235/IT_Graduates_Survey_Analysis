#Create a dataframe
name <- c("Anastasia", "Dima","Katherine","James","Emily","Micheal","Matthew","Laura","Kevin","Jonas")
score <- c(12.5, 9.0, 16.5, 12.0, 9.0, 20.0, 14.5, 13.5, 8.0, 19.0)
attempts <- c(1, 3, 2, 3, 2, 3, 1, 1, 2, 1)
qualify <- c("yes","no","yes","no","no","yes","yes","no","no","yes")

#Create the dataframe
students <- data.frame(name, score, attempts, qualify)

#Print the dataframe
print(students)

#Filter the dataframe
non_qualifiers <- students[students$qualify == "no", ]

#Print the filtered dataframe
print(non_qualifiers)

#Filter for students with only 1 attempt
one_attempt <- students[students$attempts == 1, ]

#Separate quailfied and non-qualified students
qualified_one_attempt <- one_attempt[one_attempt$qualify == "yes", ]
non_qualified_one_attempt <- one_attempt[one_attempt$qualify == "no", ]

#Calculaye average scores
avg_qualified <- mean(qualified_one_attempt$score)
avg_non_qualified <- mean(non_qualified_one_attempt$score)

#Print the results
cat("Average score of qualified students with 1 attempt:", avg_qualified, "\n" )
cat("Average score of non-qualified students with 1 attempt:", avg_non_qualified, "\n")
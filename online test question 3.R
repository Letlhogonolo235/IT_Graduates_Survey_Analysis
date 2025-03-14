#Load the dataset
insurance <- read.csv("C:/insurance.csv")

summary(insurance)

#Calculate mode
getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode_sex <- getmode(insurance$sex)
mode_smoker <- getmode(insurance$smoker)
mode_region <- getmode(insurance$region)

cat("Mode of sex:", mode_sex, "\n")
cat("Mode of smoker:", mode_smoker, "\n")
cat("Mode of region:", mode_region, "\n")

#Create the BMI category column
insurance$BMI_Category <- ifelse(insurance$bmi < 18.5, "Underweight",
                                 ifelse(insurance$bmi >= 18.5 & insurance$bmi <= 24, "Normal",
                                        ifelse(insurance$bmi >= 25 & insurance$bmi <=29, "Overweight",
                                               ifelse(insurance$bmi >=30 & insurance$bmi <= 40, "Obese",
                                                      "Extremely Obese"))))
#Display
head(insurance)

#Histogram of charges

hist(insurance$charges, main = "Distribution of charges", xlab = "Charges", col = "lightblue")


#Histogramof BMI

hist(insurance$bmi, main = "Distribution of BMI", xlab = "BMI", col = "lightpink")



#Boxplots for outlier detection

boxplot(insurance$charges, main = "Boxplot of charges", ylab = "Charges", col = "yellow")

boxplot(insurance$bmi, main = "Boxplot of BMI", ylab = "BMI", col = "green"
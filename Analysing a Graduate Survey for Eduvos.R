#QUESTION 1
#Install the necessary libraries
install.packages("dplyr")
install.packages("tidyr")
install.packages("readr")

#Load the necessary libraries
library(dplyr)
library(tidyr)
library(readr)

#Load the dataset
data <- read_csv("C:/graduate_survey.csv")

#a. Selecting relevant columns
relevant_columns <- c("Campus", "StudyField", "Branch", "Role", "EduLevel", "ProgLang", "Databases", "Platform", "WebFramework", "Industry", "AISearch", "AITool", "Employment")

cleaned_data <- data %>%
  select(all_of(relevant_columns))

#b. Missing value treatment
#Check for missing values
missing_values <- colSums(is.na(cleaned_data))

#Remove all rows with missing values for simplicity
cleaned_data <- cleaned_data %>%
  drop_na()

#c. Standardizing categorical columns
cleaned_data <- cleaned_data %>%
  mutate(Campus = case_when(
    Campus %in% c("Durban", "Umhlanga") ~ "Durban",
    Campus %in% c("Mbombela", "Nelspruit") ~ "Mbombela",
    Campus %in% c("Nelson Mandela Bay", "Port Elizabeth") ~ "Nelson Mandela Bay",
    TRUE ~ Campus
  ))

#d. Subsetting data
#Count responses per campus
campus_counts <- cleaned_data %>%
  count (Campus) %>%
  arrange(desc(n))

#Selecting the top 3-5 campuses
top_campuses <- campus_counts %>%
  top_n(5,n) %>%
  pull(Campus)

#Subset the data
cleaned_data <- cleaned_data %>%
  filter(Campus %in% top_campuses)

#Checking the structure of the cleaned data
str(cleaned_data)
#Summary of the cleaned data
summary(cleaned_data)


#QUESTION 2
#Install the necessary libraries
install.packages("ggplot2")
install.packages("stringr")

#Load the necessary libraries
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)

#Function to process multi-value columns
count_top_tools <- function(column_name){
  cleaned_data %>%
    select(all_of(column_name)) %>%
    separate_rows(!!sym(column_name), sep = ";") %>%
    count(!!sym(column_name), sort = TRUE)
}

#Count tools for each category
top_prog_langs <- count_top_tools("ProgLang")
top_databases <- count_top_tools("Databases")
top_platforms <- count_top_tools("Platform")
top_web_frameworks <- count_top_tools("WebFramework")
top_ai_search <- count_top_tools("AISearch")
top_ai_tools <- count_top_tools("AITool")

#Function to create bar plots
plot_top_tools <- function(data, title) {
  ggplot(data, aes(x = reorder(!!sym(names(data)[1]), n), y = n)) +
    geom_bar(stat = "identity", fill = "blue") +
    coord_flip() +
    labs(title = title, x = "Tools", y = "Count") +
    theme_minimal()
}

#Create plots for each tool category
plot_top_tools(top_prog_langs, "Top Programming Languages used currently by Graduates in the tech space")
plot_top_tools(top_databases, "Top Databases used currently by Graduates in the tech space")
plot_top_tools(top_platforms, "Top Platforms used currently by Graduates in the tech space")
plot_top_tools(top_web_frameworks, "Top Web Frameworks used currently by Graduates in the tech space")
plot_top_tools(top_ai_search, "Top AI Search used currently by Graduates in the tech space")
plot_top_tools(top_ai_tools, "Top AI Developer Tools used currently by Graduates in the tech space")

#Count industries by study field
industry_counts <- cleaned_data %>%
  select(StudyField, Industry) %>%
  separate_rows(Industry, sep = ";") %>%
  count(StudyField, Industry, sort = TRUE)

#Create a plot for industries
ggplot(industry_counts, aes(x = reorder(Industry, n), y = n, fill = StudyField)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Most popular Industries graduates go into from the various Study Fields", x = "Industry", y = "Count") +
  theme_minimal()

#Count job roles
job_role_counts <- cleaned_data %>%
  count(StudyField, Role, sort = TRUE)

#Create a plot for job roles
ggplot(job_role_counts, aes(x = reorder(Role,n), y = n, fill = StudyField)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Top Job Roles graduates go into from the various Study Fields", x = "Job Role", y = "Count") +
  theme_minimal()

#Calculate employment rate
employment_rate <- cleaned_data %>%
  group_by(StudyField) %>%
  summarise(Employed = sum(str_detect(Employment, "Employed")),
            Total = n(),
            EmploymentRate = Employed / Total * 100)

#Create a plot for employment rates
ggplot(employment_rate, aes(x = StudyField, y = EmploymentRate, fill = StudyField)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Employment Rate of graduates from each Study Field", x = "Study Field", y = "Employment Rate (%)") +
  theme_minimal()
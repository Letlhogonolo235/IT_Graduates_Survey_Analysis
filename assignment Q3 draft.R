#Install required packages
install.packages(c("shiny", "tidyverse", "shinythemes"))

#Load the necessary libraries
library(shiny)
library(tidyverse)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

#Load the dataset
data <- read_csv("C:/graduate_survey.csv")

#Creating the UI file (ui.R)
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Eduvos IT Graduates Survey Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("toolType", "Select Tool Type:",
                  choices = c("Programming Languages", "Databases", "Web Frameworks", "AI Tools", "Platforms")),
      selectInput("studyField", "Select Study Field:",
                  choices = unique(data$StudyField)),
      actionButton("update", "Update View")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 h3("Overview of Tools Used"),
                 plotOutput("toolPlot")),
        tabPanel("Industries",
                 h3("Industries Graduates Work In"),
                 plotOutput("industryPlot")),
        tabPanel("Job Roles",
                 h3("Job Roles by Study Field"),
                 plotOutput("rolePlot")),
        tabPanel("Employment Rate",
                 h3("Employment Rate by Study Field"),
                 plotOutput("employmentPlot"))
      )
    )
  )
)

#Creating the Server file (server.R)
server <- function(input, output) {
  
  #Reactive expression to filter data based on user input
  filtered_data <- reactive({
    req(input$update) #To ensure the button is clicked
    cleaned_data %>%
      filter(StudyField == input$studyfield)
  })
  
  #Plot for tools used
  output$toolPlot <- renderPlot({
   tool_counts <- filtered_data() %>%
       select(ProgLang) %>%
       separate_rows(ProgLang, sep = ";") %>%
       count(ProgLang, sort = TRUE) %>%
       top_n(10, n)
       
       ggplot(tool_counts, aes(x = reorder(ProgLang, n), y = n)) +
         geom_bar(stat = "identity", fill = "green") +
         coord_flip() +
         labs(title = "Top Programming Languages Used by Graduates", x = "Programming Language", y = "Count") +
         theme_minimal()
  })
  
  # Plot for industries
  output$industryPlot <- renderPlot({
    industry_counts <- filtered_data() %>%
      separate_rows(Industry, sep = ";") %>%
      count(Industry, sort = TRUE)
    
    ggplot(industry_counts, aes(x = reorder(Industry, n), y = n)) +
      geom_bar(stat = "identity", fill = "red") +
      coord_flip() +
      labs(title = "Industries Graduates Work In", x = "Industry", y = "Count")
  })
  
  # Plot for job roles
  output$rolePlot <- renderPlot({
    role_counts <- filtered_data() %>%
      count(Role, sort = TRUE)
    
    ggplot(role_counts, aes(x = reorder(Role, n), y = n)) +
      geom_bar(stat = "identity", fill = "orange") +
      coord_flip() +
      labs(title = "Top Job Roles by Study Field", x = "Job Role", y = "Count") +
      theme_minimal()
  })
  
  # Plot for employment rate
  output$employmentPlot <- renderPlot({
    employment_rate <- filtered_data() %>%
      group_by(StudyField) %>%
      summarise(Employed = sum(Employment == "Employed"),
                Total = n(),
                EmploymentRate = Employed / Total * 100)
    
    ggplot(employment_rate, aes(x = StudyField, y = EmploymentRate, fill = StudyField)) +
      geom_bar(stat = "identity") +
      labs(title = "Employment Rate by Study Field", x = "Study Field", y = "Employment Rate (%)") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
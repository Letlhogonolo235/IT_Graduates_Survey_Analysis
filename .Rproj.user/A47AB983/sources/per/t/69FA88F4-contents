#Install required packages
install.packages(c("shiny", "tidyverse", "shinythemes"))

#Load necessary libraries
library(shiny)
library(tidyverse)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

#Load the dataset
data <- read_csv("C:/graduate_survey.csv")

#Creating the UI file (ui.R)
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Eduvos IT Graduates Survey Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("toolType", "Select Tool Type:",
                  choices = c("Programming Languages", "Databases", "Platforms", "Web Frameworks", "AI Searches", "AI Tools")),
      selectInput("studyField", "Select Study Field:",
                  choices = unique(data$StudyField)),
      actionButton("update", "Update View")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 h3("Overview of Tools Used"),
                 plotOutput("overviewPlot")),
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
    req(input$update) #To ensure that the button is clicked
    data %>% 
      filter(StudyField == input$studyField)
  })
  
  #Overview plot
  output$overviewPlot <- renderPlot({
    toolType <- input$toolType
    tool_data <- filtered_data() #Ensures that overview plots change according to what study field is selected 
    #Create plots based on selected tool type
    if (toolType == "Programming Languages") {
      # Code to create programming languages plot
      tool_data <- tool_data %>%
        separate_rows(ProgLang, sep = ";") %>%
        count(ProgLang, sort = TRUE) %>%
        top_n(10, n)
      ggplot(tool_data, aes(x = reorder(ProgLang, n), y = n)) +
        geom_bar(stat = "identity", fill = "blue") +
        coord_flip() +
        labs(title = paste("Top Programming Languages Used by", input$studyField), x = "Programming Languages", y = "Count")
    } else if (toolType == "Databases") {
      #Code to create databases plot
      tool_data <- tool_data %>%
        separate_rows(Databases, sep = ";") %>%
        count(Databases, sort = TRUE) %>%
        top_n(10, n)
      ggplot(tool_data, aes(x = reorder(Databases, n), y = n)) +
        geom_bar(stat = "identity", fill = "pink") +
        coord_flip() +
        labs(title = paste("Top Databases Used by", input$studyField), x = "Databases", y = "Count")
    } else if (toolType == "Platforms") {
      #Code to create platforms plot
      tool_data <- tool_data %>%
        separate_rows(Platform, sep = ";") %>%
        count(Platform, sort = TRUE) %>%
        top_n(10, n)
      ggplot(tool_data, aes(x = reorder(Platform, n), y = n)) +
        geom_bar(stat = "identity", fill = "purple") +
        coord_flip() +
        labs(title = paste("Top Platforms Used by", input$studyField), x = "Platforms", y = "Count")
    } else if (toolType == "Web Frameworks") {
      # Code to create web frameworks plot
      tool_data <- tool_data %>%
        separate_rows(WebFramework, sep = ";") %>%
        count(WebFramework, sort = TRUE) %>%
        top_n(10, n)
      ggplot(tool_data, aes(x = reorder(WebFramework, n), y = n)) +
        geom_bar(stat = "identity", fill = "green") +
        coord_flip() +
        labs(title = paste("Top Web Frameworks Used by", input$studyField), x = "Web frameworks", y = "Count")
    } else if (toolType == "AI Searches") {
      #Code to create AI searches plot
      tool_data <- tool_data %>%
        separate_rows(AISearch, sep = ";") %>%
        count(AISearch, sort = TRUE) %>%
        top_n(10, n)
      ggplot(tool_data, aes(x = reorder(AISearch, n), y = n)) +
        geom_bar(stat = "identity", fill = "darkblue") +
        coord_flip() +
        labs(title = paste("Top AI Search Used by", input$studyField), x = "AI Searches", y = "Count")
    } else if (toolType == "AI Tools") {
      # Code to create AI tools plot
      tool_data <- tool_data %>%
        separate_rows(AITool, sep = ";") %>%
        count(AITool, sort = TRUE) %>%
        top_n(10, n)
      ggplot(tool_data, aes(x = reorder(AITool, n), y = n)) +
        geom_bar(stat = "identity", fill = "yellow") +
        coord_flip() +
        labs(title = paste("Top AI developer tools Used by", input$studyField), x = "AI Tools", y = "Count")
    }
  })
  
  #Plot for industries
  output$industryPlot <- renderPlot({
    #Code to create industries  plot
      industry_counts <- filtered_data() %>%
        separate_rows(Industry, sep = ";") %>%
        count(Industry, sort = TRUE)
      
      ggplot(industry_counts, aes(x = reorder(Industry, n), y = n)) +
        geom_bar(stat = "identity", fill = "red") +
        coord_flip() +
        labs(title = paste("Industries for", input$studyField), x = "Industries", y = "Count")
  })
  
  #Plot for job roles
  output$rolePlot <- renderPlot({
    #Code to create job roles plot
    role_counts <- filtered_data() %>%
      count(Role, sort = TRUE)
    
    ggplot(role_counts, aes(x = reorder(Role, n), y = n)) +
      geom_bar(stat = "identity", fill = "orange") +
      coord_flip() +
      labs(title = paste("Top Job Roles for", input$studyField), x = "Job Roles", y = "Count") +
      theme_minimal()
  })
  
  #Plot for employment rate
  output$employmentPlot <- renderPlot({
    #Code to create employment rate plot
    employment_rate <- filtered_data() %>%
      filter(!is.na(Employment)) %>%
      group_by(StudyField) %>%
      summarise(Employed = sum(Employment == "Employed", na.rm = TRUE),
                Total = n(),
                EmploymentRate = Employed / Total * 100) %>%
      drop_na(EmploymentRate)
    
    ggplot(employment_rate, aes(x = StudyField, y = EmploymentRate, fill = StudyField)) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      labs(title = paste("Employment Rate for", input$studyField), x = "Study Field", y = "Employment Rate (%)") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
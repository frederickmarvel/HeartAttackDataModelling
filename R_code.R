install.packages("ggplot")
install.packages("plotly")
install.packages("tidyverse")
install.packages("ggcorrplot")

library(shiny)
library(plotly)
library(tidyverse)
library(ggcorrplot)

data <- data %>% rename(
  Age = age,
  Sex = sex,
  ChestPainType = cp,
  RestingBP = trtbps,
  Cholesterol = chol,
  FastingBS = fbs,
  RestECG = restecg,
  MaxHR = thalachh,
  ExerciseAngina = exng,
  STDepression = oldpeak,
  STSlope = slp,
  NumMajorVessels = caa,
  ThalassemiaType = thall,
  HeartDisease = output
)

ui <- navbarPage(
  "Heart Disease Data Visualization",
  tags$head(tags$style(HTML("
    .custom-tab {
      background-color: #f8f9fa;
      color: #343a40;
      font-weight: bold;z
    }
    .custom-tab:hover {
      background-color: #e9ecef;
      color: #495057;
    }
    .nav-pills .nav-link.active {
      background-color: #007bff;
      color: #ffffff;
    }
  "))),
  tabPanel("Introduction",
           mainPanel(
             h1("Welcome"),
             h2("Heart Disease Data Visualization App"),
             p("This app allows you to explore different visualizations of a heart disease dataset."),
             p("Use the navigation bar to select which plot you would like to view.")
           )),
  tabPanel("Scatter Plot",
           sidebarLayout(
             sidebarPanel(
               sliderInput("age_slider", "Age range:",
                           min = min(data$Age), max = max(data$Age),
                           value = c(min(data$Age), max(data$Age)),
                           step = 1)
             ),
             mainPanel(
               plotlyOutput("scatterplot")
             )
           )),
  tabPanel("Bar Plot",
           sidebarLayout(
             sidebarPanel(
               sliderInput("age_slider2", "Age range:",
                           min = min(data$Age), max = max(data$Age),
                           value = c(min(data$Age), max(data$Age)),
                           step = 1)
             ),
             mainPanel(
               plotlyOutput("barplot")
             )
           )),
  tabPanel("Correlation Heatmap",
           mainPanel(
             plotOutput("heatmap")
           ))
)

# Define the server logic
server <- function(input, output) {
  output$scatterplot <- renderPlotly({
    scatter <- plot_ly(data, x = ~Age, y = ~MaxHR, type = 'scatter', mode = 'markers',
                       marker = list(size = 10, color = ~HeartDisease, colorscale = 'Viridis',
                                     showscale = TRUE), text = ~rownames(data), hovertemplate =
                         paste(
                           "Age: %{x}<br>",
                           "Max Heart Rate: %{y}<br>",
                           "Heart Disease: %{marker.color}<br>",
                           "<extra></extra>"
                         ))
    
    layout <- list(
      title = "Age vs. Maximum Heart Rate",
      xaxis = list(title = "Age"),
      yaxis = list(title = "Maximum Heart Rate"),
      showlegend = FALSE
    )
    
    fig <- scatter %>% layout(layout)
    fig
  })
  
  output$barplot <- renderPlotly({
    filtered_data <- data %>% filter(Age >= input$age_slider2[1], Age <= input$age_slider2[2])
    
    gender_count <- filtered_data %>%
      group_by(Sex, HeartDisease) %>%
      summarise(count = n()) %>%
      ungroup()
    gender_count$Sex <- recode(gender_count$Sex, `0` = "Female", `1` = "Male")
    gender_count$HeartDisease <- recode(gender_count$HeartDisease, `0` = "No", `1` = "Yes")
    
    fig <- plot_ly(gender_count, x = ~Sex, y = ~count, type = 'bar', split = ~HeartDisease,
                   text = ~count, textposition = "auto", hoverinfo = "text", colors = c("#1f77b4", "#ff7f0e")) %>%
      layout(title = "Count of Heart Disease Patients by Gender",
             xaxis = list(title = "Gender"),
             yaxis = list(title = "Count"),
             barmode = 'group')
    
    fig
  })
  
  output$heatmap <- renderPlot({
    corr <- cor(data)
    fig <- ggcorrplot(corr, method = "square", lab = TRUE, lab_size = 3,
                      title = "Correlation Heatmap", colors = c("#132C33", "white", "#D35151"))
    fig
  })
}
shinyApp(ui, server)


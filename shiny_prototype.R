library(shiny)
library(datasets)
library(cowplot)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)



Final_DF_2010 <- read.csv("df_2010.csv") 
Final_DF_2011 <- read.csv("df_2011.csv") 
str(Final_DF_2010)



ui <- navbarPage("Air Quality and Health",
                 tabPanel("Home",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Introduction"),
                              p("This is the introduction to my project."),
                              #img(src = "my_image.png", height = 200, width = 200)
                            ),
                            mainPanel(
                              
                            )
                          )
                 ),
                 tabPanel("State vs. Air pollution",
                          sidebarLayout(
                            sidebarPanel(
                              h3("State vs. Air pollution"),
                              h4("Expectations:"),
                              p("We wanted to integrate data on different air quality in each state and different chronic diseases in each state to determine the relationship between air quality and human health."),
                              p("In this step we'll start by showing you different air quality data for each state."),
                              h4("Analysis:"),
                              p("We found that there was no strong correlation between air pollution in each state, but it was clear from the graph that the air pollution level in California was far higher than that in other states, exceeding 1,000 and approaching 1,500. The second highest was Virginia, which surpassed 725. There are four states with more than 500. The rest are relatively evenly distributed.")
                            ),
                            mainPanel(
                              div(style = "overflow-x: scroll;",
                                  plotOutput("barplot", width = "1500px")
                              ),
                              selectInput("data_year", "Select data year:", choices = c("2010", "2011")),
                              selectInput("state_name", "Select state:", choices = unique(Final_DF_2010$StateName)),
                              uiOutput("state_value")
                            )
                          )
                 )
                 ,
                 tabPanel("State vs. Chronic disease",
                          sidebarLayout(
                            sidebarPanel(
                              h3("State vs. Chronic disease"),
                              h4("Expectations:"),
                              p("We wanted to integrate data on different air quality in each state and different chronic diseases in each state to determine the relationship between air quality and human health."),
                              p("In this step we will show the relationship between state and chronic disease by analyzing the data between the two."),
                              h4("Analysis:"),
                              p("The bar chart shows that there is no clear relationship between states and rate of chronic disease. From the data chart, we can find that Texas and Kentucky are the highest, nearly 0.2. In all, 14 states exceeded 0.1. Others are relatively evenly distributed below 0.1.")
                            ),
                            mainPanel(
                              div(style = "overflow-x: scroll;",
                                  plotOutput("barplot2", width = "1500px")
                              ),
                              selectInput("data_year2", "Select data year:", choices = c("2010", "2011")),
                              selectInput("state_name2", "Select state:", choices = unique(Final_DF_2010$StateName)),
                              uiOutput("state_value2")
                            )
                          )
                 ),
                 tabPanel("Air pollution vs. Chronic disease",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Air pollution vs. Chronic disease"),
                              h4("Expectations:"),
                              p("We wanted to integrate data on different air quality in each state and different chronic diseases in each state to determine the relationship between air quality and human health."),
                              p("In this step, we will integrate the data from the previous two steps to determine the relationship between air pollution and human health."),
                              h4("Analysis:")
                            ),
                            mainPanel(
                              div(style = "overflow-x: scroll;",
                                  plotOutput("barplot3", width = "1500px")
                              ),
                              selectInput("data_year3", "Select data year:", choices = c("2010", "2011")),
                              selectInput("state_name3", "Select state:", choices = unique(Final_DF_2010$StateName)),
                              plotOutput("state_plot")
                            )
                          )
                 ),
                 tabPanel("Summary")
)

server <- function(input, output, session) {
  
  
  selected_data <- reactive({
    if (input$data_year == "2010") {
      Final_DF_2010
    } else {
      Final_DF_2011
    }
  })
  
  output$barplot <- renderPlot({
    ggplot(data = selected_data(), aes(x = StateName, y = average.Measured.Air.conditional.Value.in.State)) +
      geom_bar(stat = "identity") +
      xlab("State Name") +
      ylab("Average Measured Air Conditional Value") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }, width = 1500)
  
  output$state_value <- renderUI({
    selected_state <- input$state_name
    state_value_2010 <- Final_DF_2010[Final_DF_2010$StateName == selected_state, "average.Measured.Air.conditional.Value.in.State"][1]
    state_value_2011 <- Final_DF_2011[Final_DF_2011$StateName == selected_state, "average.Measured.Air.conditional.Value.in.State"][1]
    
    tagList(
      p(selected_state),
      p("Average Measured Air Conditional Value in 2010:", state_value_2010),
      p("Average Measured Air Conditional Value in 2011:", state_value_2011)
    )
  })
  
  selected_data2 <- reactive({
    if (input$data_year2 == "2010") {
      Final_DF_2010
    } else {
      Final_DF_2011
    }
  })
  
  output$barplot2 <- renderPlot({
    ggplot(data = selected_data2(), aes(x = StateName, y = Ratio.Total.chronic.disease.value.per.census)) +
      geom_bar(stat = "identity") +
      xlab("State Name") +
      ylab("Ratio Total Chronic Disease Value per Census") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }, width = 1500)
  
  output$state_value2 <- renderUI({
    selected_state <- input$state_name2
    state_value_2010 <- Final_DF_2010[Final_DF_2010$StateName == selected_state, "Ratio.Total.chronic.disease.value.per.census"][1]
    state_value_2011 <- Final_DF_2011[Final_DF_2011$StateName == selected_state, "Ratio.Total.chronic.disease.value.per.census"][1]
    
    tagList(
      p(selected_state),
      p("Ratio Total Chronic Disease Value per Census in 2010:", state_value_2010),
      p("Ratio Total Chronic Disease Value per Census in 2011:", state_value_2011)
    )
  })
  
  selected_data3 <- reactive({
    if (input$data_year3 == "2010") {
      Final_DF_2010
    } else {
      Final_DF_2011
    }
  })
  
  output$barplot3 <- renderPlot({
    air_quality_data <- selected_data3()[, c("StateName", "average.Measured.Air.conditional.Value.in.State")]
    chronic_disease_data <- selected_data3()[, c("StateName", "Ratio.Total.chronic.disease.value.per.census")]
    
    chronic_disease_data$Ratio.Total.chronic.disease.value.per.census <- chronic_disease_data$Ratio.Total.chronic.disease.value.per.census * 10000
    
    air_quality_data_long <- air_quality_data %>%
      pivot_longer(cols = -StateName, names_to = "variable", values_to = "value")
    
    chronic_disease_data_long <- chronic_disease_data %>%
      pivot_longer(cols = -StateName, names_to = "variable", values_to = "value")
    
    combined_data <- bind_rows(air_quality_data_long, chronic_disease_data_long)
    
    ggplot(data = combined_data, aes(x = StateName, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("State Name") +
      ylab("Value") +
      scale_fill_manual(values = c("blue", "red")) +
      scale_y_continuous(sec.axis = sec_axis(~./10000, name = "Ratio Total Chronic Disease Value per Census (scaled)")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }, width = 1500)
  
  output$state_plot <- renderPlot({
    selected_state <- input$state_name3
    
    air_quality_data_2010 <- Final_DF_2010[Final_DF_2010$StateName == selected_state, c("StateName", "average.Measured.Air.conditional.Value.in.State")]
    chronic_disease_data_2010 <- Final_DF_2010[Final_DF_2010$StateName == selected_state, c("StateName", "Ratio.Total.chronic.disease.value.per.census")]
    
    air_quality_data_2011 <- Final_DF_2011[Final_DF_2011$StateName == selected_state, c("StateName", "average.Measured.Air.conditional.Value.in.State")]
    chronic_disease_data_2011 <- Final_DF_2011[Final_DF_2011$StateName == selected_state, c("StateName", "Ratio.Total.chronic.disease.value.per.census")]
    
    chronic_disease_data_2010$Ratio.Total.chronic.disease.value.per.census <- chronic_disease_data_2010$Ratio.Total.chronic.disease.value.per.census * 10000
    chronic_disease_data_2011$Ratio.Total.chronic.disease.value.per.census <- chronic_disease_data_2011$Ratio.Total.chronic.disease.value.per.census * 10000
    
    air_quality_data_long_2010 <- air_quality_data_2010 %>%
      pivot_longer(cols = -StateName, names_to = "variable", values_to = "value")
    
    chronic_disease_data_long_2010 <- chronic_disease_data_2010 %>%
      pivot_longer(cols = -StateName, names_to = "variable", values_to = "value")
    
    combined_data_2010 <- bind_rows(air_quality_data_long_2010, chronic_disease_data_long_2010)
    
    air_quality_data_long_2011 <- air_quality_data_2011 %>%
      pivot_longer(cols = -StateName, names_to = "variable", values_to = "value")
    
    chronic_disease_data_long_2011 <- chronic_disease_data_2011 %>%
      pivot_longer(cols = -StateName, names_to = "variable", values_to = "value")
    
    combined_data_2011 <- bind_rows(air_quality_data_long_2011, chronic_disease_data_long_2011)
    
    plot_2010 <- ggplot(data = combined_data_2010, aes(x = StateName, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("State Name") +
      ylab("Value") +
      scale_fill_manual(values = c("blue", "red")) +
      scale_y_continuous(sec.axis = sec_axis(~./10000, name = "Ratio Total Chronic Disease Value per Census (scaled)"))
    
    plot_2011 <- ggplot(data = combined_data_2011, aes(x = StateName, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("State Name") +
      ylab("Value") +
      scale_fill_manual(values = c("blue", "red")) +
      scale_y_continuous(sec.axis = sec_axis(~./10000, name = "Ratio Total Chronic Disease Value per Census (scaled)"))
    
    cowplot::plot_grid(plot_2010, plot_2011, ncol = 1)
  })
  
  
}

shinyApp(ui = ui, server = server)

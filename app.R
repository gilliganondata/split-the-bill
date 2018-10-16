# Split the Bill Shiny App
# This is an app for calculating the contribution by each person at a dinner
# that accounts for the wage disparity in the country in which they're dining.

library(shiny)
library(googlesheets)
library(tidyverse)

# Read in the values for the different countries and the wage gaps from Google Sheets. This sheet is viewable at
# https://docs.google.com/spreadsheets/d/1Y5USfz5WlqK27dfw6bv4g1ZZhRkyA5NXgM1yXbPhuOU/edit?usp=sharing.
# It's a pretty simple structure, so easy enough to duplicate should you fork this and want to 
# make updates.
gs_auth()
wages_key <- gs_key("1Y5USfz5WlqK27dfw6bv4g1ZZhRkyA5NXgM1yXbPhuOU")
wages_data <- gs_read(wages_key)

# Rename the columns for simpler use later. The "pre_word" is whether "the"
# should comes before the country name (e.g. "...the disparity in Australia..." vs.
# "...the disparity in *the* United States...")
names(wages_data) <- c("pre_word", "country", "percent_diff", "data_source", "url")

# Convert the percentages to numerics
wages_data$percent_diff <- 
  as.numeric(sub("%", "", wages_data$percent_diff))/100

# Create the countries list for the dropdown
countries_list <- wages_data$country

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Split the Bill (Gender) Equally!"),
  
  tags$em("A gender pay gap persists. To promote awareness of that fact, as well as to provide at least ",
          "a token acknowledgment of the problem, why not split the bill for your meal in a way where the women ",
          "at the table get a break on their meal cost that is proportional to their country-wide depressed income?"),
  
  tags$hr(),
  
  selectizeInput("country",
                 "Choose Your Country:",
                 choices = countries_list),
  numericInput("total_bill",
               "Total Bill Amount:",
               100),
  numericInput("women",
               "# of Women:",
               2),
  numericInput("men",
               "# of Men:",
               2),
  
  # Output the results
  
  tags$h3(textOutput("disparity_message")),
  tags$em(textOutput("data_source")),
  tags$h3(textOutput("women_result")),
  tags$h3(textOutput("men_result")),
  tags$strong(textOutput("adjustment"))
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactive function to get the wage disparity percent
  get_wage_disparity <- reactive({
    wage_disparity_result <- wages_data %>% 
      filter(country == input$country) %>% 
      select(percent_diff) %>% 
      as.numeric()
  })
  
  # Reactive function to calculate what the men should pay. This will be used to return what the men 
  # pay as well as to calculate what the women will pay
  get_men_amount <- reactive({
    
    disparity <- get_wage_disparity()  # Get the wage disparity
    
    # Calculate the amount the men owe
    men_contribute <- input$total_bill /
      (input$men + input$women - input$women * disparity)
  })
  
  # Reactive function to calculate what the women should pay.
  get_women_amount <- reactive({
    
    men_amount <- get_men_amount()
    disparity <- get_wage_disparity()
    
    # Calculate the amount the man/men should pay
    women_amount <- men_amount - men_amount * disparity
    
  })
  
  
  output$disparity_message <- renderText({
    
    # Change the wage disparity to a percentage
    wage_disparity_selected <- get_wage_disparity() * 100
    
    # Get the pre-word
    pre_word <- wages_data %>% 
      filter(country == input$country) %>% 
      select(pre_word) %>% 
      as.character()
    
    # Add a space after the word if there is a word
    pre_word <- ifelse(is.na(pre_word), "",
                       paste0(pre_word, " "))
    
    # Create the output message
    paste0("In ", pre_word, input$country, ", women are paid ", wage_disparity_selected,"% less than men.")
  })
  
  output$data_source <- renderText({
    
    # Get the data source info
    data_source_selected <- wages_data %>% 
      filter(country == input$country) %>% 
      select(data_source) %>% 
      as.character()
    
    # Return the data source message
    paste("Data Source:", data_source_selected)
  })
  
  output$men_result <- renderText({
    
    # Get the amount the man/men should pay
    men_amount <- get_men_amount()
    
    # Figure out if it's "man" or "men" and whether there should be an "each" at the end
    man_men <- ifelse(input$men == 1, "man", "men")
    sentence_end <- ifelse(input$men == 1, ".", " each.")
    
    # Concoct the string
    paste0("The ", input$men, " ", man_men, " should pay ", round(men_amount, 2), sentence_end)
  })
  
  output$women_result <- renderText({
    
    # Get the women amount
    women_amount <- get_women_amount()
    
    # Figure out if it's "woman" or "women"
    woman_women <- ifelse(input$women == 1, "woman", "women")
    sentence_end <- ifelse(input$women == 1, ".", " each.")
    
    # Concoct the string.
    paste0("The ", input$women, " ", woman_women, " should pay ", round(women_amount,2), sentence_end)
    
  })
  
  # Because of rounding, someone may need to throw in an extra coin here or there,
  # but we'll just leave this up to the diners
  output$adjustment <- renderText({
    
    # Calculate the total tallied up
    men_amount <- get_men_amount()
    women_amount <- get_women_amount()
    
    total_calculated <- round(men_amount, 2) * input$men + round(women_amount, 2) * input$women
    
    if(total_calculated == input$total_bill){
      message <- ""
    } else {
      if(total_calculated > input$total_bill){
        delta <- total_calculated - input$total_bill %>% round(2)
        paste0("These numbers will result in a total that is ", round(delta, 2), 
               " more than the total bill, so adjust accordingly.")
      } else {
        delta <- input$total_bill - total_calculated %>% round(2)
        paste0("These numbers will result in a total that is ", round(delta, 2), 
               " LESS than the total bill, so adjust accordingly.")
      }
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


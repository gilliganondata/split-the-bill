# Split the Bill Shiny App
# This is an app for calculating the contribution by each person at a dinner
# that accounts for the wage disparity in the country in which they're dining.

library(shiny)
library(shinythemes)
library(googlesheets4)
library(tidyverse)

# Just reading from a public Google Sheet
gs4_deauth()

# Read in the values for the different countries and the wage gaps from Google Sheets. This sheet is viewable at
# https://docs.google.com/spreadsheets/d/1Y5USfz5WlqK27dfw6bv4g1ZZhRkyA5NXgM1yXbPhuOU/edit?usp=sharing.
# It's a pretty simple structure, so easy enough to duplicate should you fork this and want to 
# make updates.
wages_key <- "1Y5USfz5WlqK27dfw6bv4g1ZZhRkyA5NXgM1yXbPhuOU"

wages_data <- read_sheet(wages_key)

# Rename the columns for simpler use later. The "pre_word" is whether "the"
# should comes before the country name (e.g. "...the disparity in Australia..." vs.
# "...the disparity in *the* United States...")
names(wages_data) <- c("pre_word", "country", "year", "percent_diff", "data_source", "url")

# Convert the percentages to numerics
wages_data$percent_diff <- 
  as.numeric(sub("%", "", wages_data$percent_diff))

# Create the countries list for the dropdown
countries_list <- wages_data$country

# Define UI for application that draws a histogram
# ui <- fluidPage(
#                 
#                 tags$head(
#                   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
#                 ),

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
  
  # Application title
  titlePanel("Split the Bill (Gender) Equally!"),
  
  # Add in Google Tag Manager (if you're using this code, you should
  # swap out the GTM-WQR7N8 value in this for your own GTM container ID. Or just
  # delete the whole tags$head section)
  tags$head(tags$script("(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
            new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
            j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
            'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
            })(window,document,'script','dataLayer','GTM-WQR7N8');")),
  
  tags$em("A gender pay gap persists. To promote awareness of that fact, as well as to provide at least ",
          "a token acknowledgment of the problem, why not split the bill for your meal in a way where the women ",
          "at the table get a break on their meal cost that is proportional to their country-wide depressed income?"),
  
  tags$hr(),
  
  selectizeInput("country",
                 "Choose Your Country:",
                 choices = countries_list,
                 selected = "United States of America"),
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
  
  tags$hr(),
  tags$h3(textOutput("disparity_message")),
  tags$em(textOutput("data_source")),
  tags$h3(textOutput("women_result")),
  tags$h3(textOutput("men_result")),
  tags$strong(textOutput("adjustment")),
  tags$hr(),
  
  # Contact details and such
  tags$div(tags$em("This calculator was created by", tags$a(href="https://twitter.com/tgwilson", "Tim Wilson"),
                   "as the result of a",
          tags$a(href="https://twitter.com/MoeMKiss/status/1041574088488083456",
                 "tweet by @MoeMKiss."))),
  tags$div(tags$em("This calculator was built using R. The source code for is available",
          tags$a(href="https://github.com/gilliganondata/split-the-bill", "on GitHub."),
          "Any feedback can be posted as issues there.")),
  tags$br()
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
    
    # Calculate the amount the men owe. We're NOT going to round this
    # to 2 decimal places here, as we want the raw number to calculate
    # the women's amount.
    men_contribute <- input$total_bill /
      (input$men + input$women - input$women * disparity)
  })
  
  # Reactive function to calculate what the women should pay.
  get_women_amount <- reactive({
    
    men_amount <- get_men_amount()
    disparity <- get_wage_disparity()
    
    # Calculate the amount the man/men should pay. Go ahead and
    # round this to two decimal places.
    women_amount <- (men_amount - men_amount * disparity) %>% 
      round(2)
    
  })
  
  output$disparity_message <- renderText({
    
    # Change the wage disparity to a percentage
    wage_disparity_selected <- get_wage_disparity() * 100
    
    # Get the pre-word
    pre_word <- wages_data %>% 
      filter(country == input$country) %>% 
      select(pre_word) %>% 
      as.character()
    
    # Get the year
    year <- wages_data %>% 
      filter(country == input$country) %>% 
      select(year) %>% 
      as.character()
    
    # Add a space after the word if there is a word
    pre_word <- ifelse(is.na(pre_word), "",
                       paste0(pre_word, " "))
    
    # Create the output message
    paste0("As of ", year, " in ", pre_word, input$country, ", women are paid ", round(wage_disparity_selected, 1),"% less than men.")
  })
  
  output$data_source <- renderText({
    
    # Get the data source info
    data_source_selected <- wages_data %>% 
      filter(country == input$country) 
    
    name <- data_source_selected %>% 
      select(data_source) %>% 
      as.character()
    
    url <- data_source_selected %>% 
      select(url) %>% 
      as.character()
    
    paste("Data Source:", name, "--", url)
    
  })
  
  output$data_source_url <- renderText({
    
    # Get the data source info
    data_source_url_selected <- wages_data %>% 
      filter(country == input$country) %>% 
      select(url) %>% 
      as.character()
  
  })
  
  
  output$men_result <- renderText({
    
    # Get the amount the man/men should pay
    men_amount <- get_men_amount() %>% round(2)
    
    # Figure out if it's "man" or "men" and whether there should be an "each" at the end
    man_men <- ifelse(input$men == 1, "man", "men")
    sentence_end <- ifelse(input$men == 1, ".", " each.")
    
    # Concoct the string
    paste0("The ", input$men, " ", man_men, " should pay ", format(men_amount, nsmall = 2), sentence_end)
  })
  
  output$women_result <- renderText({
    
    # Get the women amount
    women_amount <- get_women_amount()
    
    # Figure out if it's "woman" or "women"
    woman_women <- ifelse(input$women == 1, "woman", "women")
    sentence_end <- ifelse(input$women == 1, ".", " each.")
    
    # Concoct the string.
    paste0("The ", input$women, " ", woman_women, " should pay ", format(women_amount, nsmall = 2), sentence_end)
    
  })
  
  # Because of rounding, someone may need to throw in an extra coin here or there,
  # but we'll just leave this up to the diners
  output$adjustment <- renderText({
    
    # Calculate the total tallied up. The men's amount needs to be rounded
    # to two decimal places here. The women amount already has this done in the function.
    men_amount <- get_men_amount() %>% round(2)
    women_amount <- get_women_amount()
    
    total_calculated <- men_amount * input$men + women_amount * input$women
    
    if(total_calculated == input$total_bill){
      message <- ""
    } else {
      if(total_calculated > input$total_bill){
        delta <- (total_calculated - input$total_bill) %>% round(2)
        paste0("These numbers will result in a total that is ", format(delta, nsmall = 2), 
               " more than the total bill, so adjust accordingly.")
      } else {
        delta <- (input$total_bill - total_calculated) %>% round(2)
        paste0("These numbers will result in a total that is ", format(delta, nsmall = 2), 
               " LESS than the total bill, so adjust accordingly.")
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


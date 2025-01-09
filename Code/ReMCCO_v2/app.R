# ################################################
# Author: Remco A. Scheepmaker, Ph.D. 
# Date: September 11, 2024
#
# Script:   app.R
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above or by visiting
# https://remcoscheepmaker.shinyapps.io/ReMCCO/  
#
# Dependencies: 
# shiny    
# bslib        - for using a layout with cards
# waterfalls   - for waterfall portfolio plots
# ggplot2      - for improving the waterfall plots
# stringr      - to reformat category names  
# ################################################

# Clear workspace
rm(list = ls(all = TRUE))

# setwd("~/Documents/UCF/Summer24/Project/Code")

library(shiny)
library(bslib)
library(waterfalls)
library(ggplot2)
library(stringr) 

# Read the budget selection and card portfolio selection functions
source("CCPortfolioFunctions.R")

# Read the data
cards_data <- read.csv(file = "CreditCards.csv", header = TRUE)
budget_data <- read.csv(file = "BudgetIncome.csv", header = TRUE)

# List of all banks to be used for filtering
banks <- c("Amex", "BoA", "CapOne", "Chase", "Citi", "US Bank", "Wells Fargo")

# Define UI for the application that draws a waterfall plot of the portfolio and 
# prints card assignments.
ui <- page_fillable(

    # # Application title
    # title = "ReMCCO - Recommend Me Credit Cards Optimally",

    # Sidebar with a slider input for number of cards 
    layout_columns(
        #card(card_header("Settings"),
             accordion(
               accordion_panel(
                 "Preferences",
                    sliderInput(inputId = "K",
                    "Number of Cards:", 
                    min = 1, 
                    max = 9, 
                    value = 4),
            
                    sliderInput(inputId = "eta",
                    "Use of Transfer Partners [%]:",
                    min = 0,
                    max = 100,
                    value = 50),

                    sliderInput(inputId = "theta",
                    "Use of Benefits [%]:",
                    min = 0,
                    max = 100,
                    value = 50),
               ),
             
               # Filter for Banks
               accordion_panel(
                 "Banks",
                 checkboxGroupInput(inputId = "banks", label = "Consider cards from:",
                             choices = banks, selected = banks)
            ),
            
            # Point values, both minimum (basic cash back) and maximum (transfer partners) 
            accordion_panel(
              "Travel point/mile value (cpp)",
              numericInput(inputId = "travel_value_amex", label = "Amex",
                                 value = 2.0, min = 1.0),
              
              numericInput(inputId = "travel_value_boa", label = "BoA",
                           value = 1.0, min = 1.0),
              
              numericInput(inputId = "travel_value_capone", label = "CapOne",
                           value = 1.7, min = 1.0),
              
              numericInput(inputId = "travel_value_chase", label = "Chase",
                           value = 2.6, min = 1.0),
              
              numericInput(inputId = "travel_value_citi", label = "Citi",
                           value = 1.5, min = 1.0),
              
              numericInput(inputId = "travel_value_usbank", label = "US Bank",
                           value = 1.0, min = 1.0),
              
              numericInput(inputId = "travel_value_wellsfargo", label = "Wells Fargo",
                           value = 1.5, min = 1.0)
            )
            
        ),
        
      card(card_header("Budget"),
            sliderInput(inputId = "income",
                        "Average Budget Annual Income [$]:",
                        min = 15000,
                        max = 350000,
                        value = 94000),
            
            uiOutput("budget_everything_else"),
            uiOutput("budget_groceries"),
            uiOutput("budget_dining"),
            uiOutput("budget_gas"),
            uiOutput("budget_utility"),
            uiOutput("budget_home_improvement"),
            uiOutput("budget_online_shopping"),
            uiOutput("budget_drug_store"),
            uiOutput("budget_travel_other"),
            uiOutput("budget_phone"),
            uiOutput("budget_streaming"),
            uiOutput("budget_department_store"),
            uiOutput("budget_entertainment"),
            uiOutput("budget_cable_internet"),
            uiOutput("budget_hotel_portal"),
            uiOutput("budget_airline_portal"),
            uiOutput("budget_car_portal"),
            uiOutput("budget_office_supplies")),
       
        layout_columns(
          card(card_header("Recommended Portfolio"),
           plotOutput("waterfallPlot", height = "800px"),
           textOutput("text1")),
          card(card_header("Card Assignments"),
           tableOutput("table")),
        
        col_widths = c(12, 12),
        row_heights = c(2, 1)
    ),
    col_widths = c(3,3,6)
    )
)

server <- function(input, output) {

    # Update travel point values
    for (b in 1:length(banks)) {
      cbank <- banks[b]
      #("Amex", "BoA", "CapOne", "Chase", "Citi", "US Bank", "Wells Fargo")
      if (cbank == "Amex") { 
        cards_data[(cards_data$bank == cbank & cards_data$cash_only == FALSE), 
                   travel_value] <- input$travel_value_amex
      } else if (cbank == "BoA") { 
        cards_data[(cards_data$bank == cbank & cards_data$cash_only == FALSE), 
                   travel_value] <- input$travel_value_boa
      } else if (cbank == "CapOne") { 
        cards_data[(cards_data$bank == cbank & cards_data$cash_only == FALSE), 
                   travel_value] <- input$travel_value_capone
      } else if (cbank == "Chase") { 
        cards_data[(cards_data$bank == cbank & cards_data$cash_only == FALSE), 
                   travel_value] <- input$travel_value_chase
      } else if (cbank == "Citi") { 
        cards_data[(cards_data$bank == cbank & cards_data$cash_only == FALSE), 
                   travel_value] <- input$travel_value_citi
      } else if (cbank == "US Bank") { 
        cards_data[(cards_data$bank == cbank & cards_data$cash_only == FALSE), 
                   travel_value] <- input$travel_value_usbank
      } else if (cbank == "Wells Fargo") { 
        cards_data[(cards_data$bank == cbank & cards_data$cash_only == FALSE), 
                   travel_value] <- input$travel_value_wellsfargo
      }
    }
    
  
    budget <- reactive({
      #get_budget(input$income, budget_data)
      c("everything_else" = input$everything_else, 
        "groceries" = input$groceries,
        "dining" = input$dining,
        "gas" = input$gas,
        "utility" = input$utility,
        "home_improvement" = input$home_improvement,
        "online_shopping" = input$online_shopping,
        "drug_store" = input$drug_store,
        "travel_other" = input$travel_other,
        "phone" = input$phone,
        "streaming" = input$streaming,
        "department_store" = input$department_store,
        "entertainment" = input$entertainment,
        "cable_internet" = input$cable_internet,
        "hotel_portal" = input$hotel_portal,
        "airline_portal" = input$airline_portal,
        "car_portal" = input$car_portal,
        "office_supplies" = input$office_supplies)
    })
    
    portfolio <- reactive({
      get_portfolio(input$K, input$eta/100, input$theta/100, 
                    cards_data[cards_data$bank %in% input$banks,], 
                    budget(), verbose = FALSE)
    })

    output$budget_everything_else <- renderUI({
      numericInput(inputId="everything_else", label="Everything Else", value=round(get_budget(input$income, budget_data)["everything_else"]), min = 1)
    })
    output$budget_groceries <- renderUI({
      numericInput(inputId="groceries", label="Groceries", value=round(get_budget(input$income, budget_data)["groceries"]), min=0)
    })
    output$budget_dining <- renderUI({
      numericInput(inputId="dining", label="Dining", value=round(get_budget(input$income, budget_data)["dining"]), min=0)
    })
    output$budget_gas <- renderUI({
      numericInput(inputId="gas", label="Gas", value=round(get_budget(input$income, budget_data)["gas"]), min=0)
    })
    output$budget_utility <- renderUI({
      numericInput(inputId="utility", label="Utility", value=round(get_budget(input$income, budget_data)["utility"]), min=0)
    })
    output$budget_home_improvement <- renderUI({
      numericInput(inputId="home_improvement", label="Home Improvement", value=round(get_budget(input$income, budget_data)["home_improvement"]), min=0)
    })
    output$budget_online_shopping <- renderUI({
      numericInput(inputId="online_shopping", label="Online Shopping", value=round(get_budget(input$income, budget_data)["online_shopping"]), min=0)
    })
    output$budget_drug_store <- renderUI({
      numericInput(inputId="drug_store", label="Drug Store", value=round(get_budget(input$income, budget_data)["drug_store"]), min=0)
    })
    output$budget_travel_other <- renderUI({
      numericInput(inputId="travel_other", label="Travel (non-portal)", value=round(get_budget(input$income, budget_data)["travel_other"]), min=0)
    })
    output$budget_phone <- renderUI({
      numericInput(inputId="phone", label="Phone", value=round(get_budget(input$income, budget_data)["phone"]), min=0)
    })
    output$budget_streaming <- renderUI({
      numericInput(inputId="streaming", label="Streaming", value=round(get_budget(input$income, budget_data)["streaming"]), min=0)
    })
    output$budget_department_store <- renderUI({
      numericInput(inputId="department_store", label="Department Store", value=round(get_budget(input$income, budget_data)["department_store"]), min=0)
    })
    output$budget_entertaintment <- renderUI({
      numericInput(inputId="entertainment", label="Entertainment", value=round(get_budget(input$income, budget_data)["entertainment"]), min=0)
    })
    output$budget_cable_internet <- renderUI({
      numericInput(inputId="cable_internet", label="Cable & Internet", value=round(get_budget(input$income, budget_data)["cable_internet"]), min=0)
    })
    output$budget_hotel_portal <- renderUI({
      numericInput(inputId="hotel_portal", label="Hotel (portal)", value=round(get_budget(input$income, budget_data)["hotel_portal"]), min=0)
    })
    output$budget_airline_portal <- renderUI({
      numericInput(inputId="airline_portal", label="Airline (portal)", value=round(get_budget(input$income, budget_data)["airline_portal"]), min=0)
    })
    output$budget_car_portal <- renderUI({
      numericInput(inputId="car_portal", label="Rental Car (portal)", value=round(get_budget(input$income, budget_data)["car_portal"]), min=0)
    })
    output$budget_office_supplies <- renderUI({
      numericInput(inputId="office_supplies", label="Office Supplies", value=round(get_budget(input$income, budget_data)["office_supplies"]), min=0)
    })

      output$waterfallPlot <- renderPlot({
      waterfall(values = portfolio()$marginal_benefit, labels = portfolio()$cards,
                calc_total = TRUE, draw_lines = FALSE, total_rect_color = "orange",
                total_rect_text_color = "black", rect_width = 0.9, rect_text_size = 1.5, 
                put_rect_text_outside_when_value_below = 0.2 * (max(cumsum(portfolio()$marginal_benefit)) -
                                                                   min(cumsum(portfolio()$marginal_benefit))),
                rect_text_labels = scales::dollar_format(accuracy = 1)(portfolio()$marginal_benefit), 
                total_rect_text  = scales::dollar_format(accuracy = 1)(sum(portfolio()$marginal_benefit))) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        # labs(title = TeX(sprintf(r'(K = %d    income = %s    $\eta$ = %.1f    $\theta$ = %.1f)', 
        #                          input$K, input$income, input$eta, input$theta)), 
        labs(title = "Marginal Benefit Per Card", x = "", y = "Net Benefit") +
        theme(plot.title = element_text(hjust = 0.5), axis.title = element_text()) +
        scale_y_continuous(labels = scales::dollar_format()) +
        theme(text=element_text(size=18))
        })
    
    output$text1 <- renderText({
      paste0("The optimal portfolio has a total benefit of $", 
            round(tail(portfolio()$net_benefit, n = 1), digits = 2), 
            " a total spend of $", round(portfolio()$total_spend, digits = 2), 
            ", and a return-on-spend of ", 
            round(100*tail(portfolio()$net_benefit, n=1) / portfolio()$total_spend, digits = 2), "%.")
      })

    # Print a table with card assignments, but ignore the category if it is the 
    # same as the Everything Else category:
    output$table <- renderTable({
      data.frame(Category = str_to_title(gsub('[_]', ' ', names(portfolio()$card_assignments)[append(which(portfolio()$card_assignments != portfolio()$card_assignments[1]),1) ])),
                 Card = portfolio()$card_assignments[append(which(portfolio()$card_assignments != portfolio()$card_assignments[1]),1) ])
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

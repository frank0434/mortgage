#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#https://www.mymove.com/mortgage/mortgage-calculation/


library(shiny)
library(ggplot2)
library(magrittr)
library(DT)
library(data.table)
# library(plotly)
library(htmltools)

mortgage <- function(P, i, year){
  # M = Total monthly payment
  # P = The total amount of your loan
  # I = Your interest rate, as a monthly percentage
  # N = The total amount of months in your timeline for paying off your mortgage
  i = i/12
  n = year * 12
  M = (P *  (i * (1 + i)^n) / ((1 + i)^n - 1))
  # M = paste("Monthly repayment is", round(M))
  M 
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Frank's personal finanicial advisory service"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          # input variables for the amount you want to borrow
          # the interest rate
          # and how many years you want to repay
            sliderInput("amount",
                        "The total amount:",
                        min = 10000,
                        max = 1500000,step = 10000,
                        value = 10000),
            numericInput("interest", label = "interest rate",
                         value = 0.045, min = 0.000, max = 0.1),
            numericInput("year", label = "how many years",
                         value = 30, min = 10, max = 30),
            numericInput("netincome", label = "how much you earn monthly (after tax)?",
                         value = 2500, min = 0, max = 1000000000),
            # get a fancy but useless plot to show Annuity mortgage repayment
            plotOutput("plot")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          uiOutput("text"),
          # output the amortisation table 
          DT::dataTableOutput("tbl")
          
        )
        
    )
)
# help from the website: 
# https://www.thebalancemoney.com/amortization-calculator-5115846
# calculate amortization table
server <- function(input, output) {
  # amort_tab <- reactive({})

  amort_tab <- reactive({
      # Find your monthly interest rate: Divide your interest rate by 12 to get 
      # your monthly interest rate. In this case, it???s 0.008333 (0.10/12).
      
      # Calculate your interest payment: Multiply your monthly interest rate by
      # your current balance. Here, it???s $33.33 (0.008333 x $4,000).
      
      # Calculate your principal payment: Subtract your interest payment from
      # your total monthly payment to see how much goes toward paying down your
      # loan. In this example, it???s $318.33 ($351.66 ??? $33.33). 
      
      # Calculate your
      # remaining balance: Subtract your principal payment from your current
      # balance to get your new remaining balance. Here, it???s $3,681.67 ($4,000
      # ??? $318.33).
      i = input$interest
      P = input$amount
      year = input$year
      n = 30 * 12
      M = mortgage(P, i, year)
      # M = paste("Monthly repayment is", round(M))
      int_monthly = i/12
      
      x = seq(n)
      first_month_int = P * int_monthly
      first_month_p = M - first_month_int
      first_month_r = P - first_month_p
      tax_deduct_init = 0.43
      tax_deduct_reduce_rate = 0.029
      year = 5
      tax_return = as.numeric(c(tax_deduct_init, NA))
      for (i in 2:year){
        tax_return[i] = tax_return[i-1] - tax_deduct_reduce_rate
      }
        
      period = data.frame(year = 1:year,
                          tax_return = tax_return)
      period = period[rep(seq_len(nrow(period)), each = 12), ]
      period$month = rep(1:12, times = year)
      
      df = data.frame(month = x, amount = P, monthly_pay = M,
                      interest_pay = c(first_month_int, NA), 
                      principal_pay = c(first_month_p, NA), 
                      remaining_bal = c(first_month_r,NA),
                      tax_return = period$tax_return)
      for(i in 2:n){
        
        df$interest_pay[i] = df$remaining_bal[i - 1] * int_monthly
        df$principal_pay[i] = M -  df$interest_pay[i]
        df$remaining_bal[i] = df$remaining_bal[i - 1] - df$principal_pay[i]
        
      }
      # Calculate net income 
      df$net_income = input$netincome
      df$tax_deduct = df$tax_return * df$interest_pay
      df$net_pay = df$monthly_pay - df$tax_deduct
      df$DI =  df$net_income -  df$net_pay 
      return(df)
    })
  output$plot <- renderPlot({
    P <- ggplot(amort_tab(), aes(month, monthly_pay)) +
      geom_area(aes(fill = "monthly_pay"))+
      geom_area(aes(y = interest_pay, fill = "interest_pay"))+
      theme_classic() +
      scale_x_continuous(limits = c(0,NA), expand = c(0,0))+
      scale_y_continuous(limits = c(0,NA), expand = c(0,0))+
      scale_fill_manual(name = "", values = c("red", "grey50"), 
                        labels = c("Interest payment","Monthly payment"))+
      theme(legend.position = "bottom", text = element_text(size = 18)) +
      ylab("Monthly Payment")
    P
    
    
  })
    output$text <- renderUI({
      P = input$amount
      i = input$interest
      year = input$year
      
      M = mortgage(P, i, year)
      # paste("Monthly payment is ", M)
      HTML(paste0(
        "<h3>", "Summary", "</h3>",
        "Principal (loan amount): ", format(round(P, 2), big.mark = ","),
        "<br>",
        "Annual interest rate: ", i * 100, "%",
        "<br>",
        "Term: ", year, " years (", year * 12, " months)",
        "<br>",
        "<b>", "Monthly payment: ", format(round(M, digits = 2), big.mark = ","), "</b>",
        "<br>"
      ))
    })
    output$tbl <-  DT::renderDataTable({
      options(DT.options = list(pageLength = 12))
      DT::datatable(amort_tab(),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: bottom; text-align: left;',
                      'Notes: ', htmltools::em('DI = Disposable income')
                    )) %>% 
        formatRound(columns = c(3:11),2) %>% 
        formatStyle(
          columns = c('net_pay','DI'),
          fontWeight = "bold",
          backgroundColor = 'yellow')
      
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

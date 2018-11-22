source("goldPrice.R")
# Shiny App
library(shiny)
library(rsconnect)
library(shinythemes)
library(plotly)
library(gridExtra)


options(shiny.usecairo=T)

ui <- fluidPage(theme = shinytheme("flatly"),
                titlePanel("Gold Price Analysis"),
                sidebarLayout(
                  sidebarPanel(
                    dateRangeInput("reportdate",
                                   "Select Time Period: ",
                                   format = "dd-mm-yyyy",
                                   startview = 'year',
                                   start = "2018-01",
                                   end   = "2018-12",
                                   min = "1979-01",
                                   max = "2020-12"
                    ),
                    selectInput("diffs", 
                                h5("Chart 1: Select Type of Differences: "),
                                choices = list("Daily Price" = 1,
                                               "One-day Differences" = 2,
                                               "7-day Differences" = 3,
                                               "30-day Differences" = 4), 
                                selected = 1
                    ),
                    
                    selectInput("variable2", 
                                h5("Chart 2: Select Variable for Rolling 
                                   Correlation with Gold Price: "), 
                                choices = list("One Month Treasury Bill Yield" =  "onemo",
                                               "Three Month Treasury Bill Yield" =  "threemo",
                                               "Six Month Treasury Bill Yield" =  "sixmo",
                                               "One Year Treasury Bill Yield" =  "oneyr",
                                               "Two Year Treasury Note Yield" =  "twoyr",
                                               "Three Year Treasury Note Yield" =  "threeyr",
                                               "Five Year Treasury Note Yield" =  "fiveyr",
                                               "Seven Year Treasury Note Yield" =  "sevenyr",
                                               "Ten Year Treasury Note Yield" =  "tenyr",
                                               "Twenty Year Treasury Bond Yield" =  "twentyyr",
                                               "Thirty Year Treasury Bond Yield" =  "thirtyyr"), 
                                selected = "threemo"),
                    
                    numericInput("days", 
                                 h5("Chart 2: Select Lag Value for Rolling Correlation 
                                    (number of days from 2-50): "), 
                                 value = 7, 
                                 min = 2, 
                                 max = 51)),
                  
                  mainPanel(plotlyOutput("lineChart1"), 
                            plotlyOutput("lineChart2")
                  )
                    )
                ) 

server <- function(input, output) {
  output$lineChart1 <- renderPlotly({
    date_1 = as.Date(format(input$reportdate[1]))
    date_2 = as.Date(format(input$reportdate[2]))
    
    if (input$diffs == 2){
      
      #adjust y limits
      gold_fd1 = subset(gold_fd, date <=date_2 & date >= date_1)
      m <- gold_fd1$value_lag1
      min_v = min(m) - 5
      max_v = max(m) + 5
      
      a <- list(title = "One-day Difference in Price ($)",
                showticklabels = TRUE,
                range = c(min_v, max_v)
      )
      b <- list(title = "Date",
                showticklabels = TRUE,
                range = c(date_1, date_2)
      )
      plot_ly(gold_fd, x = ~date, y = ~value_lag1, type = 'scatter', mode = 'lines', line = list(color = 'FF6600', width = 1) )  %>% layout(title = "Chart 1: Daily Gold Price ($): One-day Differences",  xaxis = b,yaxis = a)
      
    } else if (input$diffs == 3){
      
      #adjust y limits
      gold_wd1 = subset(gold_wd, date <=date_2 & date >= date_1)
      m <- gold_wd1$value_lag7
      min_v = min(m) - 5
      max_v = max(m) + 5
      
      a <- list(title = "7-day Difference in Price ($)",
                showticklabels = TRUE,
                range = c(min_v, max_v)
      )
      b <- list(title = "Date",
                showticklabels = TRUE,
                range = c(date_1, date_2)
      )
      
      plot_ly(gold_wd, x = ~date, y = ~value_lag7, type = 'scatter', mode = 'lines', line = list(color = 'FF6600', width = 1)) %>% 
        layout(title = "Chart 1: Daily Gold Price ($): 7-day Differences", 
               xaxis = b, yaxis = a)
      
    } else if (input$diffs == 4){
      
      #adjust y limits
      gold_md1 = subset(gold_md, date <=date_2 & date >= date_1)
      m <- gold_md1$value_lagm
      min_v = min(m) - 5
      max_v = max(m) + 5
      
      a <- list(title = "30-day Difference in Price ($)",
                showticklabels = TRUE,
                range = c(min_v, max_v)
      )
      b <- list(title = "Date",
                showticklabels = TRUE,
                range = c(date_1, date_2)
      )
      
      plot_ly(gold_md, x = ~date, y = ~value_lagm, type = 'scatter', mode = 'lines', line = list(color = 'FF6600', width = 1)) %>%     
        layout( title = "Chart 1: Daily Gold Price ($): 30-day Differences", 
                xaxis = b, yaxis = a)
    } else {
      gold_1 = subset(gold, date <=date_2 & date >= date_1)
      m <- gold_1$Value
      min_v = min(m) - 5
      max_v = max(m) + 5
      a <- list(title = "Daily Gold Price ($)",
                showticklabels = TRUE,
                range = c(min_v, max_v)
      )
      b <- list(title = "Date",
                showticklabels = TRUE,
                range = c(date_1, date_2)
      )
      plot_ly(gold, x = ~date, y = ~Value, type = 'scatter', mode = 'lines', line = list(color = 'FF6600', width = 1)) %>% 
        layout( title = "Chart 1: Daily Gold Price ($)", xaxis = b, yaxis = a)
    }
  })
  
  output$lineChart2 <- renderPlotly({
    
    date_1 = as.Date(format(input$reportdate[1]))
    date_2 = as.Date(format(input$reportdate[2]))
    
    date_1 = as.Date(format(date_1))
    date_2 = as.Date(format(date_2))
    
    variable2 = as.character(input$variable2)
    
    gy = data.frame(gold_yield$date, gold_yield$Value, gold_yield[,variable2])
    colnames(gy) <-  c("date", "value", variable2)
    gy = na.omit(gy, cols=variable2)
    gy = na.omit(gy, cols="value")
    
    
    window = as.numeric(input$days)
    v = length(gy$value)
    result_len = v - window
    result<-array(NA, c(result_len))
    for(i in 1:result_len) {
      for (j in 1:window) {
        result[i]<-cor(as.numeric(gy[, variable2][i:(i+window)]), 
                       x = as.numeric(gy$value[i:(i+window)]))
      }
    }
    
    
    date1 = gy$date[-c(0:window)]
    df = data.frame(date1, result)
    
    titles <- c("1-Month Treasury Bill Yield", "3-Month Treasury Bill Yield",
                "6-Month Treasury Bill Yield", "1-Year Treasury Bill Yield", 
                "2-Year Treasury Note Yield", "3-Year Treasury Note Yield", 
                "5-Year Treasury Note Yield","7-Year Treasury Note Yield",
                "10-Year Treasury Note Yield", "20-Year Treasury Bond Yield",
                "30-Year Treasury Bond Yield")
    names(titles) <- c("onemo","threemo", "sixmo","oneyr", "twoyr","threeyr",
                       "fiveyr","sevenyr", "tenyr", "twentyyr", "thirtyyr")
    
    a <- list(title = titles[variable2],
              showticklabels = TRUE
    )
    b <- list(title = "Date",
              showticklabels = TRUE,
              range = c(date_1, date_2)
    )
    plot_ly(df, x = ~date1, y = ~result, type = 'scatter', mode = 'lines', line = list(color = '666666', width = 1)) %>% 
      layout( xaxis = b, yaxis = a)
  })
}

shinyApp(ui, server)





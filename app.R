#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(tidyverse)
library(vroom)
library(DT) 
library(paletteer)
library(crosstalk)
#library(leaflet) 
library(rsconnect)
# rsconnect::deployApp()


rm(list=ls()) 

not_sel <- "Not Selected"


main_page <- tabPanel(
    title = "Analysis",
    titlePanel("Analysis"),
    sidebarLayout(
        sidebarPanel(
            title = "Inputs",
            fileInput("csv_input", "Select Male Internal Testers CSV File to Import", accept = ".csv"),
            
            selectInput("num_var_1", "Length", choices = c("Length")),
            selectInput("num_var_2", "Width", choices = c("Width")),
            selectInput("num_var_3", "Instep", choices = c("Instep")),
            selectInput("num_var_4", "Girth", choices = c("Girth")),
            
            #----Factor Variable - ie. how many of each shoe size
            selectInput("num_var_5", "Choose a Variable", choices = c("Choose a Variable")),  
            br(),
            h3("How many people per shoe size"),
            
            selectInput("fact_var", "Factor Variable", choices = c("ShoeSize")),
            
            ##----Shoe Size Slider ---
            sliderInput("ShoeSize", label = h3("Select Shoe Size"),min = 5, 
                        max = 15, value = c(9), step = 0.5),
            
            br(),
            h4('Once you have uploaded a Table and selected a variable, press', span("Run Analysis.", style = "color:blue"), 
               "If you would like to look at new metrics in a different shoe size, click",  span("Run Analysis", style = "color:blue"), "again to refresh the page."),
            actionButton("run_button", "Run Analysis", icon = icon("play")),
            br(),  
            strong(h4('How the measurments were taken', style = "color:orange")),
            fluidRow(
                column( 10,img(src = "Aetrex_foot length.png", height = 265, width = 130),
                        img(src = "Aetres width.png", height = 265, width = 130))
            ),
            fluidRow(
                column( 10, img(src = "aetres instep.png", align = "Center", title = "Instep", height = 170, width = 225), 
                        img(src = "Girth.PNG", align = "Center", height = 170, width = 225))
            )
            
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    title = "Data Tables",  
                    DT::dataTableOutput("mytable"),
                    # df <- read.csv("data/Employee_MaleFootScans.csv"),
                    
                    fluidRow(
                        column(width = 4, strong(textOutput("num_var_1_title"))),
                        column(width = 4, strong(textOutput("num_var_2_title"))),
                        column(width = 4, strong(textOutput("num_var_3_title"))),
                        
                    ),
                    fluidRow(
                        column(width = 4, tableOutput("num_var_1_summary_table")),
                        column(width = 4, tableOutput("num_var_2_summary_table")),
                        column(width = 4, tableOutput("num_var_3_summary_table")),
                        
                    ),
                    fluidRow(
                        column(width = 4, strong(textOutput("num_var_4_title"))),
                        column(width = 4, strong(textOutput("num_var_5_title"))),
                        column(width = 4, strong(textOutput("fact_var_title"))),
                        
                    ),
                    fluidRow(
                        column(width = 4, tableOutput("num_var_4_summary_table")),
                        column(width = 4, tableOutput("num_var_5_summary_table")), 
                        column(width = 4, tableOutput("fact_var_summary_table")), 
                        h4(span(style = "color:blue", "* Note:"), style = "color:black", "These metrics are based off of the assigned shoe size via Aetrex (the foot scanning software). The shoe size that a tester would typically regularly wear are in the column 'RepShoeSize'." )    
                    ),
                )
            )
        )
    )
)



create_num_var_table <- function(data_input, num_var){
    if(num_var != not_sel){
        col <- data_input[,get(num_var)]
        if (length(col)>5000) col_norm <- sample(col,5000) else col_norm <- col
        norm_test <- shapiro.test(col_norm)
        statistic <- c("5th percentile - Low end","25th percentile - Mid Low", "50th percentile - Average",
                       "75th percentile - Mid High", "95th percentile - High end"
        )
        value <- c(round(quantile(col, 0.05, na.rm = TRUE),2),round(quantile(col, 0.25, na.rm = TRUE),2),round(quantile(col,0.5, na.rm = TRUE),2),
                   round(quantile(col, 0.75, na.rm = TRUE),2), round(quantile(col, 0.95,na.rm = TRUE),2)
        )
        data.table(statistic, value)
    }
}

create_fact_var_table <- function(data_input, fact_var){
    if(fact_var != not_sel){
        freq_tbl <- data_input[,.N/2, by = get(fact_var) ]
        freq_tbl <- setnames(freq_tbl,c("factor_value", "count"))
        freq_tbl
    }
}



create_combined_table <- function(data_input, num_var_1, num_var_2, fact_var){
    if(fact_var != not_sel){
        if(num_var_1 != not_sel & num_var_2 != not_sel){
            res_tbl <- data_input[,.(correlation = cor(get(num_var_1), get(num_var_2))), by = fact_var]
        }
        else if(num_var_1 != not_sel & num_var_2 == not_sel){
            res_tbl <- data_input[,.(mean = mean(get(num_var_1))), by = fact_var]
        }
        else if(num_var_1 == not_sel & num_var_2 != not_sel){
            res_tbl <- data_input[,.(mean = mean(get(num_var_2))), by = fact_var]
        }
    }
    else if(num_var_1 != not_sel & num_var_2 != not_sel){
        res_tbl <- data.table(
            statistic = c("correlation"),
            value = c(cor(
                data_input[,get(num_var_1)],
                data_input[,get(num_var_2)])))
    }
    return(res_tbl)
}

ui <- navbarPage(
    title = "BOA Foot Volume App via BigData",
    theme = shinytheme('sandstone'),
    main_page
    
    
)

server <- function(input, output, session){
    
    options(shiny.maxRequestSize=10*1024^2)
    
    data_input <- reactive({
        req(input$csv_input)
        fread(input$csv_input$datapath)
    })
    
    
    
    
    observeEvent(data_input(),{
        choices <- c(not_sel,names(data_input()))
        updateSelectInput(inputId = "num_var_1", choices = 'Length')
        updateSelectInput(inputId = "num_var_2", choices = 'Width') 
        updateSelectInput(inputId = "num_var_3", choices = 'Instep')
        updateSelectInput(inputId = "num_var_4", choices = 'Girth')
        updateSelectInput(inputId = "num_var_5", choices = choices) 
        updateSelectInput(inputId = "fact_var", choices = 'ShoeSize')
    })
    num_var_1 <- eventReactive(input$run_button,input$num_var_1)
    num_var_2 <- eventReactive(input$run_button,input$num_var_2)
    num_var_3 <- eventReactive(input$run_button,input$num_var_3)
    num_var_4 <- eventReactive(input$run_button,input$num_var_4)
    num_var_5 <- eventReactive(input$run_button,input$num_var_5)
    fact_var <- eventReactive(input$run_button,input$fact_var)
    
  
    
    #Sliders 
    #-----------------
    df_subset <- reactive({
      a <- subset(data_input(), ShoeSize == input$ShoeSize)
      return(a)
    }) 
    

    #Old sliders 
    #-----------------
    # df_subset <- reactive({
    #   a <- subset(data_input(), ShoeSize >= input$MinShoeSize & ShoeSize <= input$MaxShoeSize)
    #   return(a)
    # }) 
    
    # df_subset <- reactive({
    #     a <- subset(data_input(), ShoeSize >= input$ShoeSizeRange)
    #     return(a)
    # })
    
    
    
    # Data Table  
    #-----------------
    output$mytable = DT::renderDataTable({ 
    DT::dataTableOutput(read.csv("Employee_MaleFootScans.csv"))
        df_subset()
        
    })
    
    
  
  
    # 1-d summary tables 
    #-----------------
    
    output$num_var_1_title <- renderText(paste(num_var_1()))
    
    num_var_1_summary_table <- eventReactive(input$run_button,{
        create_num_var_table(df_subset(),   num_var_1())
        
    })
    
    output$num_var_1_summary_table <- renderTable(num_var_1_summary_table(),colnames = FALSE)
    
    
    
    output$num_var_2_title <- renderText(paste(num_var_2()))
    
    num_var_2_summary_table <- eventReactive(input$run_button,{
        create_num_var_table(df_subset(), num_var_2())
        
    })
    
    output$num_var_2_summary_table <- renderTable(num_var_2_summary_table(),colnames = FALSE)
    
    
    
    output$num_var_3_title <- renderText(paste(num_var_3()))
    
    num_var_3_summary_table <- eventReactive(input$run_button,{
        create_num_var_table(df_subset(), num_var_3())
        
    })
    
    output$num_var_3_summary_table <- renderTable(num_var_3_summary_table(),colnames = FALSE)
    
    
    
    
    output$num_var_4_title <- renderText(paste(num_var_4()))
    
    num_var_4_summary_table <- eventReactive(input$run_button,{
        create_num_var_table(df_subset(), num_var_4())
        
    })
    
    output$num_var_4_summary_table <- renderTable(num_var_4_summary_table(),colnames = FALSE)
    
    
    
    output$num_var_5_title <- renderText(paste(num_var_5()))
    
    num_var_5_summary_table <- eventReactive(input$run_button,{
        create_num_var_table(df_subset(), num_var_5()) 
        
        
    })
    
    output$num_var_5_summary_table <- renderTable(num_var_5_summary_table(),colnames = FALSE)
    
    
    
    output$fact_var_title <- renderText(paste(fact_var(), "| Number of people per shoe size"))

    fact_var_summary_table <- eventReactive(input$run_button,{
        create_fact_var_table(df_subset(), fact_var())
    })

    output$fact_var_summary_table <- renderTable(fact_var_summary_table(),colnames = FALSE)
    
    
    
    
    
}


shinyApp(ui = ui, server = server)

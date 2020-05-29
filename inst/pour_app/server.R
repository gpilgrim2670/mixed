library(shiny)
library(dplyr)
library(ggplot2)
library(readr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    '%!in%' <- function(x,y)!('%in%'(x,y))
    specific_gravity <- read_csv("specific_gravity.csv")

    output$Drink_1_In <- renderUI({
        selectizeInput(
            inputId = "Drink_1",
            label = "Drink 1",
            selected = "Orange Liqueur",
            choices = unique(specific_gravity$drink),
            options = list(maxItems = 1,
                           placeholder = 'Enter First Drink'
                           # onInitialize = I('function() { this.setValue("None"); }')
            )
        )
    })

    output$Vol_1_In <- renderUI({
    sliderInput(inputId = "Vol_1",
                label = "Volume Percent Drink 1",
                value = 33,
                min = 0,
                max = 100,
                step = 1)
    })

    output$Drink_2_In <- renderUI({
        selectizeInput(
            inputId = "Drink_2",
            label = "Drink 2",
            selected = "Baileys Irish Cream",
            choices = unique(specific_gravity$drink[specific_gravity$drink != input$Drink_1]),
            options = list(maxItems = 1,
                           placeholder = 'Enter First Drink'
                           # onInitialize = I('function() { this.setValue("None"); }')
            )
        )
    })

    output$Vol_2_In <- renderUI({
        sliderInput(inputId = "Vol_2",
                    label = "Volume Percent Drink 2",
                    value = 33,
                    min = 0,
                    max = 100 - input$Vol_1,
                    step = 1)
    })

    output$Drink_3_In <- renderUI({
        selectizeInput(
            inputId = "Drink_3",
            label = "Drink 3",
            selected = "Creme de Menthe",
            choices = unique(specific_gravity$drink[specific_gravity$drink %!in% c(input$Drink_1, input$Drink_2)]),
            options = list(maxItems = 1,
                           placeholder = 'Enter First Drink'
                           # onInitialize = I('function() { this.setValue("None"); }')
            )
        )
    })

    output$Vol_3_In <- renderUI({
        sliderInput(inputId = "Vol_2",
                    label = "Volume Percent Drink 3",
                    value = 100 - sum(input$Vol_1, input$Vol_2),
                    min = 100 - sum(input$Vol_1, input$Vol_2),
                    max = 100 - sum(input$Vol_1, input$Vol_2),
                    step = 1)
    })


    output$drink <- renderPlot({
        df <- specific_gravity %>%
            filter(drink %in% c(input$Drink_1, input$Drink_2, input$Drink_3)) %>%
            filter(drink != "None")

        # percs <- c(0.1, 0.5, 0.4)
        percs <- c(input$Vol_1/100, input$Vol_2/100, (100 - sum(input$Vol_1, input$Vol_2, na.rm = TRUE))/100)
        pour(drink = df, area_pcts = percs)



    })

})

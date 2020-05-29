
library(shiny)
library(ggplot2)
library(readr)

# specific_gravity <- read_csv("specific_gravity.csv")

# drinks_df <-
#     system.file("pour_app", "www", "specific_gravity.rda", package = "mixed")
# load("specific_gravity.rda")



# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Pilgrims' Rest Taphouse"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            uiOutput("Drink_1_In"),

            uiOutput("Vol_1_In"),

            uiOutput("Drink_2_In"),

            uiOutput("Vol_2_In"),

            uiOutput("Drink_3_In"),

            uiOutput("Vol_3_In"),
            ),
        # Show a plot of the generated distribution
        mainPanel(plotOutput("drink"))
    )))

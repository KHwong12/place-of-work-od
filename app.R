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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Theme options --------
    theme = shinytheme("flatly"),
    includeCSS("style.css"),
    
    
    # Application title
    titlePanel("Flow of commuters"),
    
    checkboxGroupInput("checkGroup", 
                      h3("Place of residence"), 
                      choices = list("Central and Western" = 0, 
                                     "Wan Chai" = 1, 
                                     "Eastern" = 2,
                                     "Southern" = 3,
                                     "Yau Tsim Mong" = 4,
                                     "Sham Shui Po" = 5,
                                     "Kowloon City" = 6,
                                     "Wong Tai Sin" = 7,
                                     "Kwun Tong" = 8,
                                     "Kwai Tsing" = 9,
                                     "Tsuen Wan" = 10,
                                     "Tuen Mun" = 11,
                                     "Yuen Long" = 12,
                                     "North" = 13,
                                     "Tai Po" = 14,
                                     "Sha Tin" = 15,
                                     "Sai Kung" = 16,
                                     "Islands" = 17),
                      selected = 1),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    ),
    
    hr(),
    
    
    
    # Footer ---------------
    div(
        class = "footer",
        includeHTML("template/footer.html")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

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

library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggalluvial)

DISTRICT_ORDER <- c("Central and Western", "Wan Chai", "Eastern", "Southern",
                    "Yau Tsim Mong", "Sham Shui Po", "Kowloon City", "Wong Tai Sin", "Kwun Tong",
                    "Kwai Tsing", "Tsuen Wan", "Tuen Mun", "Yuen Long",
                    "North", "Tai Po", "Sha Tin", "Sai Kung", "Islands")

WORKPLACE_ORDER <- c("Central and Western", "Wan Chai", "Eastern", "Southern",
                     "Yau Tsim Mong", "Sham Shui Po", "Kowloon City", "Wong Tai Sin", "Kwun Tong",
                     "Kwai Chung New Town", "Tsuen Wan New Town", "Tsing Yi New Town", "Tuen Mun New Town", "Tin Shui Wai New Town", "Yuen Long New Town",
                     "Fanling/ Sheung Shui New Town", "Tai Po New Town", "Sha Tin New Town", "Ma On Shan New Town", "Tseung Kwan O New Town", "North Lantau New Town",
                     "Other areas in the New Territories", "No fixed places of work in Hong Kong/ Marine/ Work at home/ Places outside Hong Kong")

DISTRICT_ORDER_CHI <- c("中西區", "灣仔", "東區", "南區",
                        "油尖旺", "深水埗", "九龍城", "黃大仙", "觀塘",
                        "葵青", "荃灣", "屯門", "元朗",
                        "北區", "大埔", "沙田", "西貢", "離島")

WORKPLACE_ORDER_CHI <- c("中西區", "灣仔", "東區", "南區",
                         "油尖旺", "深水埗", "九龍城", "黃大仙", "觀塘",
                         "葵涌新市鎮", "荃灣新市鎮", "青衣新市鎮", "屯門新市鎮", "天水圍新市鎮", "元朗新市鎮",
                         "粉嶺／上水新市鎮", "大埔新市鎮", "沙田新市鎮", "馬鞍山新市鎮", "將軍澳新市鎮", "北大嶼山新市鎮",
                         "新界其他地區", "在香港沒有固定工作地點／水上／於家中工作／香港以外地方")

dataset <- read_csv("data/data-output/cross_travel_type_rmspecial.csv") %>% 
    mutate(
        place_of_residence = factor(place_of_residence, DISTRICT_ORDER),
        place_of_work = factor(place_of_work, WORKPLACE_ORDER),
        place_of_residence_chi = factor(place_of_residence_chi, DISTRICT_ORDER_CHI),
        place_of_work_chi = factor(place_of_work_chi, WORKPLACE_ORDER_CHI)
    )

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Theme options --------
    theme = shinytheme("flatly"),
    includeCSS("style.css"),
    
    
    # Application title
    titlePanel("Flow of commuters"),
    
    fluidRow(
        
        column(3 ,
    
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
                      selected = 2),
        ),
    
    column(9,
           # Show a plot of the generated distribution
           mainPanel(
               plotOutput("alluPlot")
           ),
           
           hr(),
           
           )
    ),
    



    
    
    
    # Footer ---------------
    div(
        class = "footer",
        includeHTML("template/footer.html")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    MA_NT_PALETTE = c("FALSE" = "#377eb8", "TRUE" = "#e25a61")

    output$alluPlot <- renderPlot({
        
        req(input$checkGroup)
        
        dataset_selected <- dataset %>%
            subset(origin_ID %in% input$checkGroup)

        
        OD_work_alluvial_travelcolour_mant = ggplot(dataset_selected, aes(y = N_workers, axis1 = place_of_residence, axis2 = place_of_work)) +
            geom_alluvium(aes(fill = is_cross_ma_nt), width = .2, alpha = .4) +
            geom_stratum(width = .2, fill = "#EEEEEE", color = "#FFFFFF") +
            # http://corybrunson.github.io/ggalluvial/reference/stat_stratum.html
            # https://stackoverflow.com/questions/29465941/format-number-in-r-with-both-comma-thousands-separator-and-specified-decimals
            geom_text(stat = "stratum", aes(label = paste0(after_stat(stratum), "\n", after_stat(format(count, big.mark=",")))), size = 2) +
            scale_x_discrete(limits = c("Place of \nResidence", "Place of \nWork"), expand = c(.05, .05), position = "top") +
            scale_fill_manual(values = MA_NT_PALETTE, aesthetics = c("colour", "fill")) +
            # scale_fill_brewer(type = "qual", palette = "Set1") +
            theme_void() +
            theme(
                axis.text.x = element_text(size = 12),
                legend.position = "None",
                aspect.ratio = 3/1
            ) +
            labs(
                title = "Flow of commuters",
                subtitle = "Place of residence and work of commuters in Hong Kong",
                caption = "Note: workers with no fixed places of work or \nwork in Marine/at home/in places outside Hong Kong are excluded."
            )
        
        OD_work_alluvial_travelcolour_mant
    }, height = 900,width = 300)
}

# Run the application 
shinyApp(ui = ui, server = server)

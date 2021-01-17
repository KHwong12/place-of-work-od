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
library(shinyWidgets)

library(tidyverse)
library(ggalluvial)
library(gghighlight)

library(plotly)

library(metathis)

source("R/init-data.R")


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Theme options --------
  theme = shinytheme("flatly"),
  includeCSS("style.css"),
  
  # meta(meta_social(
  #   title = "metathis",
  #   description = "<meta> and social media cards for web things in R",
  #   url = "https://pkg.garrickadenbuie.com/metathis",
  #   image = "https://garrickadenbuie.com/apple-touch-icon-114x114.png",
  #   image_alt = "An image for social meda cards"
  # )),


  # Application title
  titlePanel("The great migration of workers"),
  
  p("Commuting to work is usually not something pleasant. Maybe the work from home culture have changed our understanding of commuting, but
    the general pattern on the commuting seems do not change much. With around 3,000,000 workers living across and working across the whole 
    territory of Hong Kong, how does the flow pattern looks like? "),
  
  div(class = "sticky",
    
    fluidRow(
      h3("Choose your area of interest:")
    ),
    
    fluidRow(
      
      # Origin picker
      column(
        6,
        
        pickerInput(
            "residence_group",
            label = "Place of residence",
            choices = origin_choice_list,
            selected = origin_df$node_ID[1:4], # c(0, 1, 2, 3)
            multiple = TRUE,
            options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = "Unselect All",
                `select-all-text` = "Select All",
                `none-selected-text` = "Select Region(s)...",
                `selected-text-format`= "count",
                `count-selected-text` = "{0} regions choosed (on a total of {1})"
            ),
            choicesOpt = NULL,
            width = NULL,
            inline = FALSE
        )
        
      ),
        
        column(
          6,
          
        
        # Destination picker
        pickerInput(
          "work_group",
          label = "Place of work",
          choices = destination_choice_list,
          selected = destination_df$node_ID[5:9], # 22 - 26 (Kowloon)
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `deselect-all-text` = "Unselect All",
            `select-all-text` = "Select All",
            `none-selected-text` = "Select Region(s)...",
            `selected-text-format`= "count",
            `count-selected-text` = "{0} regions choosed (on a total of {1})"
          ),
          choicesOpt = NULL,
          width = NULL,
          inline = FALSE
        )
      )
    )
  ),

  hr(),

  h2("The OD of workers"),
  
  p("How many people are working from/to each district? The following heatmap helps you grasp a quick understanding
    about the travel pattern. The vertical axis list the districts workers are living in, and the horizontol axis 
    list the working location, classified by districts. Each square in the plot refers to the number of workers",
    tags$b("living"), "in that row and", tags$b("working"), "in that column. Darker the colour of the square, more 
    workers are in that living/working place pattern."),
  
  p("When hovering on the grids, a tooltip will appear, showing the exact number of workers falling to that category."),

  br(),
  
  plotlyOutput("odHeatmap", height = "800px"),

  hr(),

  h2("The flow of workers"),
  
  p("Here we have a bunch of 'flows' meandering from left to right: each flow comprises of the total number of workers
    living in the area stated in the box on the left and workiong in the area stated in box on the right. The color of
    the flows are classified according to the travel type, including:"),
  
  tags$ul(
    tags$li(tags$mark("Same area", style = "color:#ffffff; background-color:#4daf4a")),
    tags$li(tags$mark("Between Hong Kong Island and Kowloon", style = "color:#ffffff; background-color:#377eb8")),
    tags$li(tags$mark("Between Hong Kong Island and New Territories", style = "color:#ffffff; background-color:#ff7f00")),
    tags$li(tags$mark("Between Kowloon and New Territories", style = "color:#ffffff; background-color:#984ea3"))
  ),
  
  # tags$mark("Same area", style = "color:#ffffff; background-color:#4daf4a"),
  # tags$mark("Between Hong Kong Island and Kowloon", style = "color:#ffffff; background-color:#377eb8"),
  # tags$mark("Between Hong Kong Island and New Territories", style = "color:#ffffff; background-color:#ff7f00"),
  # tags$mark("Between Kowloon and New Territories", style = "color:#ffffff; background-color:#984ea3"),
  
  p("Hover/Click on the flows to check the numbers of workers in each flow."),
  
  br(),

  # fluidRow(width = 12, plotlyOutput("sankey", height = "1200px"), align = "center"),
  plotlyOutput("sankey", height = "1200px"),

  hr(),

  h2("The cross-district nightmare"),
  
  p("PLACEHOLDER"),

  plotOutput("alluPlot", height = "1200px"),
  
  hr(),

  # Footer ---------------
  div(
    class = "footer",
    includeHTML("template/footer.html")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session, ...) {

  # https://community.rstudio.com/t/controlling-the-height-of-fluidrow-in-shiny/4968/2
  # to relay the height/width of the plot's container, we'll query this
  # session's client data http://shiny.rstudio.com/articles/client-data.html
  cdata <- session$clientData
  
  MIN_SCREEN_WIDTH <- 300
  MAX_SCREEN_WIDTH <- 768

  output$odHeatmap <- renderPlotly({
    req(input$residence_group)

    dataset_simlabel_selected <- dataset_simlabel %>%
      subset(origin_ID %in% input$residence_group & destination_ID %in% input$work_group)

    od_workers_plotly <- ggplot(
      dataset_simlabel_selected,
      aes(
        x = place_of_work_sim,
        y = fct_rev(place_of_residence),
        text = paste0("Place of Residence: ", place_of_residence, "\n", "Place of Work: ", place_of_work_sim, "\n", "Number of Workers: ", formattable::comma(N_workers, digits = 0)),
        fill = N_workers
      )
    ) +
      geom_tile(color = "#EEEEEE", size = 1) +
      geom_vline(xintercept = c(4.5, 9.5), color = "#ffffff") +
      geom_hline(yintercept = c(9.5, 14.5), color = "#ffffff") +
      scale_fill_viridis_c(option = "A", direction = -1, trans = "log") +
      scale_x_discrete(position = "top") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = -1, hjust = 0)
      ) +
      labs(
        y = "Place of residence",
        x = "Place of work",
        fill = "Number of\nworkers",
        title = "The OD of workers"
      )

    # workaround for transformed colour scale
    # https://stackoverflow.com/questions/54107531/ggplotly-mouse-values-while-using-a-log-transformed-color-scale
    
    # width should be between 300 to max width
    width_heatmap = ifelse(cdata$output_odHeatmap_width > MAX_SCREEN_WIDTH, MAX_SCREEN_WIDTH, max(cdata$output_odHeatmap_width, MIN_SCREEN_WIDTH))
    height_heatmap = width_heatmap * 18/22
  
    # ggplotly cannot handle subtitle
    # https://datascott.com/blog/subtitles-with-ggplotly/
    ggplotly(od_workers_plotly, tooltip = c("text"),
             width = width_heatmap,
             height = height_heatmap) %>%
      layout(title = list(text = paste0('The OD of workers',
                                        '<br>',
                                        '<sup>',
                                        'Number of workers living/working in every selected regions',
                                        '</sup>')))
    # ggplotly(od_workers_plotly, tooltip = c("text"), width = cdata$output_odHeatmap_width, height = cdata$output_odHeatmap_height)
  })

  output$sankey <- renderPlotly({
    req(input$residence_group)

    work_OD_plot_reorder_plotly_selected <- work_OD_plot_reorder_plotly %>%
      subset(origin_ID %in% input$residence_group & destination_ID %in% input$work_group)

    od_sankey_plotly <- plot_ly(
      type = "sankey",
      arrangement = "snap",
      orientation = "h",
      width = cdata$output_odHeatmap_width,
      height = 1200,

      node = list(
        label = nodes_OD[["node_name"]],
        # x = nodes_OD_wposition[["node_x"]],
        # y = nodes_OD_wposition[["node_y"]],
        color = "#666666",
        pad = 20,
        thickness = 20,
        line = list(
          color = "black",
          width = 0.5
        ),
        # https://plotly.com/r/hover-text-and-formatting/
        # printf format for numbers
        # https://stackoverflow.com/questions/23718936/explanation-for-sprintf03d-7-functionality/23719045
        hovertemplate = "<b>%{label}</b> <br />Number of workers: %{value:.5d}"
      ),

      link = list(
        source = work_OD_plot_reorder_plotly_selected[["origin_ID"]],
        target = work_OD_plot_reorder_plotly_selected[["destination_ID"]],
        value = work_OD_plot_reorder_plotly_selected[["N_workers"]],
        color = work_OD_plot_reorder_plotly_selected[["journey_type_colour"]],
        hovertemplate = "There are %{value:.5d} workers<br />living in <b>%{source.label}</b> and<br />working in <b>%{target.label}</b>"
      )
    )
    
    od_sankey_plotly <- od_sankey_plotly %>%
      layout(
        title = list(text = paste0('The flow of workers',
                                 '<br>',
                                 '<sup>',
                                 'Magnitude of the flow of the commuters across the territory, classified by flows\' origin/destination',
                                 '</sup>'))
    )

    od_sankey_plotly
  })

  MA_NT_PALETTE <- c("FALSE" = "#377eb8", "TRUE" = "#e25a61")

  output$alluPlot <- renderPlot({
    
      req(input$residence_group)
      
      # manual gghighlight
      dataset_simlabel_selected_allu <- dataset_simlabel %>%
        mutate(
          journey_type_colour_highlight = ifelse((origin_ID %in% input$residence_group & destination_ID %in% input$work_group), journey_type_colour, "#cccccc33"),
          ) %>%
        mutate(
          journey_type_colour_highlight = factor(journey_type_colour_highlight, levels = c("#4daf4a33", "#377eb833", "#ff7f0033", "#984ea333", "#cccccc33"))
        )

      OD_work_alluvial_travelcolour <- 
        ggplot(dataset_simlabel_selected_allu, aes(y = N_workers, axis1 = place_of_residence, axis2 = place_of_work_sim, fill = journey_type_colour_highlight)) +
        geom_alluvium(aes(fill = journey_type_colour_highlight), width = .2, alpha = 1) +
        geom_stratum(width = .2, fill = "#EEEEEE", color = "#FFFFFF") +
        # http://corybrunson.github.io/ggalluvial/reference/stat_stratum.html
        # https://stackoverflow.com/questions/29465941/format-number-in-r-with-both-comma-thousands-separator-and-specified-decimals
        geom_text(stat = "stratum", aes(label = paste0(after_stat(stratum), "\n", after_stat(format(count, big.mark = ",")))), size = 2.5) +
        scale_x_discrete(limits = c("Place of \nResidence", "Place of \nWork"), expand = c(.05, .05), position = "top") +
        # scale_fill_manual(values = MA_NT_PALETTE, aesthetics = c("colour", "fill")) +
        scale_fill_identity() +
        # scale_fill_brewer(type = "qual", palette = "Set1") +
        coord_cartesian(clip = "off") +
        theme_void() +
        theme(
          axis.text.x = element_text(size = 12),
          legend.position = "None"
        ) +
        labs(
          title = "Flow of commuters",
          subtitle = "Place of residence and work of commuters in Hong Kong",
          caption = "Note: workers with no fixed places of work or \nwork in Marine/at home/in places outside Hong Kong are excluded."
        )
      
      OD_work_alluvial_travelcolour
    },
    height = 1200,
    width = 600
  )
}

# Run the application
shinyApp(ui = ui, server = server)

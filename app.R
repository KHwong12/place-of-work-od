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
  
  meta_social(
    .meta = meta(),
    title = "The great migration of workers",
    description = "Interactive visualisation of around 3 million workers' commuting flows in Hong Kong",
    url = "https://kenneth-12.shinyapps.io/place-of-work-od",
    image = "https://raw.githubusercontent.com/KHwong12/place-of-work-od/master/www/fromtoKCD.png"
  ),


  # Application title and subtitle
  titlePanel(h1("The great migration of workers"), windowTitle = "The great migration of workers"),
  h3("Where do the workers working in your district live, and where do the workers living in your districrt work"),
  
  br(),
  
  htmltools::includeMarkdown("description_md/introduction.md"),
  
  hr(),
  
  p("To start, choose the group of workers you are interested in terms of living and working places. 
    By default, this application chooses all workers living in Hong Kong Island and working in Kowloon."),
  
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
  
  htmltools::includeMarkdown("description_md/heatmap.md"),

  br(),
  
  plotlyOutput("odHeatmap", height = "800px"),

  hr(),
  
  htmltools::includeMarkdown("description_md/sankey.md"),
  
  # tags$ul(
  #   tags$li(tags$mark("Same area (Hong Kong Island/Kowloon/New Territories)", style = "color:#ffffff; background-color:#4daf4a")),
  #   tags$li(tags$mark("Between Hong Kong Island and Kowloon", style = "color:#ffffff; background-color:#377eb8")),
  #   tags$li(tags$mark("Between Hong Kong Island and New Territories", style = "color:#ffffff; background-color:#ff7f00")),
  #   tags$li(tags$mark("Between Kowloon and New Territories", style = "color:#ffffff; background-color:#984ea3"))
  # ),
  
  # tags$mark("Same area", style = "color:#ffffff; background-color:#4daf4a"),
  # tags$mark("Between Hong Kong Island and Kowloon", style = "color:#ffffff; background-color:#377eb8"),
  # tags$mark("Between Hong Kong Island and New Territories", style = "color:#ffffff; background-color:#ff7f00"),
  # tags$mark("Between Kowloon and New Territories", style = "color:#ffffff; background-color:#984ea3"),
  
  br(),

  # fluidRow(width = 12, plotlyOutput("sankey", height = "1200px"), align = "center"),
  plotlyOutput("sankey", height = "1200px"),

  hr(),

  htmltools::includeMarkdown("description_md/sankey-overall.md"),

  plotOutput("alluPlot", height = "1200px"),
  
  p("When you select only one origin and select all destinations (or vice versa), you could produce a highlighted 
    chart designated to your area of interest like below."),
  
  img(src = "fromtoKCD.png", width = "100%"),
  
  hr(),

  # Footer ---------------
  div(
    class = "footer",
    tags$footer("Data source: 2016 Population By-Census"),
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
        # text = paste0("Place of Residence: ", place_of_residence, "\n",
        #               "Place of Work: ", place_of_work_sim, "\n",
        #               "Number of Workers: ", formattable::comma(N_workers, digits = 0)),
        text = paste0("There are ", formattable::comma(N_workers, digits = 0), " workers \n",
                      "living in ", "<b>", place_of_residence, "</b>", " and\n",
                      "working in ", "<b>", place_of_work_sim, "</b>"),
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
    width_heatmap = ifelse(cdata$output_odHeatmap_width > MAX_SCREEN_WIDTH,
                           MAX_SCREEN_WIDTH,
                           # ensure minimum width is 300
                           max(cdata$output_odHeatmap_width, MIN_SCREEN_WIDTH))
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
        title = list(
          text = paste0('Workers flowing',
                        '<br>',
                        '<sup>',
                        'Flow of the commuters across the territory, classified by flows\' origin/destination',
                        '</sup>')
        )
    )

    od_sankey_plotly
  })
  
  output$alluPlot <- renderPlot({
    
      req(input$residence_group)
      
      # manual gghighlight
      dataset_simlabel_selected_allu <- dataset_simlabel %>%
        mutate(
          journey_type_colour_highlight = ifelse((origin_ID %in% input$residence_group & destination_ID %in% input$work_group), journey_type_colour, "#cccccc33"),
          ) %>%
        mutate(
          # put highlighted (selected) rows to the top and draw the non-highlighted grey flows first
          journey_type_colour_highlight = factor(journey_type_colour_highlight, levels = c("#4daf4a66", "#377eb866", "#ff7f0066", "#984ea366", "#cccccc33"))
        )

      OD_work_alluvial_travelcolour <- 
        ggplot(dataset_simlabel_selected_allu,
               aes(y = N_workers, axis1 = place_of_residence, axis2 = place_of_work_sim, fill = journey_type_colour_highlight)) +
        geom_alluvium(aes(fill = journey_type_colour_highlight), width = .2, alpha = 1) +
        geom_stratum(width = .2, fill = "#EEEEEE", color = "#FFFFFF") +
        # http://corybrunson.github.io/ggalluvial/reference/stat_stratum.html
        # https://stackoverflow.com/questions/29465941/format-number-in-r-with-both-comma-thousands-separator-and-specified-decimals
        geom_text(stat = "stratum",
                  aes(label = paste0(after_stat(stratum),
                                     # if count too small (i.e. short stratum height), keep district and count in one single line
                                     ifelse(after_stat(count) < 1e5, " - ", "\n"),
                                     # count with thousand separators
                                     after_stat(format(count, big.mark = ",")))),
                  size = 2) +
        scale_x_discrete(limits = c("Place of\nResidence", "Place of\nWork"), expand = c(.075, .075), position = "top") +
        # scale_fill_manual(values = MA_NT_PALETTE, aesthetics = c("colour", "fill")) +
        scale_fill_identity() +
        # scale_fill_brewer(type = "qual", palette = "Set1") +
        coord_cartesian(clip = "off") +
        theme_void() +
        theme(
          plot.title = element_text(size = 24, face = "bold"),
          plot.subtitle = element_text(size = 12),
          plot.caption = element_text(size = 6, vjust = 25),
          # vjust to move x-axis title lower
          axis.text.x = element_text(size = 10, vjust = -15),
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
    width = 600,
    res = 96
  )
}

# Run the application
shinyApp(ui = ui, server = server)

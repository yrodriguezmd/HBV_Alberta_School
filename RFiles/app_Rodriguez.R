
#----------------------------------------------------------------------------------------------------------
# Created by Maria Rodriguez, Data Scientist, 2023_Nov_16
# Objective:  Create a reproducible data product 
#     to describe grade 6 hepatitis B virus (HBV) school immunization coverage in Alberta over time.
# using data downloaded from http://www.ahw.gov.ab.ca/IHDA_Retrieval, under Immunization and School Coverage
#     filtered: http://www.ahw.gov.ab.ca/IHDA_Retrieval/selectSubCategoryParameters.do#
#-----------------------------------------------------------------------------------------------------------

install.packages(c('shinyWidgets', 'leaflet'))
# install.packages('DT') ## uncomment if want to show table ##

library(shiny)  # facilitates building apps 
library(shinyWidgets)  # enables fine tuning of app features, choice selections for this case
# library(DT)  # facilitates showing of the table in shiny, ## uncomment if needed ##
library(leaflet) # facilitates map plotting

# retrieve previously prepared data
source('main_Rodriguez.R') 

# Shiny application
# first component:  IMMUNIZATION PERCENTAGE per year per Immunization status
# with ability to differentiate between Geography and Sex based on user input

# second component:  STANDARD SCORE per year per zone in a map
# Score_Category color palette based on Data Name designated colours
color_palette <- colorFactor(
  palette = c('red', 'orange','yellow','lightgreen','darkgreen'),
  domain = c('Significantly Higher','High','Average','Low','Significantly Lower'),
  ordered=TRUE
)

yearMap <- function(year) {
  filtered_score_df <- score_df %>%
    filter(Year == year)
  
  leaflet(filtered_score_df) %>%
    addTiles() %>%
    addCircleMarkers(
      lat = ~Latitude,
      lng = ~Longitude,
      label = ~paste(Score_Category),
      labelOptions = labelOptions(style = list('font-size'= '12px')),
      color = ~color_palette(Score_Category),
      fillColor = ~color_palette(Score_Category),
      radius = 20,
      fillOpacity = 0.01
    ) 
}

ui <- fluidPage(
  
  # 1st component: Immunization Percentage
  titlePanel('Figure 1.  HBV Immunization Percentage for Grade 6 students by School Year, Alberta Zones and Sex'),

  fluidRow(
    # choice selection widget for Sex and Geography
    column(width = 3,
           selectInput('selectedSex', 'Select Sex:', choices = c('Both', 'Male', 'Female'), selected= 'Both'),
           pickerInput('selectedGeography', 'Select Geography', choices = unique(perc_df$Geography), 
                       selected= unique(perc_df$Geography), multiple= TRUE, options = list(`actions-box` = TRUE))
    ),
    # show plot for immunization percentage
    column(width = 12,
           plotOutput('myPlot', width = '110%', height = '600px'),
           
           ## uncomment this portion if want table to be shown ##
           # DT::dataTableOutput('myTable'),
    )
  ),
  
  #  2nd component: Standard Score
  titlePanel('Figure 2. HBV Complete Immunizations for Grade 6 students by Zone Relative to Provincial Average'),
  
  fluidRow(
    # one plot column per year to facilitate comparison
    column(width = 2, align = 'center',
           tags$h4('2018-2019'),
           leafletOutput('myMap1', width = '110%', height = '450px')
    ),
    column(width = 2, align = 'center',
           tags$h4('2019-2020'),
           leafletOutput('myMap2', width = '110%', height = '450px')
    ),
    column(width = 2, align = 'center',
           tags$h4('2020-2021'),
           leafletOutput('myMap3', width = '110%', height = '450px')
    ),
    column(width = 2, align = 'center',
           tags$h4('2021-2022'),
           leafletOutput('myMap4', width = '110%', height = '450px')
    ),
    column(width = 2, align = 'center',
           tags$h4('2022-2023'),
           leafletOutput('myMap5', width = '110%', height = '450px')
    )
  )
)


server <- function(input, output) {
  
  # 1st component: Immunization Percentage
  
  ## Uncomment this portion if want the Table to be shown ##
  # output$myTable <- DT::renderDataTable({
  #   filtered_perc_df <- perc_df %>%
  #     filter(Geography %in% input$selectedGeography,
  #            Sex == input$selectedSex)
  #   DT::datatable(filtered_perc_df, options = list(pageLength =10))
  # })

  # process the plot filtered for the user's chosen input/s for Sex and Geography
  output$myPlot <- renderPlot({
    filtered_perc_df <- perc_df %>%
      filter(Geography %in% input$selectedGeography,
             Sex == input$selectedSex)
    ggplot(filtered_perc_df, aes(x = Year, y = Immunization_Percent, fill = Geography)) +
      geom_bar(stat = 'identity', position = 'dodge', color = 'black') +
      facet_grid(~Immunization_Status ~.) +
      labs(x= 'School Year', y = 'Percentage') +
      theme(axis.text = element_text(size=14),
            axis.title = element_text(size = 14),
            legend.text = element_text(size=14),
            strip.text = element_text(size=14))
  }, width = 1000, height = 600)

  
  # 2nd component: Standard Score
  # process map plots per year based on year filter defined in yeapMap function (line 26)
  output$myMap1 <- renderLeaflet({
    yearMap('2018-2019')
  })
  
  output$myMap2 <- renderLeaflet({
    yearMap('2019-2020')
  })
  
  output$myMap3 <- renderLeaflet({
    yearMap('2020-2021')
  })
  
  output$myMap4 <- renderLeaflet({
    yearMap('2021-2022')
  })
  
  output$myMap5 <- renderLeaflet({
    yearMap('2022-2023')
  })
}

shinyApp(ui = ui, server = server)

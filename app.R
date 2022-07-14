
library(shinydashboard)
library(shiny)
library(leaflet)
library(tidyverse)
library(DT)
library(plotly)
library(rworldmap)



travel_countrys <- c(1,2,3)

dsp = read_csv("co-emissions-per-capita.csv")
p2_1_country = dsp%>% distinct(Entity)

ui <- dashboardPage(
  
  skin = 'black',
  dashboardHeader(title =h1("Worldwide Carbon Emissions Explosion", 
                            style = 'font-size:16px;color:black;font-weight:bold;'),
                  titleWidth=320),
  
  dashboardSidebar(width=340,
    sidebarMenu(
      menuItem("Home", tabName = "page1", icon = icon("home")),
      menuItem("Data visualization", tabName = "page2", icon = icon("bar-chart"),
               menuSubItem("Global CO2 emissions",tabName = "page2_1",icon='●'),
               menuSubItem("Global CO2 emissions - Fossil fuels and Land use",tabName = "page2_2",icon='●'),
               menuSubItem("Global CO2 emissions - Climate Warming ",tabName = "page2_3",icon='●'),
               menuSubItem("Global CO2 emissions - Global Econ",tabName = "page2_4",icon='●')),
      menuItem("Dataset", tabName = "page3", icon = icon("database"))
    )
  
  ),
  body=dashboardBody(
    tabItems(
    tabItem(tabName="page1",
            h1(icon("youtube-play"),"NASA: A Year in the Life of Earth's CO2", style = 'font-size:24px;color:black;font-weight:bold;', align="center"),
            column(width = 12, HTML('<iframe width="660"height="415"src="https://www.youtube.com/embed/x1SgmFa0r04"frameborder="0" allowfullscreen></iframe>'), align = "center"),
            tags$hr(),
            tags$br(),
            fluidRow(
              box(
                title = strong(icon("envelope-open"),"Background"),
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                collapsible = TRUE,
                column(12, 
                       tags$div("Carbon dioxide emissions are the primary driver of global climate change. 
                                It's widely recognised that to avoid the worst impacts of climate change, 
                                the world needs to urgently reduce emissions. 
                                But, how this responsibility is shared between regions, countries, 
                                and individuals has been an endless point of contention in international discussions."),
                       style="font-size:14px"),
                br(),
                hr(),
                strong("Global carbon budget 2021", 
                       style = 'font-size:18px;color:black;font-weight:bold;', 
                       align="center"),
                p("The data below summarize all human-caused sources of CO2 emissions and global sinks (where the CO2 goes).  Numbers present the yearly average for one decade (2011 to 2020). "),
                tags$ul(
                  tags$li("Global CO2 emissions from human activity",
                          style = 'font-size:18px;color:"dark blue";font-weight:bold;')
                ),
                div(),
                column(6,
                       column(6,
                              img(src="1.jpg", width="100%")
                              ),
                       column(6,
                              strong("89%", style = 'font-size:28px;color:gray;font-weight:bold;'),
                       p(em("34.8 GtCO2/yr")),
                       strong("Fossil fuel emissions", style = 'font-size:20px;color:black;font-weight:bold;')
                              )
                ),
                column(6,
                       column(6,
                              img(src="2.jpg", width="100%")
                       ),
                       column(6,
                              strong("11%", style = 'font-size:28px;color:gray;font-weight:bold;'),
                              p(em("4.1 GtCO2/yr")),
                              strong("Emissions from land use change", style = 'font-size:20px;color:black;font-weight:bold;'),
                              p("(mostly deforestation)")
                       )
                       ),
                br(),
                hr(),
                tags$ul(
                  tags$li("Where the CO2 emissions go",
                          style = 'font-size:18px;color:"dark blue";font-weight:bold;')
                ),
                column(6,
                       column(6,
                              img(src="3.jpg", width="100%")
                              ),
                       column(6,
                              strong("48%", style = 'font-size:28px;color:gray;font-weight:bold;'),
                              p(em("18.6 GtCO2/yr")),
                              strong("Atmosphere", style = 'font-size:20px;color:black;font-weight:bold;')
                              )
                       ),
                column(6,
                       column(6,
                              img(src="4.jpg", width="100%")
                       ),
                       column(6,
                              strong("29%", style = 'font-size:28px;color:gray;font-weight:bold;'),
                              p(em("11.2 GtCO2/yr")),
                              strong("Vegetegation & Soils", style = 'font-size:20px;color:black;font-weight:bold;'),
                              p("(terrestrial biosphere)")
                       )
                       ),
                column(12,
                       column(3,
                              img(src="5.jpg", width="100%")
                              ),
                       column(3,
                              strong("26%", style = 'font-size:28px;color:gray;font-weight:bold;'),
                              p(em("10.2 GtCO2/yr")),
                              strong("Oceans", style = 'font-size:20px;color:black;font-weight:bold;'),
                              p("(terrestrial biosphere)")
                              )
                       ),
               
                br(),
                hr(),
                div(),
                tags$ul(
                  tags$li("Balancing the Budget",
                          style = 'font-size:18px;color:"dark blue";font-weight:bold;')
                ),
                column(12,
                       column(3,
                              img(src="6.jpg", width="100%")
                              ),
                       column(9,
                              p("The global carbon budget numbers above are the best available scientific 
                                determinations at the time they were reported.  Scientists also report an 
                                imbalance of 3% (-1.0 GtCO2/yr) between the estimates for global sources and sinks."),
                              strong("-3%", style = 'font-size:28px;color:gray;font-weight:bold;'),
                              p(em("-1.0 GtCO2/yr")),
                              strong("-1.0 GtCO2/yr", style = 'font-size:20px;color:black;font-weight:bold;'),
                              p("(all sinks vs. all sources)")
                              )
                )
            ),
              box(
                title = strong(icon("paw"),"Data"),
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                collapsible = TRUE,
                column(12,
                       tags$a("	https://doi.org/10.18160/gcp-2021"),
                       tags$p("This is our main dataset resource. It shows the carbon emission through 1750-2020 in different countries."),
                       style="font-size:14px"
                )
              ),
              box(
                title = strong(icon("heart"), "About this project"),
                solidHeader = TRUE,
                status = "info",
                width = 12,
                collapsible = TRUE,
                column(12,
                       tags$div("Our team up to explore how carbon emissions influenced the countries in the world. For example, we will visualize the relationship between 
                                carbon emission and the climate change, different quantities of carbon emission in the world through these years, find the relationship 
                                between economic development with carbon emission in countries, and also some special policies of carbon emission implemented in different 
                                countries, such as changes and impacts on electrical cars produced and used and so on. Visualization will help us explore more about carbon 
                                emissions and provide us with a clear blueprint of world development in the future."),
                       style="font-size:14px")
              ),
              box(
                title = strong(icon("address-book"), "Group member"),
                solidHeader = TRUE,
                status = "warning",
                width = 12,
                collapsible = TRUE,
                column(12,
                       tags$p(icon("user-circle-o"),"Tsun Yuen Wong"),
                       tags$p(icon("user-circle-o"),"Shuya Yang"),
                       tags$p(icon("user-circle-o"),"Yu Zhang"),
                       tags$p(icon("user-circle-o"),"Shanshan Qiao"),
                       tags$p(icon("user-circle-o"),"Shiying Luo"),
                       style="font-size:14px")
              )
            ),
   
),
    tabItem(tabName = "page2_1",
            titlePanel(h1(icon("globe"),"Global CO2 Emissions by Country & Region ",
                          style = 'font-size:24px;color:black;font-weight:bold;', 
                          align="center")),
            fluidRow(
              box(
                title = strong(icon("user-o"),"Per capita CO2 Emissions", style = 'font-size:18px;color:black;font-weight:bold;', align="center"),
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                status = "primary",
                column(8,
                  wellPanel(
                    sliderInput("p2_1_year", "Year:", min = 1800, max = 2020, value = 2020, 
                            step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
                    selectInput("p2_1_c", label = "Select Country/Region", 
                                choices = p2_1_country, 
                                selected = "Afghanistan",
                                multiple = FALSE,
                                width = "100%")
                    
                  ),
                  
                  plotOutput("map2_1", width="100%")
                ),
                column(4,
                       h3(icon("caret-up"),"Top 5 Countries with the highest Per capita CO2 Emissions",
                          style = 'font-size:16px;color:black', align="center"),
                       plotlyOutput("p2_1_bar")
                       )
                
              ),
              box(
                title = strong(icon("globe"),"Annual CO2 emissions", style = 'font-size:18px;color:black;font-weight:bold;', align="center"),
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                status = "primary",
                column(12,
                       h3(icon("location-arrow"),"Who emits the most CO2 each year?",
                          style = 'font-size:16px;color:black;font-weight:bold;', align="center"),
                       plotlyOutput("p2_1_area")
                       )
              )
             
            )
                       
            
      
    ),

    tabItem(tabName = "page2_2",
            titlePanel(h1(icon("globe"),"Sources of Global CO2 Emissions",
                          style = 'font-size:24px;color:black;font-weight:bold;', 
                          align="center")),
            fluidRow(
              box(
                title = "How have global emissions of carbon dioxide (CO2) from fossil fuels and land use changed over time?",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                status = "primary",
                navbarPage(
              inverse = FALSE,
              collapsible = TRUE,
              title = "Overview",
              tabPanel(icon = icon("line-chart"), title = "Trend",
                       h1("Global CO2 emissions from fossil fuels and land use change",style = 'font-size:24px;color:black;font-weight:bold;', align="center"),
                       sliderInput("p2_2_year", "Year:", min = 1850, max = 2020, value = 2020, 
                                   step = 1, animate = animationOptions(interval = 100, loop = FALSE)),
                       plotlyOutput("plot2_2_trend", width = "100%", height = "400px")),
              tabPanel(icon = icon("table"), title = "Data Overview",
                       dataTableOutput("table2_2_trend"))
            )
              ),
            box(
              title = "Annual CO??? emissions from fossil fuels by world region",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              status = "primary",
              navbarPage(
                inverse = FALSE,
                collapsible = TRUE,
                title = "Overview",
                tabPanel(icon = icon("area-chart"), title = "View by Chart",
                         sliderInput("p2_1_year", "Year:", min = 1750, max = 2020, value = 2020, 
                                     step = 1, animate = animationOptions(interval = 100, loop = FALSE)),
                         plotlyOutput("plot2_2_area")
                  
                ),
                tabPanel(icon = icon("table"),title = "Data Overview",
                         dataTableOutput("table2_2_area")
                  
                )
              )
            )
            )
            
            
      
    ),

    tabItem(tabName="page2_3",
            fluidRow(
            column(6,h4("plot1"),
                   title="Global map",
                   plotOutput("Global_warming map")),
            column(6,h4("plot2"),
                     tabPanel(
                       title="Glbal warming change by year",
                       plotOutput("Warming_by_Year")
              ))),
            fluidRow(h4("plot3"),
              tabPanel(
                title="temperature_vs_CO2",
                plotOutput("Temperature_Co2")
                  
                ))),
    
    
    tabItem(tabName="page2_4",
            fluidRow(
              box(title = "CO₂ emissions embedded in global trade",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                collapsible = TRUE,
                column(12,sliderInput("year", "Year:", min = 1990, max = 2019, value = 1990, 
                        step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
            leafletOutput("tradeMap")))),
            
             fluidRow(box(title = "Change in per capita CO₂ emissions and GDP",
                  solidHeader = TRUE,
                  status = "info",
                  width = 12,
                  collapsible = TRUE,
                  column(12, box(width = 12,column(width = 6,
                      selectInput( "countrys", "Pleace select a County ",
                        choices = travel_countrys,
                        width = "100%",
                        selected = "1"),
                         plotOutput("GDP") ))
            )))),
    tabItem(tabName = "page3",
            fluidRow(
              box(
                title = "Per capita CO2 emissions",
                solidHeader = TRUE,
                status = "info",
                width = 12,
                collapsible = TRUE,
                dataTableOutput("Per_capita_CO2_Emissions")
              ),
              box(
                title = "Annual CO2 emissions",
                solidHeader = TRUE,
                status = "info",
                width = 12,
                collapsible = TRUE,
                dataTableOutput("Annual_CO2_emissions")
              )
            )
      
    )

)))
  


server <- function(input, output) { 
  dsf = read_csv("global-co2-fossil-plus-land-use.csv")
  dsf1 = pivot_longer(dsf, c(4:6), names_to = "type", values_to = "emissions")
  output$plot2_2_trend = renderPlotly (
    {
      g = dsf1 %>%
        #filter(as.numeric(Year) == input$p2_2_year) %>%
        mutate(Label = if_else(Year == max(Year), as.character(type), NA_character_)) %>%
        
        ggplot(mapping = aes(x=Year, y=emissions, group=type, color=type))+
        geom_line(size=0.9)+
        labs(
          title = "Global CO2 emissions from fossil fuels and land use change",
          y="",
          x="Year"
        ) +
        theme_light()+
        geom_text(aes(label = Label))+
        theme(legend.position = "none")
        
      ggplotly(g)
    }
  )
  output$table2_2_trend = renderDataTable(
    return(datatable(dsf1, rownames= FALSE))
  )
  
  dsp1 = reactive({
    dsp %>% filter(Year == input$p2_1_year)
    
  })
  
  output$map2_1 = renderPlot({
    n <- joinCountryData2Map(dsp1(), joinCode="NAME", nameJoinColumn="Entity")
    mapCountryData(n, nameColumnToPlot="Annual CO2 emissions (per capita)", mapTitle="Per capita CO2 Emissions Worldwide",
                   mapRegion = "World",
                   colourPalette = c('beige','bisque', 'darkgoldenrod1','darkorange','darkorange2','darkorange3', 'darkred'),
                   missingCountryCol = "gray",
                   addLegend=TRUE)
  })

  }

shinyApp(ui, server)

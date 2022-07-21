
library(shinydashboard)
library(shiny)
library(leaflet)
library(tidyverse)
library(DT)
library(plotly)
library(rworldmap)
library(gghighlight)


travel_countrys <- c(1,2,3)

dsp = read_csv("co-emissions-per-capita.csv")
dsa = read_csv("annual-co2-emissions-per-country.csv")
dsc = read_csv("change-co2-annual-pct.csv")
dsfr = read_csv("annual-co-emissions-by-region.csv" )
p2_1_country = dsp%>% distinct(Entity)

ds2=read_csv("global_co2_temp.csv")
ds3=read_csv("temperature-anomaly.csv")
temp_country=ds3%>% distinct(Entity)

trade=read_csv("share-co2-embedded-in-trade.csv")
gdp=read_csv("co2-emissions-and-gdp-per-capita.csv")
p2_4_country = gdp%>% distinct(Entity)

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
      menuItem("Dataset", tabName = "page3", icon = icon("database")),
      menuItem("About us", tabName = "page4", icon = icon("phone-alt"))
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
                column(12,
                  wellPanel(
                    sliderInput("p2_1_map", "Year:", min = 1800, max = 2020, value = 2020, 
                            step = 1, animate = animationOptions(interval = 1000, loop = FALSE)),
                    selectInput("p2_1_c", label = "Select Country/Region", 
                                choices = p2_1_country, 
                                selected = "World",
                                multiple = FALSE,
                                width = "100%")
                    
                  ),
                ),
                column(8,
                       plotOutput("map2_1", width="100%")
                       ),
                
                column(4,
                       h3(icon("caret-up"),"Top 5 Countries with the highest Per capita CO2 Emissions",
                          style = 'font-size:16px;color:black', align="center"),
                       plotlyOutput("p2_1_bar")
                       )
                
              ),
              box(
                title = strong(icon("globe"),"Worldwide CO2 emissions", style = 'font-size:18px;color:black;font-weight:bold;', align="center"),
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                status = "primary",
                h3(icon("location-arrow"),"Who emits the most CO2 each year?",
                          style = 'font-size:16px;color:black;font-weight:bold;', align="center"),
                navbarPage(
                  "Overview",
                  tabPanel("By Region",
                           selectInput("p2_1_3", label = "Select Region", 
                                       choices = c("Default","Africa", "Asia", "North America","Europe", "South America", "Oceania"), 
                                       multiple = FALSE,
                                       width = "100%"),
                           sliderInput("p2_1_8", "Year:", min = 1750, max = 2020, value = 2020, 
                                       step = 10, animate = animationOptions(interval = 1000, loop = FALSE)),
                           plotlyOutput("p2_1_area")),
                  tabPanel("By Country", 
                           selectInput("p2_1_4", label = "Select Country", 
                                       choices = p2_1_country, 
                                       multiple = FALSE,
                                       selected = "United States",
                                       width = "100%"),
                           sliderInput("p2_1_9", "Year:", min = 1800, max = 2020, value = 2020, 
                                       step = 10, animate = animationOptions(interval = 1000, loop = FALSE)),
                           plotlyOutput("p2_1_country")),
                  tabPanel("Map",
                           sliderInput("p2_1_5", "Year:", min = 1800, max = 2020, value = 2020, 
                                       step = 1, animate = animationOptions(interval = 1000, loop = FALSE)),
                           selectInput("p2_1_6", label = "Select Country/Region", 
                                       choices = p2_1_country, 
                                       selected = "World",
                                       multiple = FALSE,
                                       width = "100%"),
                           plotOutput("map2_2", width="100%")
                           ),
                  tabPanel("Annual change in CO2 emissions",
                           selectInput("p2_1_7", label = "Select Country/Region", 
                                       choices = p2_1_country, 
                                       selected = "World",
                                       multiple = FALSE,
                                       width = "100%"),
                           plotlyOutput("p2_1_change")
                           )
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
                                   step = 10, animate = animationOptions(interval = 1000, loop = FALSE)),
                       plotlyOutput("plot2_2_trend", width = "100%", height = "400px")),
              tabPanel(icon = icon("table"), title = "Data Overview",
                       dataTableOutput("table2_2_trend"))
            )
              ),
            box(
              title = "Annual CO2 emissions from fossil fuels by world region",
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
                                     step = 10, animate = animationOptions(interval = 1000, loop = FALSE)),
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
            h2("Climate Warming Is Not A Legend Anymore",style = 'font-size:24px;color:black;font-weight:bold;', align="center"),
            div(img(src="sea_ice_animation_640-nc.gif", width="50%"),align="center"),
            fluidRow(
              box(
                title = "Co2 emission with Global Warming",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                status = "primary",
                plotlyOutput("scatter_plot")
                
              )),
            fluidRow(
              box(
                title = "Average Temperature Anomaly, Regional",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                collapsible = TRUE,
                column(12,
                       wellPanel(
                         
                         selectInput("region", label = "Select Region", 
                                     choices = temp_country, 
                                     selected = "Global",
                                     multiple = FALSE,
                                     width = "100%")
                         
                       )),
                column(12,plotlyOutput("line_temp", width="100%"))
              ))),
    
    
  tabItem(tabName="page2_4",
          fluidRow(
            box(title = "CO₂ emissions embedded in global trade",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                collapsible = TRUE,
                column(12,sliderInput("p2_4_year", "Year:", min = 1990, max = 2019, value = 1990, 
                                      step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
                       plotOutput("tradeMap")))),
          
          fluidRow(box(title = " GDP per capita and CO₂ emissions ",
                       solidHeader = TRUE,
                       status = "info",
                       width = 12,
                       collapsible = TRUE,
                       column(12, box(width = 12,column(width = 12,
                                                        selectInput( "p2_4_c", label = "Select Country/Region", 
                                                                     choices = p2_4_country, 
                                                                     selected = "United States",
                                                                     multiple = FALSE,
                                                                     width = "100%"),
                                                        plotlyOutput("GDP",  width = "100%", height = "400px") ))
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
              ),
              box(
                title = "Change_co2_annual_pct",
                solidHeader = TRUE,
                status = "info",
                width = 12,
                collapsible = TRUE,
                dataTableOutput("Change_co2_annual_pct")
              ),
              box(
                title = "Global_co2_temp",
                solidHeader = TRUE,
                status = "info",
                width = 12,
                collapsible = TRUE,
                dataTableOutput("Global_co2_temp")
              ),
              box(
                title = "Temperature_anomaly",
                solidHeader = TRUE,
                status = "info",
                width = 12,
                collapsible = TRUE,
                dataTableOutput("Temperature_anomaly")
              ),
              box(
                title = "Share_co2_embedded_in_trade",
                solidHeader = TRUE,
                status = "info",
                width = 12,
                collapsible = TRUE,
                dataTableOutput("Share_co2_embedded_in_trade")
              ),
              box(
                title = "Co2_emissions_and_gdp_per_capita",
                solidHeader = TRUE,
                status = "info",
                width = 12,
                collapsible = TRUE,
                dataTableOutput("Co2_emissions_and_gdp_per_capita")
              )
            )
      
    ),
    tabItem(tabName = "page4",
            fluidRow(
              box("Yu Zhang is currently majoring in Business Analytics and Risk Management at JHU for a master's degree. 
                  She graduated from Xi'an Jiaotong-Liverpool University majored in Financial Mathematics. 
                  She has experience in data analytics and product management.",
                  p(strong("a864158707@gmail.com")),
                  title = strong(icon("user-circle-o"),"Yu Zhang",style = 'font-size:20px;color:white;font-weight:bold;'),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 6,height = 180,
                  collapsible = TRUE
              ),
              box("Tsun Yuen Wong is a full time student in the BARM program at JHU. 
                  He graduated from UCI majoring in Business Economics. 
                  He is interested in creating a website that using datas to help with our decisions in real life.",
                  p(strong("b893204077@gmail.com")),
                  title = strong(icon("user-circle-o"),"Tsun Yuen Wong",style = 'font-size:20px;color:white;font-weight:bold;'),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 6,height = 180,
                  collapsible = TRUE
              ),
              box("Shuya Yang is currently at Johns Hopkins University 
                  finishing her Master's Degree in Business Analytics and Risk Management. 
                  With the interest in pursuing data analytics and business intelligence-related works, 
                  She is proficient with descriptive and predictive analytics, 
                  data modeling and architecture, data visualization and analytics.",
                  p(strong("yangshuya96@gmail.com")),
                  title = strong(icon("user-circle-o"),"Shuya Yang",style = 'font-size:20px;color:white;font-weight:bold;'),
                  solidHeader = TRUE,
                  status = "info",
                  width = 6,height = 180,
                  collapsible = TRUE
              ),
              box("Shanshan Qiao is currently studying for a master's degree in BARM at Johns Hopkins University. 
                  She studied finance at Indiana University Bloomington's Kelley Business School. 
                  Have relevant experience in investment banking IBD department and banking system risk management.",
                  p(strong("sqiao3@outlook.com")),
                  title = strong(icon("user-circle-o"),"Shanshan Qiao",style = 'font-size:20px;color:white;font-weight:bold;'),
                  solidHeader = TRUE,
                  status = "info",
                  width = 6,height = 180,
                  collapsible = TRUE
              ),
              box("Shiying Luo is studying for a master’s degree in BARM at Johns Hopkins University. 
                  She majored in Human Resources Management at an undergraduate university. 
                  She has once worked for the ByteDance technology company and has experience in business consulting. 
                  She is interested in data analytics and trying to learn more.",
                  p(strong("Luoshiying991@gmail.com")),
                  title = strong(icon("user-circle-o"),"Shiying Luo",style = 'font-size:20px;color:white;font-weight:bold;'),
                  solidHeader = TRUE,
                  status = "success",
                  width = 6,height = 180,
                  collapsible = TRUE
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
        filter(Year <= input$p2_2_year) %>%
        mutate(Label = if_else(Year == max(Year), as.character(type), NA_character_)) %>%
        ggplot(mapping = aes(x=Year, y=emissions, group=type, color=type))+
        geom_line(size=0.9)+
        xlim(1850,2020)+
        ylim(0, 4.5*10^10)+
        labs(
          title = "Global CO2 emissions from fossil fuels and land use change",
          y="CO2 Emissions (Billion)",
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
  
  output$plot2_2_area = renderPlotly({
    dsfr %>%
      filter(Year <= input$p2_1_year) %>%
      filter(Entity %in% c("Africa", "Asia", "North America","Europe", "South America", "Oceania", "China","India","United States")) %>%
      mutate(Label = if_else(Year == max(Year), as.character(Entity), NA_character_)) %>%
      ggplot(aes(x=Year, y=`Annual CO2 emissions (zero filled)`,fill=Entity, color=Entity)) +
      geom_area()+
      geom_text(aes(label = Label))
  })
  
  output$table2_2_area = renderDataTable(
    return(datatable(dsfr, rownames= FALSE))
  )
  
  dsp1 = reactive({
    dsp %>% filter(Year == input$p2_1_map)
    
  })
  
  output$map2_1 = renderPlot({
    n <- joinCountryData2Map(dsp1(), joinCode="NAME", nameJoinColumn="Entity")
    mapCountryData(n, nameColumnToPlot="Annual CO2 emissions (per capita)", mapTitle="Per capita CO2 Emissions Worldwide",
                   mapRegion = input$p2_1_c,
                   colourPalette = c('beige','bisque', 'darkgoldenrod1','darkorange','darkorange2','darkorange3', 'darkred'),
                   missingCountryCol = "gray",
                   addLegend=TRUE)
  })
  dsbg = reactive({
    dsp %>% 
      filter(Year == input$p2_1_year ) %>%
      arrange(desc(`Annual CO2 emissions (per capita)`)) %>%
      slice(1:5)
  })

  output$p2_1_bar = renderPlotly({
    ggplot(dsbg(), aes(x=reorder(Entity,`Annual CO2 emissions (per capita)`),
                       y=`Annual CO2 emissions (per capita)`, 
                       fill=`Annual CO2 emissions (per capita)`)) +
      geom_col() +
      coord_flip() +
      scale_fill_gradient(low='bisque', high='darkred')+
      theme(legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank()
            )
  })
  
  output$p2_1_area = renderPlotly({
    if(input$p2_1_3=="Default"){
      dsa %>% 
        filter(Year <= input$p2_1_8) %>%
        filter(Entity %in% c("Africa", "Asia", "North America","Europe", "South America", "Oceania")) %>%
        mutate(Label = if_else(Year == max(Year), as.character(Entity), NA_character_)) %>%
        ggplot(aes(x=Year, y=`Annual CO2 emissions`,fill=Entity, color=Entity)) +
        geom_area(alpha=0.8)+
        geom_text(aes(label = Label))
    } else{
      dsa %>% 
        filter(Year <= input$p2_1_8) %>%
        filter(Entity %in% c("Africa", "Asia", "North America","Europe", "South America", "Oceania")) %>%
        mutate(Label = if_else(Year == max(Year), as.character(Entity), NA_character_)) %>%
        ggplot(aes(x=Year, y=`Annual CO2 emissions`,fill=Entity, color=Entity)) +
        geom_area(alpha=0.8)+
        geom_text(aes(label = Label)) +
        gghighlight(Entity==input$p2_1_3)
    }
    
    
  })
  
  output$p2_1_country = renderPlotly({
    dsa %>%
      filter(Year <= input$p2_1_9) %>%
      filter(Entity==input$p2_1_4)%>%
      ggplot(aes(x=Year, y=`Annual CO2 emissions`,fill=Entity, color=Entity)) +
      geom_area(alpha=0.6)+
      geom_line(size=1,color="coral")+
      geom_point(size=1,fill="coral3")
  })
  dsa1 = reactive({
    dsa %>% filter(Year == input$p2_1_5)
  })
  output$map2_2 = renderPlot({
    n <- joinCountryData2Map(dsa1(), joinCode="NAME", nameJoinColumn="Entity")
    mapCountryData(n, nameColumnToPlot="Annual CO2 emissions", mapTitle="CO2 Emissions Worldwide",
                   mapRegion = input$p2_1_6,
                   colourPalette = c('beige','bisque', 'darkgoldenrod1','darkorange','brown1','brown3', 'darkred'),
                   missingCountryCol = "gray",
                   addLegend=TRUE)

})
  
  output$p2_1_change = renderPlotly({
    dsc %>%
      filter(Entity ==input$p2_1_7) %>%
      mutate(Label = if_else(Year == max(Year), as.character(Entity), NA_character_)) %>%
      ggplot(aes(x=Year, y=`Annual CO2 emissions growth (%)`, group=Entity)) +
      geom_line(color="deepskyblue4")+
      geom_point(size=1, color="cyan3")+
      geom_text(aes(label = Label))
  })
  
  output$Per_capita_CO2_Emissions = renderDataTable(
    return(datatable(dsp, rownames= FALSE))
  )
  output$Annual_CO2_emissions = renderDataTable(
    return(datatable(dsa, rownames= FALSE))
  )
  output$Change_co2_annual_pct = renderDataTable(
    return(datatable(dsc, rownames= FALSE))
  )
  output$Global_co2_temp = renderDataTable(
    return(datatable(ds2, rownames= FALSE))
  )
  output$Temperature_anomaly = renderDataTable(
    return(datatable(ds3, rownames= FALSE))
  )
  output$Share_co2_embedded_in_trade = renderDataTable(
    return(datatable(trade, rownames= FALSE))
  )
  output$Co2_emissions_and_gdp_per_capita = renderDataTable(
    return(datatable(gdp, rownames= FALSE))
  )
  
  output$scatter_plot<-renderPlotly({
    
    
    
    f=ggplot(ds2,mapping=aes(x=co2,y=temp_changes))+
      geom_point(mapping=aes(x=co2,y=temp_changes),color="orange")+
      geom_smooth(color="Grey")+
      labs(
        title = "Co2 Emission Up, Temperature Chanegs UP",
        y="Temperature Change(°C)",
        x="Co2 Emission(billion)") +
      theme(axis.text=element_text(size=5.5),
            axis.title=element_text(size=6))
    
    ggplotly(f)
  })
  
  output$line_temp<-renderPlotly({
    ds3 %>% 
      filter(Entity==input$region)%>%
      ggplot(mapping=aes(x=Year,y=Median,color=Entity))+
      geom_line(mapping=aes(x=Year,y=Median))+
      geom_point(color="red")+
      labs(
        title="Global temperature keep Increasing in these century",
        y="Temperature(°C)",
        x="Year"
      )+
      theme(axis.text=element_text(size=5.5),
            axis.title=element_text(size=6))
  })
  
  gdp1=gdp %>%
    mutate("GDP*10^-4$"= `GDP per capita, PPP (constant 2017 international $)` *10^-4)
  gdp1=rename(gdp1,"CO2 emissions"= `Annual CO2 emissions (per capita)` , "consumption-based CO2"=`Annual consumption-based CO2 emissions (per capita)`)
  gdp2=pivot_longer(gdp1,
                    c("consumption-based CO2", "CO2 emissions", "GDP*10^-4$"),
                    names_to = "type", values_to = "value")
  
  
  trade1 = reactive({
    trade %>% filter(Year == input$p2_4_year)
  })
  
  output$tradeMap = renderPlot({
    m <- joinCountryData2Map(trade1(), joinCode="NAME", nameJoinColumn="Entity")
    mapCountryData(m, nameColumnToPlot="Share of annual CO2 emissions embedded in trade", mapTitle="CO2 emissions embedded in global trade",
                   mapRegion = "World",
                   colourPalette = c('azure2','darkorchid1','darkorchid2','darkorchid3','darkmagenta','darkorchid4', 'darkslateblue'),
                   missingCountryCol = "gray",
                   addLegend=TRUE)
  })
  
  output$GDP = renderPlotly (
    {gdp3 = gdp2 %>% filter(Entity==input$p2_4_c)
    
    gg<-ggplot(data=gdp3,mapping = aes(x=Year, y=value, group=type, color=type))+
      geom_line(size=1)+
      theme_light()+
      labs(title="GDP per capita and CO2 emissions",
           y="GDP(*10^-4)$/ CO2 Emissions")
    
    
    ggplotly(gg) })

}


shinyApp(ui, server)

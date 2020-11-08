# please uncomment and install the required libraries if not installed already
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("leaflet")
# install.packages("dplyr")
# install.packages("plotly")
# install.packages("shinycssloaders")
# install.packages("shinyWidgets")
# install.packages("dygraphs")

# import the required libraries
library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(plotly)
library(shinycssloaders)
library(shinyWidgets)
library(dygraphs)

# import and pre-process the required data for designing the UI inputs for dropdowns
data <- read.csv("data/lat_lon.csv")
state <- unique(data$state)

# trigger the UI
shinyUI(fluidPage(
  
  # set the theme and background color for the application
  theme = shinytheme("flatly"),
  setBackgroundColor("ghostwhite"),
  
  # set up the UI with a navigation panel for separate tabs
  navbarPage(title = div(img(src = "logo.png", height = 48, width = 36), "Electricity Consumption in Australia"),
             
             # define and set the zeroeth/home tab
             tabPanel("Home",
                      sidebarPanel(
                        br(),
                        h2("Introduction"),
                        br(),
                        h4("Covid-19 has affected our lives in different ways. Electricity consumption is one of the major factors that has also been heavily affected.
                          Due to these unexpected fluctuations, energy-providing companies might have faced unprecedented scenarios where their traditional time-series forecasting models might not have performed up to the mark."),
                        br(),
                        h4("Earlier, we focused on Victoria and built a causal forecasting model to predict the electricity prices for the period of Covid-19.
                           It is time we perform a comparative analysis among the different states in Australia, based on the availability of the data."),
                        br(),
                        h4("Here, a story of Australia's electricity consumption will be conveyed through various visualisations explaining the multiple aspects impacting it, especially Covid-19.")
                      ),
                      mainPanel(
                        img(src='home1.jpg', align = "centre",height="100%", width="100%"),
                        h1("A Story through Visualisations", align = "center"),
                        h2("Abhilash Anil Kale", align = "center"),
                        HTML("<center>[PS - <em>The initial load might take upto 10 seconds for chapter 3. Thank you for being patient!</em>]</center>")
                      )
                      ),
             
             # define and set the first tab for chapter 1
             tabPanel("Chapter 1",
                      h1("The Tale of Electricity in Australia"),
                      fluidRow(column(2, selectInput("covid", "Choose a time period:", 
                                  c("After Covid-19" = "Yes", 
                                    "Before Covid-19" = "No")
                      )),
                      column(2, selectInput("electricity", "Choose from Electricity Demand or Price:", 
                                  c("Regional Reference Price ($/MWh)" = "Regional Reference Price",
                                    "Electricity Demand (MWh)" = "Electricity Demand")
                      ))),
                      
                      # set a sidebar panel with dropdown menu for electricity and a line graphs
                      sidebarPanel(
                        h4("Covid-19 has significantly lowered down the electricity prices!"),
                        h4("The demand does not seem to have decreased much after the pandemic."),
                        h3("Factors Affecting the Electricity"),
                        h4(htmlOutput("tab_1_caption3")),
                        br(),
                        h4(htmlOutput("tab_1_type_caption")),
                        plotlyOutput("tab_1_typeplot"),
                        br(),
                        h4(htmlOutput("tab_1_week_caption")),
                        plotlyOutput("tab_1_week")
                        ),
                      
                      # deploy the choropleth map and the rank graph on the main panel
                      mainPanel(leafletOutput("tab_1_map", height="800") %>% withSpinner(color="#3c4c6b"),
                                br(),
                                br(),
                                h3(textOutput("tab_1_caption")),
                                plotlyOutput("tab_1_plot"))
                      ),
             
             # define and set the second tab for chapter 2
             tabPanel("Chapter 2",
                      h1("A Comparative Analysis of other states with Victoria through a Year"),
                      h4(htmlOutput("tab_2_caption2")),
                      br(),
                      
                      # set the dropdown menus for the selection of electricity and state
                      selectInput("covid1", "Choose a time period:", 
                                  c("After Covid-19" = "Yes", 
                                    "Before Covid-19" = "No")),
                      fluidRow(column(6,
                              selectInput("electricity1", "Choose from Electricity Demand or Price:", 
                                          c("Electricity Demand (MWh)" = "Electricity Demand", 
                                            "Regional Reference Price ($/MWh)" = "Regional Reference Price")
                              )),
                              column(6,
                                     selectInput("state", "Choose a state:",
                                                 c("New South Wales" = "New South Wales",
                                                   "South Australia" = "South Australia",
                                                   "Queensland" = "Queensland",
                                                   "Tasmania" = "Tasmania",
                                                   "Victoria" = "Victoria")
                              ))),
                      
                      # deploy the area graphs for comparison on the main panel
                      mainPanel(h2(htmlOutput("tab_2_caption")),
                                plotlyOutput("tab_2", height = 500, width = 1600) %>% withSpinner(color="#3c4c6b"))),
             
             # define and set the third tab for chapter 3
             tabPanel("Chapter 3",
                      h1("The Rise and Fall of Electricity Prices and Demand from 2016 through 2020"),
                      
                      # set the dropdown menus for the selection of electricity and state
                      fluidRow(column(3,
                                      selectInput("electricity2", "Choose from Electricity Demand or Price:", 
                                                  c("Regional Reference Price ($/MWh)" = "RegionalReferencePrice",
                                                    "Electricity Demand (MWh)" = "ElectricityDemand")
                                      )),
                               (column(3,
                                       selectInput("state2", "Choose a state:",
                                                   state)
                               ))),
                      h4(HTML("From this trend, we can see a change towards downfall of the electricity prices, as we are facing the Covid-19 pandemic since March 2020.
                          <br/>This has led many energy companies to huge losses as their existing forecasting models did not provide the desired accurate results.")),
                      
                      # display a short help/guide to use the dygraph
                      p(htmlOutput("tab_4_caption", align = "right")),
                      
                      # deploy the dygraph plot on the main panel
                      mainPanel(dygraphOutput("tab_4", height = 520, width = 1600)  %>% withSpinner(color="#3c4c6b"))
                      ),
             
             # define and set the fourth tab for chapter 4
             tabPanel("Chapter 4",
                      h1("The Impact of Covid-19 on Australia's Electricity by comparing its Demand and Prices"),
                      
                      # set a sidebar panel with reactive text and conclusion text for the visualisations
                      sidebarPanel(
                        selectInput("covid3", "Choose a time period:", 
                                    c("After Covid-19" = "Yes", 
                                      "Before Covid-19" = "No")),
                        h2(htmlOutput("details")),
                        h4(htmlOutput("tab_3_caption3")),
                        br(),
                        br(),
                        h2(htmlOutput("conclusion")),
                        h4(htmlOutput("tab_3_caption2")),
                      ),
                      
                      # deploy the choropleth + symbol proportional map on the main panel
                      mainPanel(leafletOutput("tab_3_map", height=800) %>% withSpinner(color="#3c4c6b"))
                      ),
             
             # display the sources of data used for the visualisations
             tabPanel("Data",
                      br(),
                      h2("Data retrieved from:"),
                      br(),
                      h4("1. AEMO - Electricity Demand and Price"),
                      tags$head(tags$style(HTML("a {color: blue}"))),
                      p(tags$a(href="https://aemo.com.au/energy-systems/electricity/national-electricity-market-nem/data-nem/aggregated-data",
                               "Aggregated price and demand data -- aemo.com.au")),
                      br(),
                      h4("2. AEMO - MRIM Meter Energy in Victoria"),
                      p(tags$a(href="https://aemo.com.au/energy-systems/electricity/national-electricity-market-nem/data-nem/metering-data/victorian-mrim-meter-data",
                               "Victorian MRIM meter data -- aemo.com.au")),
                      br(),
                      h4("3. Australian States Map"),
                      p(tags$a(href="https://github.com/rowanhogan/australian-states/",
                               "GEOJSON file for Australia's states -- https://github.com/rowanhogan/")),
                      br(),
                      HTML("<em>P.S. -<br>More data sources were used while training the forecasting model.<br>
                          Although, the major visual insights were observed from the above mentioned data sources.</em>")
                      )
             )
  
))
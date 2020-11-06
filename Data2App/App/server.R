# please uncomment and install the required libraries if not installed already
# install.packages("shiny")
# install.packages("sf")
# install.packages("leaflet")
# install.packages("RColorBrewer")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("plotly")
# install.packages("dygraphs")

# import the required libraries
library(shiny)
library(sf)
library(leaflet)
library(RColorBrewer)
library(scales)
library(ggplot2)
library(dplyr)
library(plotly)
library(dygraphs)

# import and pre-process the required data for the respective visualisations
# the data has been pre-aggregated and stored in separate csv files as per the requirement
tab1 <- read.csv("data/tab_1_map.csv")
tab1$Time <- format(tab1$Time, format = "%H:%M:%S")
colnames(tab1)[1] <- "Covid"

tab2 <- read.csv("data/tab_2.csv")
colnames(tab2)[1] <- "Covid"
tab2_2 <- tab2[tab2$State == "Victoria", ]

tab3 <- read.csv("data/tab_3.csv")

states <-  read_sf("https://raw.githubusercontent.com/rowanhogan/australian-states/master/states.geojson")
tab1_map <- read.csv("data/tab_1.csv")
colnames(tab1_map)[1] <- "Covid"
lat_lon <- read.csv("data/lat_lon.csv")
tab1_map <- tab1_map %>%
  dplyr::inner_join(lat_lon, by=c("State" = "state"))

tab4_map <- read.csv("data/tab_4.csv")

mrim <- read.csv("data/vic_mrim.csv")


shinyServer(function(input, output, session) {

  # generate a choropleth map of Australia
  output$tab_1_map <- renderLeaflet({
    
    # process the data based on the input selected by the user
    tab1_map <- tab1_map[tab1_map$Covid == input$covid, ]
    tab1_map <- tab1_map[tab1_map$Electricity == input$electricity, ]
    
    # Join to count data
    data <- states %>%
      dplyr::inner_join(tab1_map, by=c("STATE_NAME" = "State"))
    
    # Specify choropleth colors
    if(data$Electricity[1] == "Electricity Demand") {
      mypalette <- colorNumeric("Reds", domain = c(0,9000))
    }
    else if(data$Electricity[1] == "Regional Reference Price") {
      mypalette <- colorNumeric("Greens", domain = c(0,100))
    }
    
    # set a label for the hover-on tooltips on the map
    mytext <- paste(
      "State: ", data$STATE_NAME,"<br/>", 
      input$electricity, ": ", round(data$Value, 2),
      sep="") %>%
      lapply(htmltools::HTML)
    
    # visualise the map in shiny using leaflet using the appropriate conditions
    leaflet(data) %>%
      addTiles() %>%
      addPolygons(fillColor = ~mypalette(Value),
                  stroke=TRUE,
                  fillOpacity = 1,
                  color="black",
                  weight = 1,
                  label = mytext,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "10px"),
                    textsize = "18px",
                    direction = "auto"),
                  highlight = highlightOptions(weight = 5, color = "black",
                                         bringToFront = TRUE),
                  layerId = ~STATE_NAME # extracting out the id of the state to be clicked
                  ) %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      addLegend( pal=mypalette, values=~Value, opacity=0.9, title = input$electricity, position = "bottomleft" )

  })
  
  # set up click observe event on the map to add more interactivity
  observe({
    click = input$tab_1_map_shape_click
    
    if(is.null(click))
      return() # return nothing if the map is not clicked
    else
      leafletProxy("tab_1_map") %>%
      setView(lng = click$lng, lat = click$lat, zoom = 5) # set the view on the state which is clicked and zoom in
  })
  
  # set up click observe event on the map to add more interactivity
  observe({
    click = input$tab_1_map_shape_click
    
    # check for the state which is clicked and subset the required data out
    sub = tab1[tab1$State == click$id, c("State", "Value", "Season", "Electricity", "Time", "Covid")]
    sub2 = tab1[tab1$State == click$id, c("State", "Value", "WeekTime", "Electricity", "Time", "Covid")]
    
    state = unique(sub$State)
    
    if(is.null(click))
      output$tab_1_caption3 <- renderUI({
        HTML(paste("<em>Hover & click on a state to know more!</em>")) # display the text if no state is clicked
      })
    else {
      
      # display a line graph aggregated for a certain state, for seasons, using ggplot + plotly
      output$tab_1_typeplot <- renderPlotly({
        
        output$tab_1_caption3 <- renderUI({
          HTML(paste(input$electricity, "in ", state))
        })
        
        output$tab_1_type_caption <- renderUI({
          HTML(paste("Winters have notably high demand of electricity"))
        })
        
        tab_1_type <- sub[sub$Electricity == input$electricity, ]
        tab_1_type <- tab_1_type[tab_1_type$Covid == input$covid, ]
        
        plot1_data <- aggregate(tab_1_type$Value, list(tab_1_type$Time, tab_1_type$Season, tab_1_type$Electricity), mean)
        colnames(plot1_data)[1] <- "Time"
        colnames(plot1_data)[2] <- "Season"
        colnames(plot1_data)[3] <- "Electricity"
        colnames(plot1_data)[4] <- "Value"
        
        plot1_data$Time <- as.POSIXct(strptime(plot1_data$Time, format="%H:%M:%S"))
        
        # visualise the bar graph using ggplot + plotly
        p <- plot1_data %>%
          ggplot( aes(x=Time, y=Value, group=Season, color=Season,
                      text = paste("Season:", Season, "<br>", Electricity, ":", round(Value, 2)))) +
          geom_line() +
          xlab("Time of day") +
          scale_x_datetime(date_breaks = "4 hour",
                           date_labels = "%H:%M")
        
        if(tab_1_type$Electricity[1] == "Electricity Demand") {
          p <- p + scale_y_continuous(limits = c(0, 11000)) +
            ggtitle("Electricity Demand based on seasons") +
            ylab("Electricity Demand (MWh)")
        }
        
        else if(tab_1_type$Electricity[1] == "Regional Reference Price") {
          p <- p + scale_y_continuous(limits = c(-50, 350)) +
            ggtitle("Electricity Prices based on seasons") +
            ylab("Regional Reference Prices ($/MWh)")
        }
          
        
        if(tab_1_type$Covid[1] == "No") {
          p <- p + scale_color_manual(values=c("red", "blue", "orange", "darkgreen"))
        }
        
        else if(tab_1_type$Covid[1] == "Yes") {
          p <- p + scale_color_manual(values=c("red", "blue", "darkgreen"))
        }
        
        ggplotly(p, tooltip = "text")
        
      })
      
      # display a line graph aggregated for a certain state, for week time, using ggplot + plotly
      output$tab_1_week <- renderPlotly({

        output$tab_1_week_caption <- renderUI({
          paste("Weekdays have significantly high demand of electricity")
        })

        tab_1_type1 <- sub2[sub2$Electricity == input$electricity, ]
        tab_1_type1 <- tab_1_type1[tab_1_type1$Covid == input$covid, ]
        
        plot2_data <- aggregate(tab_1_type1$Value, list(tab_1_type1$Time, tab_1_type1$WeekTime, tab_1_type1$Electricity), mean)
        colnames(plot2_data)[1] <- "Time"
        colnames(plot2_data)[2] <- "WeekTime"
        colnames(plot2_data)[3] <- "Electricity"
        colnames(plot2_data)[4] <- "Value"
        
        plot2_data$Time <- as.POSIXct(strptime(plot2_data$Time, format="%H:%M:%S"))

        # visualise the bar graph using ggplot + plotly
        p <- plot2_data %>%
          ggplot( aes(x=Time, y=Value, group=WeekTime, color=WeekTime,
                      text = paste("Week Time:", WeekTime, "<br>", Electricity, ":", round(Value, 2)))) +
          geom_line() +
          xlab("Time of day") +
          scale_x_datetime(date_breaks = "4 hour",
                           date_labels = "%H:%M") +
          scale_color_manual(values=c("red", "blue"))
        
        if(tab_1_type1$Electricity[1] == "Electricity Demand") {
          p <- p + scale_y_continuous(limits = c(0, 11000)) +
            ggtitle("Electricity Demand based on time of week") +
            ylab("Electricity Demand (MWh)")
        }
        
        else if(tab_1_type1$Electricity[1] == "Regional Reference Price") {
          p <- p + scale_y_continuous(limits = c(-50, 350)) +
            ggtitle("Electricity Prices based on time of week") +
            ylab("Regional Reference Prices ($/MWh)")
        }

        ggplotly(p, tooltip = "text")

      })
      
    }
    })
  
  # generate a bar graph to show the ranks on the first tab
  output$tab_1_plot <- renderPlotly({
    
    output$tab_1_caption <- renderText({
      paste("Distribution of MRIM Meter Energy based on Profile Areas in Victoria before Covid-19")
    })
    
    # visualise the the bar graph using ggplot + plotly
    p <- ggplot(mrim, aes(Value,
                           reorder(PROFILEAREA, Value),
                           text = paste("Profile Area:", PROFILEAREA, "<br>MRIM Meter Energy:", round(Value, 2)))) +
      geom_col(fill = "#006aa5") +
      theme(axis.text.x = element_text(size = 9)) +
      theme(axis.text.y = element_text(size = 9)) +
      xlab("MRIM Meter Energy") +
      ylab("Profile Area in Victoria") +
      scale_x_continuous(labels = scales::comma)
    
    ggplotly(p, tooltip = "text")
  })
  
  # generate the comparative area charts on the second tab
  output$tab_2 <- renderPlotly({
    
    # process the data as per the user selected input data
    tab2 <- tab2[tab2$Covid == input$covid1, ]
    tab2 <- tab2[tab2$Electricity == input$electricity1, ]
    tab2 <- tab2[tab2$State == input$state, ]
    
    tab2_2 <- tab2_2[tab2_2$Covid == input$covid1, ]
    tab2_2 <- tab2_2[tab2_2$Electricity == input$electricity1, ]

    tab2$yearmonth <- as.Date(paste0("2018-", tab2$Month, "-1"))
    tab2_2$yearmonth <- as.Date(paste0("2018-", tab2_2$Month, "-1"))

    # visualise the 1st area chart of the comparison using ggplot + plotly
    a <- ggplot(tab2,
                aes(x=yearmonth,
                    y=Value,
                    group = 1,
                    text = paste(Electricity, ":", round(Value, 2)))) +
      geom_area( fill="#69b3a2", alpha=0.4) +
      geom_line(color="black") +
      geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
      scale_x_date(labels = date_format("%b"), breaks = tab2$yearmonth) +
      xlab("Months of year")
    
    if(tab2$Electricity[1] == "Electricity Demand") {
      a <- a + scale_y_continuous(limits = c(0, 9000)) +
        ylab("Electricity Demand (MWh)")
    }
    
    else if(tab2$Electricity[1] == "Regional Reference Price") {
      a <- a + scale_y_continuous(limits = c(0, 150)) +
        ylab("Regional Reference Price ($/MWh)")
    }
    
    # visualise the 2nd area chart of the comparison using ggplot + plotly
    b <- ggplot(tab2_2,
                aes(x=yearmonth,
                    y=Value,
                    group = 1,
                    text = paste(Electricity, ":", round(Value, 2)))) +
      geom_area( fill="#ff7d7d", alpha=0.4) +
      geom_line(color="black") +
      geom_point(shape=21, color="black", fill="#ff7d7d", size=4) +
      scale_x_date(labels = date_format("%b"), breaks = tab2_2$yearmonth) +
      xlab("Months of year")
    
    if(tab2_2$Electricity[1] == "Electricity Demand") {
      b <- b + scale_y_continuous(limits = c(0, 9000)) +
        ylab("Electricity Demand (MWh)")
    }
    
    else if(tab2_2$Electricity[1] == "Regional Reference Price") {
      b <- b + scale_y_continuous(limits = c(0, 150)) +
        ylab("Regional Reference Price ($/MWh)")
    }
    
    a <- ggplotly(a, tooltip = "text")
    b <- ggplotly(b, tooltip = "text")
    
    # display the 2 area charts as a subplot for comparison
    subplot(b, a, margin = 0.04)
    
  })
  
  
  # generate text for display discussing about the plots
  output$tab_2_caption2 <- renderUI({
    
    HTML(paste("From the previous chapter, we learnt that New South Wales has the maximum demand of electricity in Australia.",
               "Although Victoria has had the longest Covid-19 lockdown in Australia, their electricity demand has been quite average compared to the other states.",
               sep = "<br/>"))
    
  })
  
  output$tab_2_caption <- renderUI({
    HTML(paste(p("Victoria", HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),
                 "VS", HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),HTML("&emsp;"),
                 input$state)))
  })
  
  
  # generate instructions for the dygraph
  output$tab_4_caption <- renderUI({
    
    HTML(paste("<em>Hover on the graph to view its details on the top-right corner of the graph.<br>
               Use the seekbar under the graph to drill down in time to gain more insights.<br>
               You can also click-and-drag on the desired time period to drill down in time, on the trend graph directly.</em>"))
  })
  
  # generate the dygraph for time series of the electricity prices and demand for each state
  output$tab_4 <- renderDygraph({
    
    # process the data as per the user selected input data
    tab3 <- tab3[tab3$State == input$state2, ]
    tab3$SETTLEMENTDATE <- strptime(x = as.character(tab3$SETTLEMENTDATE),
                                    format = "%d-%m-%Y %H:%M")
    tab3 <- tab3[!(is.na(tab3$SETTLEMENTDATE)), ]
    # tab3$SETTLEMENTDATE <-as.Date(tab3$SETTLEMENTDATE,'%d-%m-%Y')
    
    # convert the data to an xts format for the plot
    don <- xts::as.xts(x = tab3[[input$electricity2]], order.by = tab3$SETTLEMENTDATE)
    
    # visualise the dygraph for the time series of the electricity prices and demand using dygraph
    p <- dygraph(don) %>%
      dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 6, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
      dyRoller(rollPeriod = 1)

    p

  })
  
  # generate the choropleth + symbol proportional map for the electricity demand with respect to electricity prices of states
  output$tab_3_map <- renderLeaflet({
    
    # process the data based on the input selected by the user
    tab4_map <- tab4_map[tab4_map$Covid == input$covid3, ]
    
    # Join to count data
    data <- states %>%
      dplyr::inner_join(tab4_map, by=c("STATE_NAME" = "State"))
    
    mypalette <- colorNumeric("Reds", domain = c(0,9000))
    
    # set a label for the hover-on tooltips on the map for shaped states
    mytext1 <- paste(
      "State: ", data$STATE_NAME,"<br/>", 
      "Electricity Demand: ", round(data$ElectricityDemand, 2),"<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    # set a label for the hover-on tooltips on the map for markers
    mytext2 <- paste(
      "State: ", data$STATE_NAME,"<br/>", 
      "Electricity Demand: ", round(data$ElectricityDemand, 2),"<br/>",
      "Regional Reference Price: ", round(data$RegionalReferencePrice, 2),
      sep="") %>%
      lapply(htmltools::HTML)
    
    # visualise the choropleth + symbol proportional map using leaflet
    leaflet(data) %>%
      addTiles() %>%
      addPolygons(fillColor = ~mypalette(ElectricityDemand),
                  stroke=TRUE, 
                  fillOpacity = 1, 
                  color="black", 
                  weight = 1,
                  label = mytext1,
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "18px", 
                    direction = "auto"),
                  highlight = highlightOptions(weight = 5, color = "black",
                                               bringToFront = FALSE),
                  layerId = ~STATE_NAME # extracting out the id of the state to be clicked
      ) %>%
      addMarkers(~lon, ~lat, icon = 
                   makeIcon(
                     iconUrl = "dollar_hd.png",
                     iconWidth = ~RegionalReferencePrice - 20,
                     iconHeight = ~RegionalReferencePrice - 20
                   ),
                 label = mytext2,
                 labelOptions = labelOptions( 
                   style = list("font-weight" = "normal", padding = "3px 8px"), 
                   textsize = "18px", 
                   direction = "auto"),
                 layerId = ~STATE_NAME # extracting out the id of the state to be clicked
                 ) %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      addLegend( pal=mypalette, values=~ElectricityDemand, opacity=0.9, title = "Electricity Demand (MWh)", position = "bottomleft" ) %>%
      addControl(html = "<img src= 'dollar.png'/><br/>Size of the dollar sign is<br/>proportional to the state's electricity's<br/>Regional Reference Price ($/MWh)", position = "bottomright")

  })
  
  # set up click observe event on the map to add more interactivity (for the shapes)
  observe({
    click = input$tab_3_map_shape_click
    
    if(is.null(click))
      return() # return nothing if the map is not clicked
    else
      leafletProxy("tab_3_map") %>%
      setView(lng = click$lng, lat = click$lat, zoom = 5) # set the view on the state which is clicked and zoom in
  })
  
  # set up click observe event on the map to add more interactivity (for the shapes)
  observe({
    click = input$tab_3_map_shape_click
    tab4_map <- tab4_map[tab4_map$Covid == input$covid3, ]
    sub = tab4_map[tab4_map$State == click$id, c("State", "ElectricityDemand", "RegionalReferencePrice")] # check for the state which is clicked and subset the required data out
    state = sub$State
    demand = sub$ElectricityDemand
    price = sub$RegionalReferencePrice
    
    if(is.null(click))
      output$tab_3_caption3 <- renderUI({
        HTML(paste("<em>Hover & click on a state or a car marker to know more!</em>")) # display text if no state is selected
      })
    else
      output$tab_3_caption3 <- renderUI({
        
        # display the reactive text whenever a state is selected showing respective details
        HTML(paste("State:", state, "<br/>",
                   "Electricity Demand (MWh):", round(demand, 2), "<br/>",
                   "Regional Reference Price ($/MWh):", round(price, 2)))
        
      })
  })
  
  # set up click observe event on the map to add more interactivity (for the markers)
  observe({
    click = input$tab_3_map_marker_click
    tab4_map <- tab4_map[tab4_map$Covid == input$covid3, ]
    sub = tab4_map[tab4_map$State == click$id, c("State", "ElectricityDemand", "RegionalReferencePrice")] # check for the state which is clicked and subset the required data out
    state = sub$State
    demand = sub$ElectricityDemand
    price = sub$RegionalReferencePrice
    
    if(is.null(click))
      output$tab_3_caption3 <- renderUI({
        HTML(paste("<em>Hover & click on a state or a dollar sign to know more!</em>")) # display text if no state is selected
      })
    else
      output$tab_3_caption3 <- renderUI({
        
        # display the reactive text whenever a state is selected showing respective details
        HTML(paste("State:", state, "<br/>",
                   "Electricity Demand (MWh):", round(demand, 2), "<br/>",
                   "Regional Reference Price ($/MWh):", round(price, 2)))
        
      })
  })
  
  output$details <- renderUI({
    
    HTML(paste("Details:"))
    
  })
  
  # set up click observe event on the map to add more interactivity (for the markers)
  observe({
    click = input$tab_3_map_marker_click
    
    if(is.null(click))
      return() # return nothing if no state is clicked
    else
      leafletProxy("tab_3_map") %>%
      setView(lng = click$lng, lat = click$lat, zoom = 5) # set the view on the state which is clicked and zoom in
  })
  
  # set up the required text as conclusion from the visualisations for display on the tab
  output$conclusion <- renderUI({
    
    HTML(paste("Conclusion"))
    
  })
  
  output$tab_3_caption2 <- renderUI({
    
    HTML(paste("<li>As the visualisations tell us, Australia's electricity prices have been significantly affected since the Covid-19 pandemic, even though its demand has not been changed by much.</li>",
               "<li>Summer season and weekdays are clear winners among the factors affecting the demand and prices of electricity.</li>",
               "<li>West and East Victoria have high MRIM energy readings when drilled down for the state of Victoria.</li>",
               sep = "<br/>"))
    
  })

})
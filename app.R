rm(list=ls())
library(shiny)
library(jsonlite)
library(curl)
library(ggplot2)
library(shinythemes)

# Get buoy data from GLOS, exclude buoys without data in last 96 hours
buoys <- fromJSON('http://data.glos.us/glos_obs/platform.glos?tid=15')
buoys <- buoys[order(buoys$shortName),]
buoys$lastDataUpdate <- as.POSIXct(buoys$lastDataUpdate, tz='America/New_York')  # Assume all times EST, may not be true.
buoys <- buoys[(abs(difftime(buoys$lastDataUpdate, Sys.time(), units='hours')) < 24*365), ]

# Render web page
ui <- fluidPage(
  theme = shinytheme('spacelab'),
  titlePanel('GLOS Data Quality Visualizer'),
  sidebarLayout(
    sidebarPanel(  
      tabsetPanel(
        tabPanel('Introduction', id='tabIntro',
                 'This tool is designed to help users verify the quality of data from the Great Lakes 
                 Observing System (GLOS) ',
                 tags$a('Data Portal.',href='http://portal.glos.us/'), br(), br(),
                 'The GLOS Data Portal provides access to near-realtime and archived observations
                  from buoys and monitoring stations from GLOS and partners. Available data from stations and buoys include 
                 measurements related to wind, waves, temperature, and water quality. These data are used by communities, 
                 recreational users, and resource managers throughout the Great Lakes region.', br(), br(),
                 'However, with many different partners streaming data in near-realtime from many different stations with many different sensors, it is important to consider data quality concerns. Automatic sensors are not perfect and the measurements can sometimes be invalidated by anomalous local conditions such as passing seaweed. Users relying on GLOS data need an easy way to assess and correct for potential data quality errors. Enter the GLOS Data Quality Visualizer.', br(), br(),
                 'The Data Quality Visualizer allows users to choose a buoy, measurement parameter, and time frame. The data is visualized along with data errors in red (such as negative concentrations) and other potential data quality concerns in blue (such as fast rates of change). The clean dataset with errors removed is visualized separately. Additionally, the user can zoom in to explore data trends. Both the full dataset and the cleaned dataset are downloadable. The tool allows GLOS users to explore the different types of data quality errors and concerns in their data and to clean the data for use in further analyses.'
                 ), 
        tabPanel('Instructions', id='tabInstr',
                 'In order to use the tool, first use the drop-down menus in the “Data Selection” panel to select a monitoring buoy or station and a parameter of interest. Then select a time-frame: the last 48 hours, week, month, or year. You can also use the check-boxes to choose which types of data errors to visualize (in red) and omit from the dataset, and which types of quality concerns to visualize (in blue) but not remove.', br(), br(),
                 'You will see the data visualized in three plots. The upper plot shows the full dataset. The user can draw a box in this master plot by clicking and dragging. The second and third plots will automatically zoom to the extent of the box. By clicking again in the first plot, the box and zooms are reset. The second plot is simply a zoomable version of the full dataset. The third plot displays the cleaned dataset, from which suspected data errors have been removed.', br(), br(),
                 'Both the full and clean datasets can be downloaded using the buttons at the bottom of the “Data Selection” panel.'),
        tabPanel('Data Selection', id='tabData',
          uiOutput('bouyC_out'),  # First selection, buoy
          uiOutput('sensC_out'),  # Second selection, sensors on buoy
          selectInput('timeSpan', label='Time span',
                      choices = list('Last 48 hours', 'Last 7 days', 'Last 30 days', 'Last 365 days'),
                      selected=1),  # Third selection, time frame for data (always backward from now)
          checkboxGroupInput('cleanOpt', 'Values to omit', 
                             choices = c('Impossible coordinates'='coordFail', 'Unlikely coordinate movement'='coordSusp', 
                                         'Impossible measurement value'='valFail', 'Unlikely measurement value'='valSusp',
                                         'Measurement spike'='spikeSusp', 'Flatline'='flatSusp'),
                             selected = c('coordFail', 'coordSusp', 
                                          'valFail', 'valSusp',
                                          'spikeSusp', 'flatSusp')),
          checkboxGroupInput('cleanVis', 'Additional values to visualize', 
                             choices = c('Large rate of change'='rtChange', 'Large time gap'='timeGap'),
                             selected=c('rtChange', 'timeGap')),
          downloadButton('dlFull', 'Download Full'),  # Download full dataset (same as GLOS)
          downloadButton('dlClean', 'Download Clean')  # Download clean dataset)
        ), selected='Data Selection'
      )
    ),
    
    mainPanel(
      textOutput('txt'),  # Output text (only if no data)
      h4('Full Dataset, Master'),
#      textOutput('txtNoData'),  # Output text before data selection
      plotOutput('pltFull1',   # Output plot full dataset (only if data)
                 brush = brushOpts(id = 'pltFull1_brush', resetOnNew = T)),
      h4('Full Dataset, Zoom'),
#      textOutput('txtNoData2'),  # Output text before data selection
      plotOutput('pltFull2'),  # Zoomable version pltFull1
      h4('Clean Dataset, Zoom'),
#      textOutput('txtNoData3'),  # Output text before data selection
      textOutput('txtNoClean'),  # Output text (only if no omitted data)
      plotOutput('pltClean2')  # Zoomable output plot clean dataset (only if data omissions)

    )
  )
)

# Processing for server
server <- function(input, output){
  
  # Render selection box
  output$bouyC_out <- renderUI({
    selectInput('buoyC', label='Buoy or Station', 
              choices = buoys$shortName, selected=1)
  })
  
  # ID num for selected buoy
  buoyID <- reactive({
    buoys$id[which(buoys$shortName == input$buoyC)]
  })
  
  # Get sensor data from GLOS
  sens <- reactive({
      fromJSON(paste('http://data.glos.us/glos_obs/sensor.glos?pid=',
                           buoyID(), sep=''))
  })
  
  # Add order to thermistor/depth sensor names to maintain uniqueness
  sens_names <- reactive({
    ifelse(sens()$order != 0, 
           paste(sens()$measureType$obsTypeName, sens()$order, sep='_'),
           sens()$measureType$obsTypeName)
  })
  
  # Render 2nd selection box
  output$sensC_out <- renderUI({
    selectInput('sensC', label='Parameter',
                choices = sens_names(), selected=1)
  })
  
  # ID num for selected sensor
  sensID <- reactive({
    sens()$id[which(sens_names()==input$sensC)]
  })
  
  # Sensor name with units
  sensUnits <- reactive({
    paste(sens()$measureType$obsTypeName[which(sens_names()==input$sensC)], ' (', 
          sens()$measureType$uomDisplay[which(sens_names()==input$sensC)],
          ')', sep='')
  })
  
  # Number of hours to fetch, from selection 3
  timeH <- reactive({
    switch(input$timeSpan, 'Last 48 hours'=48, 'Last 7 days'=24*7,
           'Last 30 days'=24*30, 'Last 365 days'=24*365)
  })
  
  # Get measurement data from GLOS
  meas <- reactive({
    fromJSON(paste('http://data.glos.us/glos_obs/obs.glos?sids=',
                         sensID(), '&pt=15&pid=',
                         buoyID(), '&hours=', timeH(), sep=''))[[1]]
  })
  
  # Date-time formatting
  meas.dateTime <- reactive({
    as.POSIXct(meas()$dateTime)
  })
  
  # Output text if no data for selection.
  output$txt <- renderText({
    if(length(input$buoyC != 0)){
      if(length(meas()$value) == 0){
       'No data for this sensor and time frame.'
      }
    }
    
  })

  # Output text if no data for selection.
  output$txtNoClean <- renderText({
    if(length(input$buoyC) != 0){
      if(length(flgs_omit()) == 0){
        'No data omitted. Full dataset appears clean.'
      }
    }
  })
  
  # Output text if no data for selection.
  output$txtNoData <- renderText({
    if(length(input$buoyC) == 0){
      'Please select data.'
    }
  })
  
  # Output text if no data for selection.
  output$txtNoData2 <- renderText({
    if(length(input$buoyC) == 0){
      'Please select data.'
    }
  })
  
  # Output text if no data for selection.
  output$txtNoData3 <- renderText({
    if(length(input$buoyC) == 0){
      'Please select data.'
    }
  })
  
  # Function for testing whether measurement outside of physical limits.
  test_grossRange_fail <- function(){
    if (substr(input$sensC, 1, length('Thermistor String')) == 'Thermistor String'){
      sensN <- 'Thermistor String'
    } else if (substr(input$sensC, 1, length('Depth in Water')) == 'Depth in Water'){
      sensN <- 'Depth in Water'
    } else {
      sensN <- input$sensC
    }
    extrVal <- switch(sensN,
                      'Wind from Direction' = c(0, 360),
                      'Significant Wave Period' = c(0, Inf),
                      'Wind Speed' = c(0, Inf),
                      'Significant Wave Height' = c(0, Inf),
                      'Air Temperature' = c(-Inf, Inf),
                      'Air Pressure' = c(0, Inf),
                      'Thermistor String' = c(-Inf, Inf),
                      'Depth in Water' = c(0, Inf),
                      'Water Temperature at Surface' = c(-Inf, Inf),
                      'Relative Humidity' = c(0, Inf),
                      'Wind Gust' = c(0, Inf),
                      'Dew Point' = c(-Inf, Inf),
                      'ysi_chlorophyll' = c(0, 100),
                      'ysi_blue_green_algae' = c(0, Inf),
                      'water_conductivity' = c(0, Inf),
                      'ysi_turbidity' = c(0, Inf),
                      'dissolved_oxygen' = c(0, Inf),
                      'dissolved_oxygen_saturation' = c(0, Inf),
                      'ph' = c(-Inf, Inf))
    return (which((meas()$value < extrVal[1]) | (meas()$value > extrVal[2])))
  }
  
  # Function for testing whether measurement outside of **expected** limits.
  test_grossRange_susp <- function(){
    if (substr(input$sensC, 1, length('Thermistor String')) == 'Thermistor String'){
      sensN <- 'Thermistor String'
    } else if (substr(input$sensC, 1, length('Depth in Water')) == 'Depth in Water'){
      sensN <- 'Depth in Water'
    } else {
      sensN <- input$sensC
    }
    extrVal <- switch(sensN,
                      'Wind from Direction' = c(-Inf, Inf),
                      'Significant Wave Period' = c(-Inf, 500),
                      'Wind Speed' = c(-Inf, 500),
                      'Significant Wave Height' = c(-Inf, 50),
                      'Air Temperature' = c(-50, 60),
                      'Air Pressure' = c(900, 1100),
                      'Thermistor String' = c(-10, 40),
                      'Depth in Water' = c(-Inf, 100),
                      'Water Temperature at Surface' = c(-10, 40),
                      'Relative Humidity' = c(-Inf, 150),
                      'Wind Gust' = c(-Inf, 500),
                      'Dew Point' = c(-50, 60),
                      'ysi_chlorophyll' = c(-Inf, 10),
                      'ysi_blue_green_algae' = c(-Inf, 10),
                      'water_conductivity' = c(-Inf, 10000),
                      'ysi_turbidity' = c(-Inf, 1000),
                      'dissolved_oxygen' = c(-Inf, 30),
                      'dissolved_oxygen_saturation' = c(-Inf, 200),
                      'ph' = c(5, 11))
    return (which((meas()$value < extrVal[1]) | (meas()$value > extrVal[2])))
  }
  
  # Test for large spikes
  test_spike <- function(){
    m <- meas()$value
    mt <- meas.dateTime()
    s <- list(spike=vector(), rt=vector())
    for (i in 2:(length(m)-1)){
      # Get data range within 48 hours (on either side)
      mdum <- m[abs(difftime(mt, mt[i], units='hours')) < 48]
      thres <- 0.5*diff(range(mdum))
      if ((abs(m[i]-m[i-1]) > thres) & (abs(m[i]-m[i+1]) > thres) &
          (sign(m[i]-m[i-1]) == sign(m[i]-m[i+1]))){
        s$spike <- c(s$spike, i)
      } else if (abs(m[i]-m[i-1]) > thres){
        if (!((i-1) %in% s$spike)){
          s$rt <- c(s$rt, i)
        }
      }
    }
    return (s)
  }
  
#   # Test for rates of change
#   test_rtChange <- function(){
#     m <- meas()$value
#     mt <- meas.dateTime()
#     s <- vector()
#     for (i in 2:(length(m))){
#       # Get data range within 48 hours (on either side)
#       dum <- which(abs(difftime(mt, mt[i], units='mins')) < 48*60)
#       dtavg <- mean(diff(mt[dum]))
#       thres <- 2 * sd(m[dum]) / as.numeric(dtavg)
#       if (abs(m[i]-m[i-1])/as.numeric(difftime(mt[i],mt[i-1], units='mins')) > thres){
#         s <- c(s, i)
#       }
#     }
#     return (s)
#   }
  
  # Test for long time gaps
  test_timeGap <- function(){
    mt <- meas.dateTime()
    s <- vector()
    for (i in 2:(length(mt))){
      # Get data range within 48 hours (on either side)
      dum <- which(abs(difftime(mt, mt[i], units='mins')) < 48*60)
      thres <-  max(3 * sd(as.numeric(mt[dum])), 60*12)
      if (as.numeric(difftime(mt[i],mt[i-1], units='mins')) > thres){
        s <- c(s, i)
      }
    }
    return (s)
  }
  
  # Test for flat-line (5 equal non-zero values in a row)
  test_flatLine <- function(){
    mdiff <- diff(meas()$value)
    s <- vector()
    numFlat <- 5
    for (i in (numFlat-1):(length(mdiff))){
      if (abs(meas()$value[i]) > 1e-12 ){
        if (sum(abs(mdiff[(i-numFlat+2):i])) < 1e-12){
          s <- c(s, (i-numFlat+2):(i+1))
        }
      }
      
    }
    s <- unique(s)
    return (s)
  }
  
  # Get error flags, return indices of flagged values for omission
  flgs_omit <- function(){
    # Coordinates unphysical
    if ('coordFail' %in% input$cleanOpt){
      location_fail <- which((meas()$lat > 50) | (meas()$lon < -95) | (meas()$lat < 40) | (meas()$lon > -75))
    } else {location_fail <- vector()}
    # Coordinates moved significantly
    if ('coordSusp' %in% input$cleanOpt){
      location_susp <- (which((diff(meas()$lat) > 0.01) | (diff(meas()$lon) > 0.01)) + 1)
    } else {location_susp <- vector()}
    # Test physical and expected limits of meas values
    if ('valFail' %in% input$cleanOpt){
      grossRange_fail <- test_grossRange_fail()
    } else {grossRange_fail <- vector()}
    if ('valSusp' %in% input$cleanOpt){
      grossRange_susp <- test_grossRange_susp()
    } else {grossRange_susp <- vector()}
    # Check for large spikes
    if ('spikeSusp' %in% input$cleanOpt){
      spike_susp <- test_spike()$spike
    } else {spike_susp <- vector()}
    # Check for flat line
    if ('FlatSusp' %in% input$cleanOpt){
      flatLine_susp <- test_flatLine()
    } else {flatLine_susp <- vector()}    
    
    return(sort(unique( c(location_fail, location_susp, 
                   grossRange_fail, grossRange_susp,
                   spike_susp, flatLine_susp))))
  }

  # Get error flags, return indices of flagged values for omission
  flgs_vis <- function(){
    # Check for large rate of change
    if ('rtChange' %in% input$cleanVis){
      rtChange <- test_spike()$rt
    } else {rtChange <- vector()}
    # Check for large time gap
    if ('timeGap' %in% input$cleanVis){
      timeGap <- test_timeGap()
    } else {timeGap <- vector()}
    
    f <- sort(unique(c(rtChange, timeGap)))
    f <- setdiff(f, flgs_omit())
    return(f)
  }
  
  # Cleaned dataset, flagged pts removed
  meas_clean <- reactive({
    data.frame(t = meas.dateTime()[-flgs_omit()], val = meas()$value[-flgs_omit()])
  })
  
  # Flagged pts for omission
  meas_errOmit <- reactive({
    data.frame(t = meas.dateTime()[flgs_omit()], val = meas()$value[flgs_omit()])
  })
  
  # Flagged pts for visualization only
  meas_errVis <- reactive({
    data.frame(t = meas.dateTime()[flgs_vis()], val = meas()$value[flgs_vis()])
  })
  
  ranges2Full <- reactiveValues(x=NULL, y=NULL)
  
  # Plot full dataset, highlighting flagged pts
  output$pltFull1 <- renderPlot({
    if(length(input$buoyC) != 0){
      if(length(meas()$value) > 0){
    #    withProgress(message = 'Please wait', value=0, {
          dat_full <- data.frame(x = meas.dateTime(), y = meas()$value)
          dat_errOmit <- data.frame(x = meas_errOmit()$t, y = meas_errOmit()$val)
          dat_errVis <- data.frame(x = meas_errVis()$t, y = meas_errVis()$val)
          
   #     })
        ggplot(dat_full, aes(x=x,y=y)) + geom_line() + 
          geom_point(data=dat_errVis, colour='blue', size=3) +
          geom_point(data=dat_errOmit, colour='red', size=3) + 
          labs(x='Date-Time', y=sensUnits()) + 
          theme(plot.title=element_text(size=20, face='bold', margin=margin(10,0,10,0)))
      }
    }
  })
  
  # Plot full dataset, highlighting flagged pts
  output$pltFull2 <- renderPlot({
    if(length(input$buoyC) != 0){
      if(length(meas()$value) > 0){
        dat_full <- data.frame(x = meas.dateTime(), y = meas()$value)
        dat_errOmit <- data.frame(x = meas_errOmit()$t, y = meas_errOmit()$val)
        dat_errVis <- data.frame(x = meas_errVis()$t, y = meas_errVis()$val)
        ggplot(dat_full, aes(x=x,y=y)) + geom_line() + 
          geom_point(data=dat_errVis,colour='blue', size=3) +
          geom_point(data=dat_errOmit,colour='red', size=3) + 
          coord_cartesian(xlim = ranges2Full$x, ylim = ranges2Full$y) +
          labs(x='Date-Time', y=sensUnits()) + 
          theme(plot.title=element_text(size=20, face='bold', margin=margin(10,0,10,0)))
      }
    }
  })
  
  # Plot Clean dataset, highlighting flagged pts
  output$pltClean2 <- renderPlot({
    if(length(input$buoyC) != 0){
      if((length(meas()$value) > 0) & (length(flgs_omit()) > 0)){
        dat_Clean <- data.frame(x = meas_clean()$t, y = meas_clean()$val)
        dat_errVis <- data.frame(x = meas_errVis()$t, y = meas_errVis()$val)
        ggplot(dat_Clean, aes(x=x,y=y)) + geom_line() + 
          geom_point(data=dat_errVis,colour='blue', size=3) +
          coord_cartesian(xlim = ranges2Full$x, ylim = ranges2Full$y) +
          labs(x='Date-Time', y=sensUnits()) + 
          theme(plot.title=element_text(size=20, face='bold', margin=margin(10,0,10,0)))
      }
    }
  })
  
  observe({
    brushFull <- input$pltFull1_brush
    if (!is.null(brushFull)) {
      ranges2Full$x <- c(as.POSIXct(brushFull$xmin, origin='1970-01-01'), 
                     as.POSIXct(brushFull$xmax, origin='1970-01-01'))
      ranges2Full$y <- c(brushFull$ymin, brushFull$ymax)
    } else {
      ranges2Full$x <- NULL
      ranges2Full$y <- NULL
    }
  })



  # Download full dataset
  output$dlFull <- downloadHandler(
    filename = function() {paste(input$buoyC, '_', input$sensC, '_',
                                 input$timeSpan, '_Full.csv', sep='')},
    content = function(file) {write.table(data.frame(DateTime=meas.dateTime(), Value=meas()$value), file, row.names=F, sep=',')}
  )
  
  # Download cleaned dataset
  output$dlClean <- downloadHandler(
    filename = function() {paste(input$buoyC, '_', input$sensC, '_',
                                 input$timeSpan, '_Clean.csv', sep='')},
    content = function(file) {write.table(data.frame(DateTime=meas_clean()$t, Value=meas_clean()$val), file, row.names=F, sep = ',')}
  )
  
  
}

shinyApp(ui=ui, server=server)

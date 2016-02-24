library(shiny)
#library(shinyjs)
#library(logging)

library(leaflet)
library(lubridate)

data(state)
old_storms <- read.csv("result2010.txt",strip.white=TRUE,fill=TRUE,nrow=-1)
col_names <- c("BGN_DATE","STATE","EVTYPE","FATALITIES","LATITUDE","LONGITUDE" )
colnames(old_storms) <- col_names
old_storms$Date <- as.Date(sapply(strsplit(as.character(old_storms$BGN_DATE),' '),
                              function(x) {x[[1]]}),format = '%m/%d/%Y')

old_storms$Year <- year(old_storms$Date)
old_storms$Month <- months.Date(old_storms$Date)
old_storms$MonthF <- factor(old_storms$Month,levels=month.name)
#print("Date Complete")
old_storms$EVTYPE <- toupper(old_storms$EVTYPE)
old_storms$EVTYPEN <- "OTHER"
old_storms$EVTYPEN[grep("*TORNADO*", old_storms$EVTYPE)] <- "TORNADO"
old_storms$EVTYPEN[grep("*HURRICANE*|*TYPHOON*", old_storms$EVTYPE)] <- "HURRICANE"
old_storms$EVTYPEN[grep("*WIND*", old_storms$EVTYPE)] <- "WIND"
old_storms$EVTYPEN[grep("*FIRE*", old_storms$EVTYPE)] <- "FIRE"
old_storms$EVTYPEN[grep("*STORM*|*GLAZE*|*HAIL*|*WETNESS*|*LIGHTNING*|*RAIN*|*BLIZZARD*", old_storms$EVTYPE)] <- "STORM"
#old_storms$EVTYPEN[grep("*COLD*|*LOW TEMPERATURE*|*WINTRY*|*FREEZE*", old_storms$EVTYPE)] <- "COLD"
#old_storms$EVTYPEN[grep("*SNOW*", old_storms$EVTYPE)] <- "SNOW"
old_storms$EVTYPEN[grep("*FLOOD*|*STREAM*|*HIGH WATER*", old_storms$EVTYPE)] <- "FLOOD"
old_storms$EVTYPEN[grep("*HEAT*|*HOT*", old_storms$EVTYPE)] <- "HEAT"
old_storms$EVTYPEN[grep("*SURF*|*SEAS*|*MARINE*|*CURRENT*|*TSUNAMI", old_storms$EVTYPE)] <- "SURF"
old_storms$EVTYPEN[grep("*FOG*", old_storms$EVTYPE)] <- "FOG"
old_storms$EVTYPEN[grep("*DRY*|*DROUGHT*", old_storms$EVTYPE)] <- "DRY"
old_storms$EVTYPEN[grep("*LANDSLIDE*|*LAND*|*AVALANCHE*|*SLIDE*", old_storms$EVTYPE)] <- "LANDSLIDE"
old_storms$EVTYPEN[grep("*ICE*|*ICY*|*FROST*|SNOW*|*COLD*|*LOW TEMPERATURE*|*WINTRY*|*FREEZE*", old_storms$EVTYPE)] <- "COLD"

#storms <- na.omit(old_storms)
old_storms$lat <- as.numeric(as.character(old_storms$LATITUDE))/100.0
old_storms$long <- (as.numeric(old_storms$LONGITUDE)/100.0)*-1.0
#old_storms <-old_storms[old_storms$Year == 2010,]
#old_storms <- old_storms[old_storms$EVTYPE %in% c("TORNADO","HURRICANE","FLOOD","SNOW","FIRE","ICE"),]
#old_storms <-old_storms[old_storms$lat >0,]
#old_storms <-old_storms[old_storms$long != 0,]
storms <- old_storms[as.character(old_storms$STATE) %in% state.abb,]
storms <- storms[complete.cases(storms),]
rm(old_storms)

#grep "^.*[0-9]\{1,\}\/[0-9][0-9]\/20*" repdata-data-StormData.csv > 2000data.txt
#awk -F "," '{print $2,"\,",$7,"\,",$8,"\,",$23,"\,",$33,"\,",$34}' 2000data.txt > result.txt
#storms <- read.csv("repdata-data-StormData.csv",nrow=10000)
valid_events  <- c("TORNADO","FLOOD","STORM")        

storms$EVTYPESTRING = paste(storms$EVTYPEN,storms$Date,"-")
storms <- storms[as.character(storms$EVTYPEN) %in% valid_events,]
#storms$EVTYPEC = recode(storms$EVTYPEN, "'TORNADO'=1; 'WIND'=2; 'FLOOD'=3; 'SNOW'=4; 'FIRE'=5; 'ICE'=6;",  as.factor.result=FALSE)
print("range complete")

#pal <- colorNumeric(palette = "Blues",domain = 1:3)
#print("colorNumeric complete")

#storms$EVTYPEC <- mapvalues(storms$EVTYPE, from = valid_events , to = c("1", "2","3","4","5","6"))


state_list <- as.list(sort(as.character(unique(storms$STATE))))

month_list <- c( "January","February","March","April","May","June","July","August","September","October","November","December")

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

input_min <- min(storms$FATALITIES)
input_max <- max(storms$FATALITIES)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
      
  #make dynamic slider
  output$selectFatalities <- renderUI({
    sliderInput("selectFatalities", label = h4("Fatality Slider"), min=input_min, max=input_max, value=0)
  })  
  
  output$selectedStates <- renderUI({
    selectInput("selectedStates", label=h4('Select States'), as.list(state_list), multiple=TRUE, selectize=TRUE)
  })
  
  output$selectMonth <- renderUI({
    checkboxGroupInput("selectMonth", "Select Month",as.list(month_list))
  })
  
  output$selectEvent <- renderUI({
    checkboxGroupInput("selectEvent", "Select Event",as.list(valid_events))
  })
  
  merged_data<-reactive({
    input_min <- min(storms$FATALITIES)
    input_max <- max(storms$FATALITIES)
    
    stormsF <- storms      
    print(dim(stormsF))
    print(input$selectedStates)
    if (length(input$selectedStates) > 0) {
        stormsF <- stormsF[stormsF$STATE %in% input$selectedStates,]
    } else {
      print("No State selected")
    }

    print(dim(stormsF))
    print(input$selectFatalities)      
    if (length(input$selectFatalities) > 0) {
        stormsF <- stormsF[stormsF$FATALITIES >= input$selectFatalities,]
    } else {
      print("No Fatality selected")      
    }
    print(dim(stormsF))

    print(input$selectMonth)
    if (length(input$selectMonth) >0) {
      stormsF <- stormsF[stormsF$Month %in% input$selectMonth,]      
    } else {
      print("No Month selected")      
    }
    print(dim(stormsF))
    
    print(input$selectEvent)
    if (length(input$selectEvent) >0) {
      stormsF <- stormsF[stormsF$EVTYPEN %in% input$selectEvent,]      
    } else {
      print("No Event selected")      
    }
    print(dim(stormsF))
    
    return(stormsF)
    
  })
  
  
  output$mymap <- renderLeaflet({

    stormsX <- merged_data()
    
    #stormsF <- storms
    print("Rendering")  
    print(dim(stormsX))
    if (nrow(stormsX) == 0) {
      leaflet(stormsX) %>%
        addProviderTiles("Stamen.TonerLite",
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        setView(lng = -83.85, lat = 27.45, zoom = 3)
        
    } else {
      withProgress(message = 'Making plot.............', value = 0, {    
        leaflet(stormsX) %>%
        addProviderTiles("Stamen.TonerLite",
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        setView(lng = -83.85, lat = 27.45, zoom = 3) %>%
        addCircleMarkers(
          lng=~long, # Longitude coordinates
          lat=~lat, # Latitude coordinates
          radius=~(.05*FATALITIES), # Total count
          stroke=TRUE, # Circle stroke
          fillOpacity=~(FATALITIES*.01), # Circle Fill Opacity
          popup = ~EVTYPESTRING,
          color = ~ifelse(EVTYPEN == 'TORNADO', 'purple', 
                           ifelse(EVTYPEN == 'FIRE', 'red', 
                                  ifelse(EVTYPEN == 'FLOOD', 'blue', 
                                         ifelse(EVTYPEN == 'STORM', 'green', 
                                                ifelse(EVTYPEN == 'ICE', 'yellow', 'pink')))))) %>%                
          addLegend("bottomright", colors = c("purple","green","blue"), labels=c("Tornado","Storm","Flood"),
                   title = "Weather Incidents (2010)",
                   labFormat = labelFormat(prefix = "--"),
                   opacity = 1
                  ) 
        }
      )
    }
  })
  
})

library(shiny)
library(leaflet)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
     headerPanel("Storm Watch"),
     mainPanel(
        tabsetPanel(
          tabPanel( "Map",leafletOutput("mymap")),
          tabPanel("Help",includeHTML(knitr::knit2html("UserDoc.Rmd", fragment.only = TRUE)))
        )
     ),
     sidebarPanel(
       uiOutput("selectFatalities"),
       uiOutput("selectedStates"),
       uiOutput("selectEvent"),
       uiOutput("selectMonth")
       
     )
))


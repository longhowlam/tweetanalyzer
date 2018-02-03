
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(networkD3)
library(stringr)
library(leaflet)
library(rCharts)

shinyUI(fluidPage(

  # Application title
  titlePanel("Longhow's Tweet Analyzer (retrieving live tweets)"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width=3,
     
      textInput("zoeksleutel","Twitter search key (max 500 tweets)","trein"),
      numericInput("sparse","sparse factor text clustering", 0.99,min=0.1,max=1),
      selectInput("clusterM","text cluster method",list("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")),
      radioButtons("taal","Stopwords language to control the word cloud",c("Dutch","English","both")),
      sliderInput("charge", "charge factor network (for tweet/retweet graph)", -20, min = -100, max = 100, step = 10),
      
      submitButton("Update View")
    
    ),
              
  
    # Show a plot of the generated distribution
    mainPanel(width=9,
      tabsetPanel(
        tabPanel("Tweets", dataTableOutput("data")), 
        tabPanel("summary", dataTableOutput("summary")),
        #tabPanel("Tweeting from", showOutput("TweetFromPlot","nvd3")),
        tabPanel("Word Cloud", plotOutput("wcloud")), 
        tabPanel("Text clusters", plotOutput("textclusters")), 
        tabPanel("GeoMap of tweets", h3("Only shows tweets with GPS locations"),leafletOutput("mapplot")),
        tabPanel("Tweet Retweet Graph", simpleNetworkOutput("simple",height="800px"))    
      )
    )
  )
  )
)

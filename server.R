
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(twitteR)
library(networkD3)
library(stringr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(leaflet)
library(dplyr)


options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

consumerKey     <- "YOURKEY"
consumerSecret  <- "YOURSECRET"
accessToken     <- "your ACCESS TOKEN"
accessSecret    <- "your ACCESS SECRET"
setup_twitter_oauth(consumer_key =consumerKey, consumer_secret=consumerSecret, access_token=accessToken,access_secret=accessSecret)




createLeafNode <- function(hclust, i) {
  list(name = hclust$labels[[i]],
       order = hclust$order[[i]])
}

hclustToTree <- function(hclust) {
  if (length(hclust$merge) == 0)
    return(NULL)
  
  merges <- list()
  for (index in 1:nrow(hclust$merge)) {
    left <- hclust$merge[index, 1]
    right <- hclust$merge[index, 2]
    
    if (left < 0)
      left <- createLeafNode(hclust, -left)
    else
      left <- merges[[left]]
    if (right < 0)
      right <- createLeafNode(hclust, -right)
    else
      right <- merges[[right]]
    
    if (left$order > right$order) {
      tmp <- left
      left <- right
      right <- tmp
    }
    
    merges[[index]] <- list(
      children = list(
        left,
        right
      ),
      order = left$order
    )
  }
  
  return(merges[nrow(hclust$merge)])
}


shinyServer(function(input, output) {

  datasetTwitter <- reactive({
   tmp =  twListToDF(searchTwitter(input$zoeksleutel,n=500))
   poster = str_extract(tmp$text,"(RT|via)((?:\\b\\W*@\\w+)+)") 
   poster = str_sub(poster,4,nchar(poster)) 
   cbind(poster,tmp)
   
  })
  
  
  # onderstaande geeft nog een ajax error
  output$data <- renderDataTable({
    
    tmp = datasetTwitter() 
    data = as.data.frame(tmp[,c("poster", "screenName","text","created")])
    data$text = iconv(data$text, "latin1", "ASCII", sub="")
    data
  })

  # probeer met gewone print
  output$myTable <- renderPrint({
    tmp = datasetTwitter() 
    data = as.data.frame(tmp[,c("text")])
    cat(data)         
  })
  
  
  output$summary <- renderDataTable({
    tmp <- datasetTwitter()
    
    Screen.Names = table(datasetTwitter()$screenName)
    data =data.frame(Screen.Names )
    names(data) =c("Screen.Name","Frequency")
    data
    
  })
  
  
  output$mapplot <- renderLeaflet({
    
    twDF    = datasetTwitter()
    test    = twDF[!is.na(twDF$long),]
    avglong = mean(as.numeric(test$longitude))
    avglat  = mean(as.numeric(test$latitude))
    
    m       = leaflet(test)%>% addCircles(weight=8) %>% addTiles()
    m       = m %>% setView(avglong, avglat, zoom = 5)
    m       = m %>% addPopups(lng = test$longitude,lat = test$latitude,popup=test$text)
    m

  })
  
  
  ### interactieve barchart voor twitter source
  output$TweetFromPlot <- renderChart2({
    twDF          = datasetTwitter()
    twDF$Twittersource = str_sub(twDF$statusSource, 
                      str_locate(twDF$statusSource,'>')[,1] + 1,
                      str_locate(twDF$statusSource,'</')[,1] -1
    )
    twDF$HOUR = format(twDF$created, format='%H')
    twDF$Freq = 1
    twDF <- filter(twDF,  Twittersource == "Twitter for Android" |  Twittersource == "Twitter for iPhone" | Twittersource=="Twitter for Windows Phone")
    n1 <- nPlot( Freq ~ HOUR, group = "Twittersource", data = twDF, type = "multiBarChart")
    return(n1)
  })

  ## code for word cloud using text mnining
  output$wcloud <- renderPlot({
    # draw the wordcloud 
    mach_text <- datasetTwitter()$text
    # remove disturbing characters
    mach_text = sapply(mach_text,function(row) iconv(row, "latin1", "ASCII", sub=""))
    
    
    # create a corpus
    mach_corpus = Corpus(VectorSource(mach_text))
    mytwittersearch_corpus <- tm_map(mach_corpus, content_transformer(stringi::stri_trans_tolower))
  
    if(input$taal == "Dutch"){stlist = stopwords("dutch")}
    if(input$taal == "English"){stlist = stopwords("english")}
    if(input$taal == "both"){stlist = c(stopwords("dutch"), stopwords("english"))}
    
    
    dm = TermDocumentMatrix(
      mytwittersearch_corpus,
      control = list(
        removePunctuation = TRUE,
        stopwords = c(input$zoeksleutel,stlist),
        removeNumbers = TRUE, tolower = TRUE
      )
    )
  
    # create document term matrix applying some transformations
    
    # define tdm as matrix
    m = as.matrix(dm)
    # get word counts in decreasing order
    word_freqs = sort(rowSums(m), decreasing=TRUE) 
    # create a data frame with words and their frequencies
    dm = data.frame(word = names(word_freqs), freq = word_freqs)
    wordcloud(dm$word, dm$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
  }, height=800)
  
  
  ## use hierarchiecal clustering on term document matrix
  output$textclusters <- renderPlot({
    # draw the wordcloud 
    mach_text <- datasetTwitter()$text
    # remove disturbing characters
    mach_text = sapply(mach_text,function(row) iconv(row, "latin1", "ASCII", sub=""))
    
    
    # create a corpus
    mach_corpus = Corpus(VectorSource(mach_text))
    mytwittersearch_corpus <- tm_map(mach_corpus, content_transformer(stringi::stri_trans_tolower))
    
    if(input$taal == "Dutch"){stlist = stopwords("dutch")}
    if(input$taal == "English"){stlist = stopwords("english")}
    if(input$taal == "both"){stlist = c(stopwords("dutch"), stopwords("english"))}
    
    dm = TermDocumentMatrix(
      mytwittersearch_corpus,
      control = list(
        removePunctuation = TRUE,
        stopwords = c(input$zoeksleutel,stlist),
        removeNumbers = TRUE, tolower = TRUE
      )
    )
    
    # create document term matrix applying some transformations
    
    # define tdm as matrix
    dm = removeSparseTerms(dm, input$sparse)
    m = as.matrix(dm)
    dist.m = dist(scale(m))
    hc = hclust(dist.m, method =  input$clusterM)
    plot(hc,cex=0.85)
    #halfway <- hclustToTree(hc)
    #d3Tree(List = halfway, fontsize = 10, diameter = 500)
    
    
  },height = 600, width = 800 )
  
  
  
  
  
  output$simple <- renderSimpleNetwork({
    
    MisLinks = datasetTwitter()
    simpleNetwork(charge = input$charge,MisLinks,Source="poster",Target="screenName",fontSize=8, opacity = 0.6)
    
  })
  
  
  
})

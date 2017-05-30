shinyServer(function(input, output) {
  
  requiredData <- reactive({
    data <- rawData[date >= input$dateRange[1] & date <= input$dateRange[2]]
    return(data)
  })
  
  output$basicInfo <- renderPrint({
    x <- paste("Number of Tweets are", nrow(requiredData()), '\n')
    return(cat(x))
    })
  
  corpusData <- reactive({
    data <- cleanTextVector(requiredData()[, text])
    return(data)
    })
  
  output$wordCloud <- renderPlot({
    
    wordcloud(corpusData(), min.freq=input$minFreq, max.words=input$maxWord, colors=brewer.pal(8, "Dark2"))
  })
  
  tdm <- reactive({
    dtm <- TermDocumentMatrix(corpusData())
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.table(word = names(v),freq=v)
    return(d)
  })
  
  output$freqTable <- renderDataTable({return(head(tdm(), input$maxWord))})
  
})
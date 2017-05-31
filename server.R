shinyServer(function(input, output) {
  
  requiredData <- reactive({
    rawData[date >= input$dateRange[1] & date <= input$dateRange[2]]
  })
  
  output$basicInfo <- renderPrint({
    paste("Number of Tweets are", nrow(requiredData()), '\n')
    })
  
  corpusData <- reactive({
    cleanTextVector(requiredData()[, text])
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
  
  output$freqTable <- renderPlot({
    plotData <- head(tdm(), input$maxWord)
    p <- ggplot(plotData, aes(x = reorder(word, freq), y = freq))
    p <- p + geom_bar(aes(fill = freq), stat = 'identity')
    #p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    p <- p + xlab("Words") + coord_flip() + ylab("Frequencies")
    p <- p + geom_text(aes(hjust = -.1, label = freq), color = "red")
    p <- p + scale_fill_gradient(low = 'blue', high = 'orange')
    return(p)
    #return(ggplotly(p))
  })
  
})

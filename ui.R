twitterHandles <- c('@republic')
minDate        <- as.Date("2017-05-02")
maxDate        <- as.Date("2017-05-28")

shinyUI(navbarPage("Indian-News Channels",
                   tabPanel("Twitter",
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                           selectInput('selectedData', "Select Twitter Handle", twitterHandles, selected = '@republic')
                                           ,sliderInput("dateRange", "Date Range :", min=minDate, max=maxDate, value = c(minDate, maxDate))
                                           ,sliderInput("minFreq", "Minimum Frequency : ", min = 1, max = 10, value = 5)
                                           ,sliderInput("maxWord", "Maximun Words : ", min = 1, max = 100, value = 50)
                              ),
                              mainPanel(
                                verbatimTextOutput('basicInfo')
                                ,tabsetPanel(type='pills'
                                             ,tabPanel('Word Cloud', plotOutput("wordCloud"))
                                             ,tabPanel('Freq Data', dataTableOutput("freqTable")))
                              )
                            )
                    )
))
twitterHandles <- c('@republic')
minDate        <- as.Date("2017-05-01")
maxDate        <- as.Date("2017-05-30")

shinyUI(navbarPage("Indian-News Channels",
                   tabPanel("Twitter",
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                           selectInput('selectedData', "Select Twitter Handle", twitterHandles, selected = '@republic')
                                           ,sliderInput("dateRange", "Date Range :", min=minDate, max=maxDate, value = c(minDate, maxDate))
                                           ,sliderInput("minFreq", "Minimum Frequency : ", min = 1, max = 10, value = 5)
                                           ,sliderInput("maxWord", "# Words : ", min = 1, max = 100, value = 25)
                              ),
                              mainPanel(
                                verbatimTextOutput('basicInfo')
                                ,tabsetPanel(type='pills'
                                             ,tabPanel('Word Cloud'
                                                       ,plotOutput("wordCloud")
                                                       ,plotOutput("freqTable")
                                                       )
                                             )
                                )
                              )
                            )
))
#Functions used in Tweet Analysis

get_most_frequent_words <- function (inputDataFrame = NULL, nWords = 20, handle = "self", month = "", year = "", consVar = "", ...){

  if(is.null(inputDataFrame)){
    cat("Input Data Frame is NULL, returning NULL\n")
    return(null)
  }
  reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
  tweet_words <- inputDataFrame %>% filter(!str_detect(text, '^"')) %>% mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) 
  tweet_words_freq <- tweet_words %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

  '%ni%' = Negate('%in%')
  tweet_words_freq_df = as.data.frame(tweet_words_freq)
  tweet_words_freq_df  = tweet_words_freq_df[which(tweet_words_freq_df$word %ni% c('rt', 'http', 'https', handle, paste('@', handle, sep = ""), tolower(paste('@', handle, sep = "")), toupper(paste('@', handle, sep = "")))),]
  word_freq_sort = arrange(aggregate(tweet_words_freq_df[,consVar], by = list(tweet_words_freq_df$word) ,FUN = length), desc = -1*x)
  names(word_freq_sort) = c('Word', 'Occurance')
  word_freq_sort = word_freq_sort[!(grepl(handle, word_freq_sort$Word)),]

  word_freq_sort_to_plot = word_freq_sort[1:nWords,]
  max_freq = word_freq_sort_to_plot[1,2]
  n_tweets = nrow(inputDataFrame)
  return(tweet_words_freq_df)
}

plot_most_frequent_words = function(freqMatrix, consVar = "", n_tweets = 100, nWords = 20, handle = "self", month = "", year = "", ... ){
#Plot words and it's frequency

  word_freq_sort = arrange(aggregate(freqMatrix[,consVar], by = list(freqMatrix$word) ,FUN = length), desc = -1*x)
  names(word_freq_sort) = c('Word', 'Occurance')
  word_freq_sort = word_freq_sort[!(grepl(handle, word_freq_sort$Word)),]

  word_freq_sort_to_plot = word_freq_sort[1:nWords,]
  max_freq = word_freq_sort_to_plot[1,2]
  
  gg <- ggplot(word_freq_sort_to_plot, aes(y=reorder(Word, Occurance), x=Occurance))
  gg <- gg + geom_lollipop(point.colour="steelblue", point.size=3, horizontal=TRUE)
  gg <- gg + scale_x_continuous(expand=c(0,0), limits=c(0, max_freq + 5))
  gg <- gg + labs(x=NULL, y=NULL, 
                title=paste("Most frequent words used by ", handle, " handle in ", month, " " ,year, sep = ""),
                subtitle=paste("Data from ", handle, " last ", n_tweets, " tweets" , sep = ""),
                caption=paste("Data from https://twitter.com/",handle, sep = ""))
  gg <- gg + theme_minimal(base_family="Arial Narrow")
  gg <- gg + theme(panel.grid.major.y=element_blank())
  gg <- gg + theme(panel.grid.minor=element_blank())
  gg <- gg + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
  gg <- gg + theme(axis.text.y=element_text(margin=margin(r=-1, l=0)))
  gg <- gg + theme(plot.margin=unit(rep(30, 4), "pt"))
  gg <- gg + theme(plot.title=element_text(face="bold"))
  gg <- gg + theme(plot.subtitle=element_text(size=8, margin=margin(b=10)))
  gg <- gg + theme(plot.caption=element_text(size=7, margin=margin(t=10)))
  return(gg)

}

plot_most_frequent_words_df = function(freqMatrix, n_tweets = 100, handle = "self", month = "", year = "", title = "", ... ){
#Plot words and it's frequency
  max_freq = freqMatrix[1,2]
  gg <- ggplot(freqMatrix, aes(y=reorder(Word, Occurance), x=Occurance))
  gg <- gg + geom_lollipop(point.colour="steelblue", point.size=5, horizontal=TRUE)
  gg <- gg + scale_x_continuous(expand=c(0,0), limits=c(0, max_freq + 5))
  gg <- gg + labs(x=NULL, y=NULL, 
                title=title,
                subtitle=paste("Data from ", handle, " tweets" , sep = ""),
                caption=paste("Data from https://twitter.com/",handle, sep = ""))
  gg <- gg + theme_minimal(base_family="Arial Narrow")
  gg <- gg + theme(panel.grid.major.y=element_blank())
  gg <- gg + theme(panel.grid.minor=element_blank())
  gg <- gg + theme(axis.line.y=element_line(color="#2b2b2b", size=0.5))
  gg <- gg + theme(axis.text.y=element_text(size = 20, face = "bold.italic", margin=margin(r=-1, l=0)))
  gg <- gg + theme(axis.text.x=element_text(size = 20, face = "bold.italic"))
  gg <- gg + theme(plot.margin=unit(rep(30, 4), "pt"))
  gg <- gg + theme(plot.title=element_text(size=32, face="bold"))
  gg <- gg + theme(plot.subtitle=element_text(size=22, margin=margin(b=10)))
  gg <- gg + theme(plot.caption=element_text(size=12, margin=margin(t=10)))
  return(gg)

}

plot_most_frequent_words_screen_shot = function(freqMatrix, n_tweets = 100, handle = "self", month = "", year = "", title = "", ... ){
#Plot words and it's frequency
  max_freq = freqMatrix[1,2]
  gg <- ggplot(freqMatrix, aes(y=reorder(Word, Occurance), x=Occurance))
  gg <- gg + geom_lollipop(point.colour="steelblue", point.size=3, horizontal=TRUE)
  gg <- gg + scale_x_continuous(expand=c(0,0), limits=c(0, max_freq + 5))
  gg <- gg + labs(x=NULL, y=NULL, 
                title=title,
                subtitle=paste("Data from ", handle, " tweets" , sep = ""),
                caption=paste("Data from https://twitter.com/",handle, sep = ""))
  gg <- gg + theme_minimal(base_family="Arial Narrow")
  gg <- gg + theme(panel.grid.major.y=element_blank())
  gg <- gg + theme(panel.grid.minor=element_blank())
  gg <- gg + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
  gg <- gg + theme(axis.text.y=element_text(margin=margin(r=-1, l=0)))
  gg <- gg + theme(plot.margin=unit(rep(30, 4), "pt"))
  gg <- gg + theme(plot.title=element_text(face="bold"))
  gg <- gg + theme(plot.subtitle=element_text(size=8, margin=margin(b=10)))
  gg <- gg + theme(plot.caption=element_text(size=7, margin=margin(t=10)))
  return(gg)

}

two_word_freq <- function(x) {
  pr <- unlist(
    lapply(
      strsplit(x, ' '), 
      function(i) combn(sort(i), 2, paste, collapse=' ')
    )
  )

  tbl <- table(pr)

  d <- do.call(rbind.data.frame, strsplit(names(tbl), ' '))
  names(d) <- c('word1', 'word2')
  d$Freq <- tbl

  d
}

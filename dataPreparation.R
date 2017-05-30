#run pyScripts to get historic data as a json file
#pyScripts are courtesy work of https://github.com/bpb27/twitter_scraping
#will be cheking how did tweets of yogendra yadav changed after he was expelled from aap
#https://www.r-bloggers.com/geom_lollipop-by-the-chartettes/

#Define Config Parameters
#TODO: Get it out to a config file
config = c()
config$baseDirPath = ""
config$rScriptsPath = ""
#config$inputDataPath = "/Users/addhyanpandey/pWorkspace/twitter_analysis/historic_data_extractor/"
config$plotPath = "Datasets/outputData/plots/"
dir.create(file.path(config$plotPath), showWarnings = FALSE)
config$packages = c('jsonlite', 'base64enc', 'dplyr', 'purrr', 'tidyr', 'lubridate', 'ggplot2', 'scales', 'tidytext', 'stringr')

config$handles = c("republic", "IndianExpress", "ZeeNews", "abpnewstv", "timesofindia")
config$handle = config$handles[1]
config$inputFileName = paste("Datasets/inputData/", config$handle, '/', config$handle, ".json", sep = "")
config$inputFileNameShort = paste("Datasets/inputData/", config$handle, '/', config$handle, "_short.json", sep = "")

#Set parameters
options(width = 200)
'%ni%' = Negate('%in%')

#Import required libraries
#NOTE: Functions do not call these libraries; ideally should. 
lapply(config$packages, require, character.only=T)

#setup_twitter_oauth(consumer_key = config$consumerKey, consumer_secret = config$consumerSecret, access_token = config$accessToken, access_secret = config$accessTokenSecret)
#Call twitter functions. 
source(paste("twitterFunctions.R", sep = ""))
#library('ggalt')

#Load the NDTV data
rawData <- fromJSON(config$inputFileName, flatten=TRUE)
rawDataShort <- fromJSON(config$inputFileNameShort, flatten=TRUE)
config$varsToFetch = c("created_at", "id_str", "text", "retweet_count", "favorite_count", "possibly_sensitive", "entities.hashtags", "entities.urls")

rawData = rawData[which(rawData[,"user.screen_name"] == config$handle),config$varsToFetch]

rawData[,"weekDay"] = substr(rawData[,"created_at"], 0, 3)
rawData[,"month"] = substr(rawData[,"created_at"], 5, 7)
rawData[,"day"] = substr(rawData[,"created_at"], 9, 10)
rawData[,"year"] = substr(rawData[,"created_at"], 27, 30)
rawData[,"dayHour"] = substr(rawData[,"created_at"], 12, 13)
rawData[,"date"] = paste(rawData[,"day"], "", rawData[,"month"], "", rawData[,"year"], sep = "")
rawData[,"date"] = as.Date(rawData[,"date"], format = "%d%b%Y")

year_months = unique(paste(rawData[,"year"], "-", rawData[,"month"], sep = ""))
#year_months = c("2016-Jan", "2016-Feb", "2016-Mar", "2016-Apr", "2016-May", "2016-Jun", "2016-Jul", "2016-Aug", "2016-Sep", "2016-Oct", "2016-Nov", "2016-Dec", "2017-Jan")
#year_months = c("2016-Sep", "2016-Oct", "2016-Nov", "2016-Dec", "2017-Jan", "2017-Feb")
year_months = c("2017-May")
dates <- unique(rawData$date)

listOfMostOccuringWords = c()
counter = 0
handlePlotPath = paste(config$plotPath, "/", config$handle, "/", sep = "")
dir.create(file.path(handlePlotPath), showWarnings = FALSE)
commonWords = c("minister", "top", "stories", "world", "india", "pm", "BREAKING", "breaking", "BREAKINGNEWS", "news", "republic", "#breaking", "live", "people")

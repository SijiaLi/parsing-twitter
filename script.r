# Scarlett
# Dec 12, 2016
setwd("/Users/Scarlett/Desktop/Projects/twitter")
library(ROAuth)
library(streamR)
library(tm)
library(SnowballC)
library(wordcloud)
library(cluster)
library(RColorBrewer)
library(plyr)
library(leaflet)
library(rgdal)
source("latlong2state.r")


# ================ ROAuth =========================
library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "IqSkDmnKey0vVc7cVjKHT2n9s"
consumerSecret <- "23H1fGt0I6GUkZru4TSLGOS7PgS7PSlpiOhIlkbMCDFMNx9R9z"
my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                             requestURL = requestURL, accessURL = accessURL, authURL = authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(my_oauth, file = "my_oauth.Rdata")


# ================ stream =========================
library(streamR)
load("my_oauth.Rdata")
# get tweets from trend "StuffMyStockingWith
filterStream( file.name="wish.json",
              track = "StuffMyStockingWith", 
              locations = c(-125,25,-66,50),  
              timeout = 600, oauth=my_oauth)
wish.df <- parseTweets("wish.json")
save(wish.df, file = "wish.df.rds")

# ================ data cleaning =================
wish.df.text <- wish.df$text
clean <- function(x) {
  x <- iconv(x, "ASCII", "UTF-8", sub="")    # remove all emojis
  x <- gsub("@\\w+ *", "", x)
  x <- gsub("#\\w+ *", "", x)
  x <- gsub("\n", " ", x)
  x <- gsub("[^a-zA-Z #]","",x)    # "a-zA-Z #" are the things we need, [^a-zA-Z #] are the things to get rid of
  x <- gsub("https\\w+ *", "", x)
  x <- tolower(x)
  return(x)
}
cleantext <- clean(wish.df.text)


# ================ wordcloud ======================
library(tm)
library(SnowballC)
library(wordcloud)
library(cluster)
library(RColorBrewer)
library(plyr)

cleantext <- clean(wish.df.text)

wish.Corpus <- Corpus(VectorSource(cleantext))
wish.Corpus <- tm_map(wish.Corpus, removeWords, stopwords('english'))
wish.Corpus <- tm_map(wish.Corpus, removeWords, c('just', 'like', 'dont', 'get', 'one', 'amp', stopwords('english')))

wordcloud(wish.Corpus, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))


# ====== state-based geo info (frequency) for keyword "love" =======
source("latlong2state.r")
library(rgdal)

load("wish.df.rds")
wish.df$text <- cleantext

pos <- grep("love", wish.df$text)
love.df <- na.omit(wish.df[pos, c(1,15,37,38)])

love.loc <- (data.frame(love.df$place_lon, love.df$place_lat))
states <- latlong2state(love.loc)

freq.table <- as.data.frame(table(states))

dns <- "/Users/Scarlett/Desktop/Projects/twitter/USA_shapefile"
fn <- list.files(dns, pattern = ".shp", full.names = FALSE)
fn <- gsub(".shp", "", fn)
shape <- readOGR(dns, fn[2])

shape.state <- as.data.frame(tolower(shape$NAME_1))
state.freq <- merge(shape.state, freq.table, by.x = "tolower(shape$NAME_1)", by.y = "states", all = TRUE)


# ================ leaflet: interactive map ======================
library(leaflet)
library(rgdal)
dns <- "/Users/Scarlett/Desktop/Projects/twitter/USA_shapefile"
fn <- list.files(dns, pattern = ".shp", full.names = FALSE)
fn <- gsub(".shp", "", fn)
shape <- readOGR(dns, fn[2])
shape$Freq <- state.freq$Freq

i_popup <- paste0("<strong>State: </strong>", 
                  shape$NAME_1, 
                  "<br>",
                  "<strong>Love Score: </strong>", 
                  shape$Freq)

pal <- colorQuantile("PuRd", NULL, n = 5)    

leaflet(shape) %>% addTiles() %>%
  setView(-95, 37, zoom = 4) %>%
  addPolygons(fillColor = ~pal(shape$Freq), 
              fillOpacity = 0.8, 
              color = "#000000", 
              weight = 1, popup = i_popup)


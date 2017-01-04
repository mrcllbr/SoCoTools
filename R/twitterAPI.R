url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if (verbose) cat(address,"\n")
  u <- url(address)
  doc <- RCurl::getURL(u)
  x <- RJSONIO::fromJSON(doc,simplify = FALSE)
  if (x$status == "OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type  <- x$results[[1]]$geometry$location_type
    formatted_address  <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
    Sys.sleep(0.5)
  } else {
    return(c(NA,NA,NA, NA))
  }
}

# Pre:
#   qu: Query to the Twitter API
#   m: Maximum number of tweets returned
#   s: Tweets searching start date
#   e: Tweets searching end date
# Post: returns a maximum of m tweets based on the query qu
#       starting on the date s and ending on the date e
twitter_query <- function(qu, m, s, e) {
  # Solve locale date problems
  Sys.setlocale("LC_ALL","C")

  u = 'https://api.twitter.com/1.1/search/tweets.json'
  query = list(q = qu, count = m, until = toString(e))
  key = 'bRoccNWeJloQfOdzcnZJZM0hc'
  secret = 'NkGAX8MRcDFScgXGRQERcVpijWLfIrnnRPCFenxJSXtXpRWkyz'
  token = '591004240-fdT8mJtrfQpTkAwIeRCYzhho9lsJA0AP144g5LiJ'
  tokenSecret = 'mXwDRLF9vPLjEWfyjfp9LIrqmpBYKzFOWay9QHtG9kuMj'

  args = ROAuth:::signRequest(u, query, key, secret, token, tokenSecret)
  tweets_raw <- RCurl::getForm(u, .params = args, crlf = TRUE)
  #print(tweets_raw[1])
  tweets_parsed <- gsub('[\r\n]', '', tweets_raw[1])
  tweets_json <- RJSONIO::fromJSON(tweets_parsed, simplify = TRUE)
  #tweets_json <- jsonlite::fromJSON(tweets_parsed)

  #print(names(tweets_json[['statuses']][[1]]))
  #print(tweets_json[['statuses']][[1]][['user']][['name']])
  #p <- sapply(tweets_json[['statuses']], function(x) x[['user']][['name']])
  #print(p)
  #print(names(tweets_json$statuses))
  #print(tweets_json$statuses$retweeted_status)

  users <- sapply(tweets_json[['statuses']], function(x) x[['user']][['name']])
  locations <- sapply(tweets_json[['statuses']], function(x) x[['user']][['location']])
  #print(locations)
  #print(geoCode('Barcelona'))
  #geo <- geoCode(tweets_json$statuses$user$location)
  geo <- as.data.frame.matrix(t(sapply(X = locations, FUN = geoCode)))
  # First 35 characters of tweet text content
  #tweets <- substr(tweets_json$statuses$text, 0, 35)
  tweets <- sapply(tweets_json[['statuses']], function(x) x[['text']])
  timestamps <- strptime(sapply(tweets_json[['statuses']], function(x) x[['created_at']]), '%a %b %d %H:%M:%S %z %Y')
  result <- data.frame(User = users, Location = locations, Location_detected = geo$V4, Latitude = geo$V1, Longitude = geo$V2, Tweet = tweets, Date = timestamps)

  #print(nrow(result))
  result <- dplyr::filter(result, Date >= strptime(s, '%Y-%m-%d'))

  return(result)
}


prova <- function() {
  # Query to the Twitter API
  query = 'linux ubuntu'
  # Maximum number of tweets returned
  max = 100L
  d = as.Date(Sys.Date(), '%y-%m-%d')
  # Tweets searching start date
  start_date = d - 1
  # Tweets searching end date
  end_date = d
  result <- twitter_query(query, max, start_date, end_date)
  #print(result)

  x <- split(result, lubridate::hour(result$Date))

  #print(x)
  #print(as.data.frame.matrix(t(sapply(X = x, FUN = nrow))))
  nt <- paste(as.data.frame.matrix(t(sapply(X = x, FUN = nrow))), sep="")
  #print(nt)

  # Create data:
  a=c(names(x))
  b=c(nt)

  # Make a basic graph
  plot(b~a, type="b", bty="l", xlab="Hour of day", ylab="Number of tweets", col="green", lwd=1, pch=20, xlim=c(0, 24))


  # Add a legend
  # legend("bottomleft",
  #        legend = c("Group 1", "Group 2"),
  #        col = c(rgb(0.2,0.4,0.1,0.7),
  #                rgb(0.8,0.4,0.1,0.7)),
  #        pch = c(17,19),
  #        bty = "n",
  #        pt.cex = 2,
  #        cex = 1.2,
  #        text.col = "black",
  #        horiz = F ,
  #        inset = c(0.1, 0.1))
}





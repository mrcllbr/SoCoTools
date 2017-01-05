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
  result2 <- twitter_query('linux overflow', max, start_date, end_date)
  result3 <- twitter_query('linux unity', max, start_date, end_date)

  results <- list(result, result2, result3)
  first = TRUE
  count = 1
  for(result in results) {
    x <- split(result, lubridate::hour(result$Date))
    nt <- paste(as.data.frame.matrix(t(sapply(X = x, FUN = nrow))), sep="")
    #print(nt)

    a = c(names(x))
    b = c(nt)

    aux = as.data.frame.matrix(t(sapply(X = x, FUN = nrow)))
    print(aux)
    a = seq(0, 23, 1)
    #print(a)
    b = rep(0, 24)
    for(x in 1:length(aux)) {
      b[as.integer(names(aux[x]))+1] = as.integer(aux[1,x])
    }
    #print(b)

    if(first) {
      plot(b~a, type="b", bty="l", xlab="Hour of day", ylab="Number of tweets", col=count, lwd=1, pch=20, xlim=c(0, 24))
      first = FALSE
      count =+ 1
    }
    else {
      lines(b~a, col=count, lwd=1, pch=20, type="b")
    }
  }

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

prova2 <- function() {
  max = 100L
  d = as.Date(Sys.Date(), '%y-%m-%d')
  start_date = d - 1
  end_date = d
  querys <- list('linux ubuntu', 'linux overflow', 'linux unity')

  first = TRUE
  plot = ''
  a = seq(0, 23, 1)
  for(query in querys) {
    result <- twitter_query(query, max, start_date, end_date)
    split_result <- split(result, lubridate::hour(result$Date))
    split_result = as.data.frame.matrix(t(sapply(X = split_result, FUN = nrow)))

    b = rep(0, 24)
    for(x in 1:length(split_result)) {
      b[as.integer(names(split_result[x]))+1] = as.integer(split_result[1,x])
    }

    if(first) {
      plot<-paste('p<-plot_ly(y=', toString(list(b)), ', x=a , type="scatter", mode="lines", name="', toString(query), '")')
      first = FALSE
    }
    else {
      plot<-paste(plot, ' %>% add_trace(y=', toString(list(b)), ', x=a, type="scatter", mode="lines", name="', toString(query), '")')
    }
  }

  eval(parse(text=plot))
  p
}

prova3 <- function() {
  library(plotly)

  my_y=rnorm(10)*3
  my_x=seq(0,9)

  plot<-paste('p<-plot_ly(y=', toString(list(my_y)), ', x=my_x , type="scatter", mode="lines")')

  for(i in 1:3) {
    my_y=rnorm(10)
    plot<-paste(plot, ' %>% add_trace(y=~', toString(list(my_y)), ', x=~my_x, type="scatter", mode="lines")')
  }
  eval(parse(text=plot))
  p
}


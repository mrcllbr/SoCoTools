twitter_query <- function(qu, m, s, e) {
  u = 'https://api.twitter.com/1.1/search/tweets.json'
  query = list(q = qu, count = m, until = toString(e))
  key = 'bRoccNWeJloQfOdzcnZJZM0hc'
  secret = 'NkGAX8MRcDFScgXGRQERcVpijWLfIrnnRPCFenxJSXtXpRWkyz'
  token = '591004240-fdT8mJtrfQpTkAwIeRCYzhho9lsJA0AP144g5LiJ'
  tokenSecret = 'mXwDRLF9vPLjEWfyjfp9LIrqmpBYKzFOWay9QHtG9kuMj'

  args = ROAuth:::signRequest(u, query, key, secret, token, tokenSecret)
  tweets_raw <- RCurl::getForm(u, .params = args)
  tweets_json <- jsonlite::fromJSON(gsub('\n', '', tweets_raw[1]))

  #print(tweets_json$statuses)
  #print(names(tweets_json$statuses))
  #print(tweets_json$statuses$retweeted_status)

  users <- tweets_json$statuses$user$name
  locations <- tweets_json$statuses$user$location
  # First 35 characters of tweet text content
  #tweets <- substr(tweets_json$statuses$text, 0, 35)
  tweets <- tweets_json$statuses$text
  timestamps <- strptime(tweets_json$statuses$created_at, '%a %b %d %H:%M:%S %z %Y')
  result <- data.frame(User = users, Location = locations, Tweet = tweets, Date = timestamps)

  result <- dplyr::filter(result, Date >= strptime(s, '%Y-%m-%d'))

  return(result)
}

#geo tweets
prova <- function() {
  # Query to the Twitter API
  query = '#cybersecurity management'
  # Maximum number of tweets returned
  max = 100L
  d = as.Date(Sys.Date(), '%y-%m-%d')
  # Tweets searching start date
  start_date = d - 1
  # Tweets searching end date
  end_date = d
  result <- twitter_query(query, max, start_date, end_date)

  print(result)
}









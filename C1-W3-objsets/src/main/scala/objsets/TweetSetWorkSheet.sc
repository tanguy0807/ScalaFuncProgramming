import objsets._

val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

val googleTweets: TweetSet = TweetReader.allTweets.filter(tweet => google.exists(s => tweet.text.contains(s)))
val appleTweets: TweetSet = TweetReader.allTweets.filter(tweet => apple.exists(s => tweet.text.contains(s)))

// type(googleTweets.descendingByRetweet)

// googleTweets.foreach(println)

/**
 * A list of all tweets mentioning a keyword from either apple or google,
 * sorted by the number of retweets.
 */
val trending: TweetList = (googleTweets union appleTweets).descendingByRetweet
trending.foreach(t => println(t.retweets))

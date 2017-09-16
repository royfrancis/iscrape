# iscrape  

A basic web scraper for Instagram. 

Functions included in this package can retrieve the following information:

From __username__   

+ Number of posts
+ Number of followers
+ Number following

From __hashtag__  

+ Number of posts

## Installation  

Install R software for your system. Install dependency packages:

```r
install.packages(c("devtools","dplyr","httr","stringr"),dep=T)
```

Then install `iscrape` from GitHub using `devtools`

```r
devtools::install_github("royfrancis/iscrape")
```

## Walkthrough  

In this walkthough, we go through the steps of using the functions once the package has been installed.

We will use the username `instagram` to get information from this page:
https://www.instagram.com/instagram/

Get the user webpage.

```r
library(iscrape)

# get webpage
pu <- get_page_user("instagram")
```

From the webpage, get post count, follower count and following count.

```r
get_count_post(pu)
instagram 
     4729 
     
get_count_follower(pu)
instagram 
226200000 

get_count_following(pu)
instagram 
      197 
```

We will use the hashtag `instagram` to get information from this page:
https://www.instagram.com/explore/tags/instagram/

Get the hashtag webpage.

```r
# get webpage
ph <- get_page_hashtag("instagram")
```

From the webpage, get post count.

```r
get_count_hashtag(ph)
instagram 
171200000 
```

Use a loop to get data from multiple user names/hashtags.

```r
# a vector of valid username/hashtags
names <- c("instagram","instagramjapan","music")

klist <- list()
len <- length(names)
for(i in 1:len)
{
  cat(paste0("\nRunning ",i," of ",len,"; ",names[i],"; "))
  pu <- get_page_user(names[i])
  pcount <- get_count_post(pu)
  cat(pcount," ")
  fcount <- get_count_follower(pu)
  cat(fcount," ")
  focount <- get_count_following(pu)
  cat(focount,"; ")

  ph <- get_page_hashtag(names[i])
  hcount <- get_count_hashtag(ph)
  cat(hcount,";")

  klist[[i]] <- data.frame(name=names[i],posts=pcount,
                           followers=fcount,following=focount,
                           hashtagcounts=hcount,stringsAsFactors=F)

  # variation in timing page request
  Sys.sleep(sample(1:6,1,replace=T))
}
```

If a username/hashtag is invalid, the page could not be retrieved or the text matching fails, NA is returned. Therefore, if you get NAs, it is a good idea to check the page manually.

### Disclaimer  

Note that web scraping is strictly discouraged by Instagram. Do not use this tool to scraping thousands of pages or it is likely that your IP address may be blocked by Instagram. Speaking of IP addresses, it might be a good idea to use a VPN when using this tool.

This package was created for personal use, but you are free to use it. The package is very simple relying on text pattern matching. This means that the code is functional at the time of writing, but future changes to Instagram page structure may cause these functions to break. This package is unlikely to be updated or maintained regularly.

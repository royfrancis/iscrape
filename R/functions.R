# instascrape 1.0.0
# 16-Sep-2017

# FUNCTIONS

#' @title Get user page.
#' @description Gets the user page.
#' @param username A character denoting a valid instagram username.
#' @return Returns an httr webpage object text or NA.
#' @details
#' If the username is not valid, it does not return a webpage,
#' NA is returned with a warning (when \code{warn=T}).
#' @examples
#' # get page for username: instagram
#' pu <- get_page_user("instagram")
#' @seealso \code{\link{get_count_post}}, \code{\link{get_count_follower}}, \code{\link{get_count_following}}
#' @export
#'
get_page_user <- function(username=NULL,warn=TRUE)
{
  if(is.null(username)) stop("Input is empty.")
  if(!is.character(username)) stop("Input must be a character datatype.")
  if(length(username)>1) stop("Input must be of length one.")

  webpage <- httr::GET(paste0("https://www.instagram.com/",username,"/"))
  if(!http_error(webpage))
  {
    page_user <- httr::content(webpage,as="text",encoding="UTF-8")
  }else{
    if(warn) warning(paste0("Username ",username," did not return a valid web page."))
    page_user <- NA
  }

  attr(page_user,"names") <- username
  return(page_user)
}

#' @title Get post count from a user page.
#' @description Parses a text userpage and extracts post count.
#' @param userpage A user page. An output from \code{\link{get_page_user}}.
#' @return Returns an integer follower count or NA.
#' @details
#' If the parsing fails, NA is returned with a warning (when \code{warn=T}).
#' @examples
#' pu <- get_page_user("instagram")
#' cf <- get_count_post(pu)
#' @export
#'
get_count_post <- function(userpage=NULL,warn=TRUE)
{
  if(is.null(userpage)) stop("Input is empty.")

  up <- readLines(tc <- textConnection(userpage)); close(tc)
  wpt <- up[grep("[0-9,.km]+[ ]{1,}Posts",up)][2]
  if(length(wpt)!=0)
  {
    count_value <- stringr::str_extract(wpt,'[0-9,.km]+[ ]{1,}Posts') %>%
      stringr::str_replace('[ ]{1,}Posts',"") %>% stringr::str_replace(",","")

    mlpy <- 1
    if(stringr::str_detect(count_value,"k")) mlpy <- 1000
    if(stringr::str_detect(count_value,"m")) mlpy <- 1000000

    count_value <- count_value %>% stringr::str_replace("k|m","")
    if(stringr::str_detect(count_value,"[.]")) mlpy <- mlpy/10

    count_value <- count_value %>% stringr::str_replace("[.]","") %>% as.integer()
    count_value <- count_value*mlpy

  }else
  {
    if(warn) warning(paste0("Userpage did not return a post count. Instagram format may have changed."))
    count_value <- NA
  }

  attr(count_value,"names") <- attr(userpage,"names")
  return(count_value)
}

#' @title Get follower count from a user page.
#' @description Parses a text userpage and extracts follower count.
#' @param userpage A user page. An output from \code{\link{get_page_user}}.
#' @return Returns an integer follower count or NA.
#' @details
#' If the parsing fails, NA is returned with a warning (when \code{warn=T}).
#' @examples
#' pu <- get_page_user("instagram")
#' cf <- get_count_follower(pu)
#'
#' @export
#'
get_count_follower <- function(userpage=NULL,warn=TRUE)
{
  if(is.null(userpage)) stop("Input is empty.")

  up <- readLines(tc <- textConnection(userpage)); close(tc)
  wpt <- up[grep("[0-9,.km]+[ ]{1,}Followers",up)][1]
  if(length(wpt)!=0)
  {
    count_value <- stringr::str_extract(wpt,'[0-9,.km]+[ ]{1,}Followers') %>%
      str_replace('[ ]{1,}Followers',"") %>% str_replace(",","")

    mlpy <- 1
    if(stringr::str_detect(count_value,"k")) mlpy <- 1000
    if(stringr::str_detect(count_value,"m")) mlpy <- 1000000

    count_value <- count_value %>% stringr::str_replace("k|m","")
    if(stringr::str_detect(count_value,"[.]")) mlpy <- mlpy/10

    count_value <- count_value %>% stringr::str_replace("[.]","") %>% as.integer()
    count_value <- count_value*mlpy

  }else
  {
    if(warn) warning(paste0("Userpage did not return a follower count. Instagram format may have changed."))
    count_value <- NA
  }

  attr(count_value,"names") <- attr(userpage,"names")
  return(count_value)
}

#' @title Get following count from a user page.
#' @description Parses a text userpage and extracts following count.
#' @param userpage A user page. An output from \code{\link{get_page_user}}.
#' @return Returns an integer following count or NA.
#' @details
#' If the parsing fails, NA is returned with a warning (when \code{warn=T}).
#' @examples
#' pu <- get_page_user("instagram")
#' cf <- get_count_following(pu)
#'
#' @export
#'
get_count_following <- function(userpage=NULL,warn=TRUE)
{
  if(is.null(userpage)) stop("Input is empty.")

  up <- readLines(tc <- textConnection(userpage)); close(tc)
  wpt <- up[grep("[0-9,.km]+[ ]{1,}Following",up)][2]
  if(length(wpt)!=0)
  {
    count_value <- stringr::str_extract(wpt,'[0-9,.km]+[ ]{1,}Following') %>%
      stringr::str_replace('[ ]{1,}Following',"") %>% stringr::str_replace(",","")

    mlpy <- 1
    if(stringr::str_detect(count_value,"k")) mlpy <- 1000
    if(stringr::str_detect(count_value,"m")) mlpy <- 1000000

    count_value <- count_value %>% stringr::str_replace("k|m","")
    if(stringr::str_detect(count_value,"[.]")) mlpy <- mlpy/10

    count_value <- count_value %>% stringr::str_replace("[.]","") %>% as.integer()
    count_value <- count_value*mlpy

  }else
  {
    if(warn) warning(paste0("Userpage did not return a following count. Instagram format may have changed."))
    count_value <- NA
  }

  attr(count_value,"names") <- attr(userpage,"names")
  return(count_value)
}

#' @title Get hashtag page.
#' @description Gets the hashtag page.
#' @param hashtag A character denoting a valid instagram hashtag.
#' @return Returns an httr webpage object as text or NA.
#' @details
#' If the tag is not valid and/or if it does not return a webpage,
#' NA is returned with a warning (when \code{warn=T}).
#' @examples
#' # get page for hashtag: instagram
#' un <- get_page_hashtag("instagram")
#' @seealso \code{\link{get_count_hashtag}}
#' @export
#'
get_page_hashtag <- function(hashtag=NULL,warn=TRUE)
{
  if(is.null(hashtag)) stop("Input is empty.")
  if(!is.character(hashtag)) stop("Input must be a character datatype.")
  if(length(hashtag)>1) stop("Input must be of length one.")

  webpage <- httr::GET(paste0("https://www.instagram.com/explore/tags/",hashtag,"/"))
  if(!http_error(webpage))
  {
    page_hashtag <- httr::content(webpage,as="text",encoding="UTF-8")

  }else{
    if(warn) warning(paste0("Hashtag ",hashtag," did not return a valid web page."))
    page_hashtag <- NA
  }

  attr(page_hashtag,"names") <- hashtag
  return(page_hashtag)
}

#' @title Get hashtag count from a hashtagpage.
#' @description Parses a text hashtagpage and extracts hashtag count.
#' @param hastagpage A hashtagpage. An output from \code{\link{get_page_hashtag}}.
#' @return Returns a numeric follower count or NA.
#' @details
#' If the parsing fails, NA is returned with a warning (when \code{warn=T}).
#' @examples
#' ph <- get_page_hastag("instagram")
#' ch <- get_count_hastag(ph)
#'
#' @export
#'
get_count_hashtag <- function(hashtagpage=NULL,warn=TRUE)
{
  if(is.null(hashtagpage)) stop("Input is empty.")

  hp <- readLines(tc <- textConnection(hashtagpage)); close(tc)
  wpt <- hp[grep('[0-9,.km]+[ ]{1,}Posts',hp)]
  if(length(wpt)!=0)
  {
    count_value <- stringr::str_extract(wpt,'[0-9,.km]+[ ]{1,}Posts') %>%
      stringr::str_replace('[ ]{1,}Posts',"") %>% stringr::str_replace(",","")

    mlpy <- 1
    if(stringr::str_detect(count_value,"k")) mlpy <- 1000
    if(stringr::str_detect(count_value,"m")) mlpy <- 1000000

    count_value <- count_value %>% stringr::str_replace("k|m","")
    if(stringr::str_detect(count_value,"[.]")) mlpy <- mlpy/10

    count_value <- count_value %>% stringr::str_replace("[.]","") %>% as.integer()
    count_value <- count_value*mlpy
  }else
  {
    if(warn) warning(paste0("Hashtag page did not return a hashtag count. Instagram format may have changed."))
    count_value <- NA
  }

  attr(count_value,"names") <- attr(hashtagpage,"names")
  return(count_value)
}

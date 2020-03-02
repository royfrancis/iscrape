# iscrape 1.0.1
# FUNCTIONS

#' @title Get user page
#' @description Gets the user page
#' @param username A character denoting a valid instagram username.
#' @return Returns an httr webpage object text or NA.
#' @details
#' If the username is not valid, it does not return a webpage,
#' NA is returned with a warning.
#' @examples
#' # get page for username: instagram
#' pu <- get_page_user("instagram")
#' @seealso \code{\link{get_count_post}}, \code{\link{get_count_follower}}, \code{\link{get_count_following}}
#' @importFrom httr GET content http_error
#' @export
#'
get_page_user <- function(username=NULL) {
  if(is.null(username)) stop("Input is empty.")
  if(!is.character(username)) stop("Input must be a character datatype.")
  if(length(username)>1) stop("Input must be of length one.")

  webpage <- httr::GET(paste0("https://www.instagram.com/",username,"/"))
  if(!httr::http_error(webpage))
  {
    page_user <- httr::content(webpage,as="text",encoding="UTF-8")
  }else{
    warning(paste0("Username ",username," did not return a valid web page."))
    page_user <- NA
  }

  attr(page_user,"names") <- username
  return(page_user)
}

#' @title Get post count from a user page
#' @description Parses a text userpage and extracts post count
#' @param userpage A user page. An output from \code{\link{get_page_user}}.
#' @return Returns an integer follower count or NA.
#' @details
#' If the parsing fails, NA is returned with a warning.
#' @examples
#' pu <- get_page_user("instagram")
#' cf <- get_count_post(pu)
#' @importFrom stringr str_extract str_replace str_detect
#' @export
#'
get_count_post <- function(userpage=NULL) {
  if(is.null(userpage)) stop("Input is empty.")
  
  # method 1
  count_value <- stringr::str_extract(userpage,'[0-9,.km]+[ ]{1,}Posts') %>%
    stringr::str_replace('[ ]{1,}Posts',"") %>% stringr::str_replace(",","")
  
  # method 2
  if(is.na(count_value) | count_value=="") {
    count_value <- stringr::str_extract(stringr::str_extract(userpage,'edge_owner_to_timeline_media[:{"count]+[0-9]+'),'[0-9]+')
  }
  
  # if there is a valid number
  if(!is.na(count_value)) {
    if(count_value!="") {
      mlpy <- 1
      if(stringr::str_detect(count_value,"k")) mlpy <- 1000
      if(stringr::str_detect(count_value,"m")) mlpy <- 1000000
      
      count_value <- count_value %>% stringr::str_replace("k|m","")
      if(stringr::str_detect(count_value,"[.]")) mlpy <- mlpy/10
      
      count_value <- count_value %>% stringr::str_replace("[.]","") %>% as.integer()
      count_value <- count_value*mlpy
    }
  }
  
  
  if(is.na(count_value) | count_value=="") {
    warning(paste0("Userpage did not return a post count. Instagram format may have changed."))
    count_value <- NA
  }
  
  attr(count_value,"names") <- attr(userpage,"names")
  return(count_value)
}

#' @title Get follower count from a user page
#' @description Parses a text userpage and extracts follower count
#' @param userpage A user page. An output from \code{\link{get_page_user}}.
#' @return Returns an integer follower count or NA.
#' @details
#' If the parsing fails, NA is returned with a warning.
#' @examples
#' pu <- get_page_user("instagram")
#' cf <- get_count_follower(pu)
#' @importFrom stringr str_replace str_extract str_detect
#' @export
#'
get_count_follower <- function(userpage=NULL) {
  if(is.null(userpage)) stop("Input is empty.")
  
  # method 1
  count_value <- stringr::str_extract(userpage,'[0-9,.km]+[ ]{1,}Followers') %>%
    str_replace('[ ]{1,}Followers',"") %>% str_replace(",","")
  
  # method 2
  if(is.na(count_value) | count_value=="") {
    count_value <- stringr::str_extract(stringr::str_extract(userpage,'userInteractionCount":"[0-9]+'),'[0-9]+')
  }
  
  # method 3
  if(is.na(count_value) | count_value=="") {
    count_value <- str_extract(str_extract(userpage,'edge_followed_by[{count":]+[0-9]+\\}'),'[0-9]+')
  }
  
  
  # if there is a valid number
  if(!is.na(count_value)) {
    if(count_value!="") {
      mlpy <- 1
      if(stringr::str_detect(count_value,"k")) mlpy <- 1000
      if(stringr::str_detect(count_value,"m")) mlpy <- 1000000
      
      count_value <- count_value %>% stringr::str_replace("k|m","")
      if(stringr::str_detect(count_value,"[.]")) mlpy <- mlpy/10
      
      count_value <- count_value %>% stringr::str_replace("[.]","") %>% as.integer()
      count_value <- count_value*mlpy
    }
  }
  
  if(is.na(count_value) | count_value=="") {
    warning(paste0("Userpage did not return a follower count. Instagram format may have changed."))
    count_value <- NA
  }
  
  attr(count_value,"names") <- attr(userpage,"names")
  return(count_value)
}

#' @title Get following count from a user page
#' @description Parses a text userpage and extracts following count
#' @param userpage A user page. An output from \code{\link{get_page_user}}.
#' @return Returns an integer following count or NA.
#' @details
#' If the parsing fails, NA is returned with a warning.
#' @examples
#' pu <- get_page_user("instagram")
#' cf <- get_count_following(pu)
#' @importFrom stringr str_replace str_extract str_detect
#' @export
#'
get_count_following <- function(userpage=NULL) {
  if(is.null(userpage)) stop("Input is empty.")
  
  # method 1
  count_value <- stringr::str_extract(userpage,'[0-9,.km]+[ ]{1,}Following') %>%
    stringr::str_replace('[ ]{1,}Following',"") %>% stringr::str_replace(",","")
  
  # method 2
  if(is.na(count_value) | count_value=="") {
  count_value <- stringr::str_extract(stringr::str_extract(userpage,'edge_follow[:{"count]+[0-9]+'),'[0-9]+')
  }
  
  if(!is.na(count_value)) {
    if(count_value!="") {
      mlpy <- 1
      if(stringr::str_detect(count_value,"k")) mlpy <- 1000
      if(stringr::str_detect(count_value,"m")) mlpy <- 1000000
      
      count_value <- count_value %>% stringr::str_replace("k|m","")
      if(stringr::str_detect(count_value,"[.]")) mlpy <- mlpy/10
      
      count_value <- count_value %>% stringr::str_replace("[.]","") %>% as.integer()
      count_value <- count_value*mlpy
    }
  }
  
  if(is.na(count_value) | count_value=="") {
    warning(paste0("Userpage did not return a following count. Instagram format may have changed."))
    count_value <- NA
  }
  
  attr(count_value,"names") <- attr(userpage,"names")
  return(count_value)
}

#' @title Get user page info
#' @description Gets the three metrics from a user page
#' @param username A character vector of one or more valid instagram usernames.
#' @return Returns a dataframe with post counts, follower count and following count.
#' @details
#' If the username is not valid, an empty data.frame is returned
#' @examples
#' # get page for username: instagram
#' pu <- get_page_info("instagram")
#' @seealso \code{\link{get_count_post}}, \code{\link{get_count_follower}}, \code{\link{get_count_following}}
#' @importFrom stringr str_replace str_extract str_detect
#' @importFrom httr content GET http_error
#' @importFrom dplyr bind_rows
#' @export
#'
get_page_info <- function(username=NULL) {
  if(is.null(username)) stop("Input is empty.")
  if(!is.character(username)) stop("Input must be a character datatype.")
  
  fun1 <- function(username){
    webpage <- httr::GET(paste0("https://www.instagram.com/",username,"/"))
    if(!httr::http_error(webpage))
    {
      pu <- httr::content(webpage,as="text",encoding="UTF-8")
      return(data.frame(username=username,
                        posts=get_count_post(pu),
                        followers=get_count_follower(pu),
                        following=get_count_following(pu),
                        stringsAsFactors=FALSE))
    }else{
      warning(paste0("Username ",username," did not return a valid web page."))
      return(data.frame(username=username,
                        posts=NA,
                        followers=NA,
                        following=NA,
                        stringsAsFactors=FALSE))
    }
  }
  
  l <- lapply(username,fun1)
  return(dplyr::bind_rows(l))
}

#' @title Get hashtag page
#' @description Gets the hashtag page
#' @param hashtag A character denoting a valid instagram hashtag.
#' @return Returns an httr webpage object as text or NA.
#' @details
#' If the tag is not valid and/or if it does not return a webpage,
#' NA is returned with a warning.
#' @examples
#' # get page for hashtag: instagram
#' un <- get_page_hashtag("instagram")
#' @seealso \code{\link{get_count_hashtag}}
#' @importFrom httr content GET http_error
#' @export
#'
get_page_hashtag <- function(hashtag=NULL) {
  if(is.null(hashtag)) stop("Input is empty.")
  if(!is.character(hashtag)) stop("Input must be a character datatype.")
  if(length(hashtag)>1) stop("Input must be of length one.")

  webpage <- httr::GET(paste0("https://www.instagram.com/explore/tags/",hashtag,"/"))
  if(!httr::http_error(webpage))
  {
    page_hashtag <- httr::content(webpage,as="text",encoding="UTF-8")

  }else{
    warning(paste0("Hashtag ",hashtag," did not return a valid web page."))
    page_hashtag <- NA
  }

  attr(page_hashtag,"names") <- hashtag
  return(page_hashtag)
}

#' @title Get hashtag count from a hashtagpage
#' @description Parses a text hashtagpage and extracts hashtag count
#' @param hashtagpage A hashtagpage. An output from \code{\link{get_page_hashtag}}.
#' @return Returns a numeric follower count or NA.
#' @details
#' If the parsing fails, NA is returned with a warning.
#' @examples
#' ph <- get_page_hashtag("instagram")
#' ch <- get_count_hashtag(ph)
#' @importFrom stringr str_replace str_extract str_detect
#' @export
#'
get_count_hashtag <- function(hashtagpage=NULL) {
  if(is.null(hashtagpage)) stop("Input is empty.")
  
  # method 1
  count_value <- stringr::str_extract(hashtagpage,'[0-9,.km]+[ ]{1,}Posts') %>%
    stringr::str_replace('[ ]{1,}Posts',"") %>% stringr::str_replace(",","")
  
  # method 2
  if(is.na(count_value) | count_value=="") {
    count_value <- stringr::str_extract(stringr::str_extract(hashtagpage,'edge_hashtag_to_media[:{"count]+[0-9]+'),'[0-9]+')
  }
  
  if(!is.na(count_value)) {
    if(count_value!="") {
      mlpy <- 1
      if(stringr::str_detect(count_value,"k")) mlpy <- 1000
      if(stringr::str_detect(count_value,"m")) mlpy <- 1000000
      
      count_value <- count_value %>% stringr::str_replace("k|m","")
      if(stringr::str_detect(count_value,"[.]")) mlpy <- mlpy/10
      
      count_value <- count_value %>% stringr::str_replace("[.]","") %>% as.integer()
      count_value <- count_value*mlpy
    }
  }
  
  if(is.na(count_value) | count_value=="") {
    warning(paste0("Hashtag page did not return a hashtag count. Instagram format may have changed."))
    count_value <- NA
  }
  
  attr(count_value,"names") <- attr(hashtagpage,"names")
  return(count_value)
}


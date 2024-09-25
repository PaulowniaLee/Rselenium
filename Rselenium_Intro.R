#### Environment ####
library(tidyverse)
library(httr)
library(rvest)
library(RSelenium)

setwd("/Users/mac/Documents/R_Notes/rvest")

#####

#### Target ####
# latlong case 
# 1. type in our address
# 2. click find button 
# 3. scrape the results in latitude and longitude boxes 

# 找element的部分参见Find html elements from html source
# 这里直接上selector
#####

#### connect ####
#eCaps <- list(chromeOptions = list(
#  args = c('--headless', '--disable-gpu')
#))
# headless is faster, but needs special attention 
# (you have to scroll down or sth to make what you want loaded)

eCaps <- list(chromeOptions = list(
 args = c('--disable-gpu', "--no-sandbox",
          '--disable-dev-shm-usage')
))
# 后面两个参数用来解决chrome的page crash
remDr <- remoteDriver(
  remoteServerAdd = "localhost",
  port = 4445L,
  browser = "chrome",
  extraCapabilities = eCaps
)
remDr$open()
#####

#### No loop ####
remDr$navigate("https://www.latlong.net/convert-address-to-lat-long.html")
Sys.sleep(6) # give some time to load


# bot detected
backEle <- remDr$findElement(
  using = "tag name", value = "p"
)
backEle$clickElement()
# 这个是有效的
remDr$screenshot(file = "test.png") 
# take a screenshot, see what selenium is doing 
# 对故障时的诊断很有用


# type 
addEle <- remDr$findElement(
  using = "class", value = "width70")
#  using = c("xpath", "css selector", "id", 
# "name", "tag name", "class name", "link text", "partial link text")
# 这里用id, 是在browser上用java找的
addEle$sendKeysToElement(list("Lombard Street, San Francisco"))
# 不用那几个特殊键就是直接打字进去


# click find 
btnEle <- remDr$findElement(
  using = "id", value = "btnfind"
)
btnEle$clickElement()


# get coordinates 
# we want lat long specifically 
page <- remDr$getPageSource()[[1]]
lat_long <- page %>%
  read_html() %>%
  html_elements(".coordinatetxt") %>%
  html_text() %>%
  .[1]

# now let's test

#####

#### loop ####
street_names <- c("Lombard Street, San Francisco", 
                  "Santa Monica Boulevard", 
                  "Bourbon Street, New Orleans", 
                  "Fifth Avenue, New York", 
                  "Richards Street, Vancouver")
# more streets to check 

remDr$navigate("https://www.latlong.net/convert-address-to-lat-long.html")
Sys.sleep(6)

# bot detected
backEle <- remDr$findElement(
  using = "tag name", value = "p"
)
backEle$clickElement()
# 这个是有效的
remDr$screenshot(file = "test.png") 
# take a screenshot, see what selenium is doing 

lat_long <- vector("list", length(street_names))

for(i in 1:length(street_names)){
  
  # progress indicator 
  cat("*")
  
  # type 
  addEle <- remDr$findElement(
    using = "class", value = "width70")
  addEle$sendKeysToElement(list(street_names[i]))
  Sys.sleep(1)
  
  # click find 
  btnEle <- remDr$findElement(
    using = "id", value = "btnfind"
  )
  btnEle$clickElement()
  Sys.sleep(3)
  
  # output screenshot 
  filename <- list("test_", i, ".png")
  remDr$screenshot(file = paste0(filename, collapse = ""))
  
  # get coordinates 
  # we want lat long specifically 
  page <- remDr$getPageSource()[[1]]
  lat_long[[i]] <- page %>%
    read_html() %>%
    html_elements(".coordinatetxt") %>%
    html_text() %>%
    .[1]
  
  remDr$refresh()
  Sys.sleep(3)
}
#####

# final:
# 网站免费名额用完了，所以没查到。不过程序是成功的，还干掉了一个简单的anti-bot
# 第二天重试程序，成功了
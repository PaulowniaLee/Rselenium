#### Environment ####
library(tidyverse)
library(httr)
library(rvest)
library(RSelenium)

setwd("/Users/mac/Documents/R_Notes/rvest")

#####

#### Connect #### 
eCaps <- list(chromeOptions = list(
  args = c('--disable-gpu', "--no-sandbox",
           '--disable-dev-shm-usage',
           "--headless")
))
# 第二和第三个参数用来解决chrome的page crash
# 这次只需要找动态element，无需点按，也不用下载page source
# 用headless没有区别
remDr <- remoteDriver(
  remoteServerAdd = "localhost",
  port = 4445L,
  browser = "chrome",
  extraCapabilities = eCaps
)
remDr$open()
#####

#### Selenium + rvest ####

# get urls 
urls <- vector("list", 40)
urls[[1]] <- paste0("https://sothebysrealty.ca/en/search-results/",
"region-greater-vancouver-province-british-columbia-real-estate/",
"price-0-1000000/status-1/ptype-1/")
urls[2:40] <- sapply(2:40, function(x){
  url <- paste0("https://sothebysrealty.ca/en/search-results/",
                "region-greater-vancouver-province-british-columbia-real-estate/",
                "price-0-1000000/status-1/page-")
  namelist <- c(url, x, "/ptype-1/")
  paste0(namelist, collapse = "")
})

# get all links (for properties, on every page)
df_all <- data.frame()
for (i in 1:(length(urls))){
  
  # navigate
  remDr$navigate(urls[[i]])
  Sys.sleep(10)
  photoname <- c("test_", i, ".png")
  remDr$screenshot(file = paste0(photoname, collapse = ""))
  
  # get links
  links <- remDr$findElements(
    using = "css selector", 
    value = ".plink")
  df <- sapply(links, function(x){x$getElementAttribute("href")}) %>%
    unlist()
  df <- data.frame(link = df)
  Sys.sleep(1)
  
  # rbind
  df_all <- rbind(df_all, df)
  
  # progress indicator 
  if (i%%10==0) {cat("*")} else {cat(".")}
  # 如果是10的倍数，就打一个*，不然就打一个.
}

# converting links to characters 
urls <- sapply(df_all$link, as.character)

# get house information 
# df_all_data <- data.frame()

# this part is purely rvest, works anyway
# a <- 923
# k1 <- 1
# k2 <- length(urls)
for (i in a:a){
  
  tryCatch({
  # open
  t0 <- Sys.time()
  page <- read_html(urls[i])
  t1 <- Sys.time()
  delay <- as.numeric(t1 - t0)
  t <- sample(1:7, size = 1) 
  Sys.sleep(t*delay) 
  
  # sometimes session just won't work

  {
    # price 
    price <- page %>%
      html_elements(".price_social") %>%
      html_text() %>%
      .[1]
  
    # address 
    address <- page %>%
      html_elements("div [class='cnt normal flex-sb'] > span > p") %>%
      html_text() %>%
      .[1]
    
    # square feet 
    square_feet <- page %>%
      html_elements("[class='quick_facts'] > li") %>%
      html_text() %>%
      .[3] 
    
    # keyfacts 
    key_facts <- page %>%
      html_elements("[class='key_facts'] > li") %>%
      html_text()
    key_facts <- key_facts %>%
      str_replace_all(".*: ", "") %>% #删掉name,留value
      set_names(nm = 
                  str_replace_all(key_facts, ":.*", "") %>% #删掉value,留name
                  str_replace_all("[0-9]+", "") %>% #删掉name里的数字
                  str_trim()
      ) 
    
    # keyfacts for each house can be different
    # we fill in NA when there is no such observation 
    {
      # property type 
      building_type <- ifelse("Property Type" %in% names(key_facts),
                              key_facts["Property Type"], NA)
      
      # year built 
      year_built <- ifelse("Year Built" %in% names(key_facts),
                           key_facts["Year Built"], NA)
      
      # number of bedrooms
      bedrooms <- ifelse("Bedrooms" %in% names(key_facts),
                         key_facts["Bedrooms"], NA)
      
      # number of bathrooms 
      bathrooms <- ifelse("Bathrooms" %in% names(key_facts),
                          key_facts["Bathrooms"], NA)
      
      # property tax 
      taxes <- ifelse("Municipal Taxes" %in% names(key_facts),
                      key_facts["Municipal Taxes"], NA)
      }
    
  }
  
  # compile 
  df_single <- data.frame(
    price = price,
    address = address,
    squares = square_feet,
    type = building_type,
    year = year_built,
    bed = bedrooms,
    bath = bathrooms,
    tax = taxes
  )
  
  # rbind 
  df_all_data <- rbind(df_all_data, df_single)
  },
  
  error = function(e){
    message(paste("URL does not seem to exist:", i))
    message("Here's the original error message:")
    message(e)
  })
  
  # progress indicator 
  #if (i < 370) {cat(".")}
  if (i%%50==0) {cat(" *",i)} 
  
}

{ua_list <- c(paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
                    "AppleWebKit/537.36 (KHTML, like Gecko)",
                    "Chrome/127.0.0.0 Safari/537.36"),
             paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit",
                    "/537.36 (KHTML, like Gecko) Chrome",
                    "/127.0.0.0 Safari/537.36"),
             paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_",
                    "15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version",
                    "/17.6 Safari/605.1.15"),
             paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:129.0) Gecko",
                    "/20100101 Firefox/129.0"),
             paste0("Mozilla/5.0 (X11; Linux x86_64) AppleWebKit",
                    "/537.36 (KHTML, like Gecko) Chrome",
                    "/127.0.0.0 Safari/537.36")
             )}
random_ua <- sample(ua_list, size = 1)
my_ua <- user_agent(random_ua)
#my_session <- session("https://www.google.com", my_ua)

#####

#### Append and Save ####
write_csv(df_all_data, file = "Canadian_houses.csv")
links <- data.frame(link = urls)
rownames(links) <- NULL
write_csv(links, file = "Canadian_houses_links.csv")

# 调试区


# remove(list=setdiff(ls(), c("remDr", "urls", "df_all")))
# remove(remDr)

# error handler
# problem with url 
urls[1240]
# whether appending is successful 
tail(df_all_data, n = 1)

#####

A very successful combination of Selenium and rvest. 1280+ links are collected from a dynamic sites by Selenium and each of these links are scrapped by rvest. 
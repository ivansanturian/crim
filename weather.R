setwd("C:/Users/ivans/OneDrive/Desktop/rstuff-CRIM402/Rdata")
library(lubridate)

# test for one day
a <- scan("https://www.farmersalmanac.com/weather-history-results/zipcode-60656/2010/01/14/", what="", sep="\n", encoding="UTF-8")

grep("High Temp", a)
i <- grep("High Temp", a)
a[i]

highTemp <- gsub(".*High Temp:</th><td>([0-9.-]*).*","\\1",a[i])
precip <- gsub(".*Precipitation Amount:</th><td>([0-9.]*).*","\\1",a[i])

data0 <- data.frame(highTemp, precip, date=ymd("2010/01/14"))
data0


# test for next day
a <- scan("https://www.farmersalmanac.com/weather-history-results/zipcode-60656/2010/01/15/", what="", sep="\n", encoding="UTF-8")

grep("High Temp", a)
i <- grep("High Temp", a)
a[i]

highTemp <- gsub(".*High Temp:</th><td>([0-9.-]*).*","\\1",a[i])

if(grepl("Precipitation Amount", a[i]))
{
precip <- gsub(".*Precipitation Amount:</th><td>([0-9.]*).*","\\1",a[i])
} else
{
  precip <- "0"
}

data0 <- data.frame(highTemp, precip, date=ymd("2010/01/15"))
data0

# FOR LOOP STUFF

dates.list <- seq(ymd("2010-01-01"), ymd("2010-01-31"), by="days")

# We're going to make a list of data frames to collect the results
#    vector("list",[Number goes here])
#    results[[i]] will contain the movie data for date i
results <- vector("list", length(dates.list))

for(i.date in 1:length(dates.list))
{
  # print out our progress
  print(dates.list[i.date])
  
  url.text <- paste0("https://www.farmersalmanac.com/weather-history-results/zipcode-60656/",
                     gsub("-","/",dates.list[i.date]))
  
  repeat
  {
    a <- try(scan(url.text,what="",sep="\n", encoding = "UTF-8"))
    if(class(a)=="try-error")
    {
      Sys.sleep(10)
    }
    else
    {
      break
    }
  }
  
  
  i <- grep("High Temp", a)
  
  highTemp <- gsub(".*High Temp:</th><td>([0-9.-]*).*","\\1",a[i])
  
  if(grepl("Precipitation Amount", a[i]))
  {
    precip <- gsub(".*Precipitation Amount:</th><td>([0-9.]*).*","\\1",a[i])
  } else
  {
    precip <- "0"
  }
  
  data0 <- data.frame(highTemp, precip, date=dates.list[i.date])
  data0
  
  results[[i.date]] <- data0
  Sys.sleep(3)
}
weather.data <- do.call(rbind, results)
save(weather.data, file = "farmersalmanacdata.Rdata")

install.packages("rvest")
library(rvest)

url <- "https://www.farmersalmanac.com/weather-history-results/zipcode-60656/2010/01/14/"

tab <- read_html(url) %>%
  html_table() %>%
  .[[1]]
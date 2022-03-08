library(lubridate)

#    Consider http://www.the-numbers.com/box-office-chart/daily/2019/07/04
a <- scan("http://www.the-numbers.com/box-office-chart/daily/2019/07/04",
          what="",sep="\n", encoding ="UTF-8")
a[1:10]
a[150:200]

# movie data table has "<TABLE" near the beginning and "</table" near the end
i.table.start <- grep("<table",a)[2]
i.table.end   <- grep("</table",a)[2]

i.table.start
i.table.end

# the lines with movie titles all have "/movie/"
i <- grep("/movie/",a)
i

# but some lines with "/movie/" occur before or after the movie table
i <- i[i>i.table.start & i<i.table.end]

# here are the lines with movie titles
a[i]

# strip away the HTML tags
gsub("<[^>]*>","",a[i])

# Class exercise: extract the movie gross
# Hint: The lines with movie gross are always 2 lines after the movie titles
k <- i + 2
a[k]
gsub("<[^>]*>|[,$]","",a[k])


# create a data frame with movie titles and gross
data0 <- data.frame(movie=gsub("<[^>]*>","",a[i]),
                    gross=gsub("<[^>]*>|[,$]","",a[k]),
                    date =ymd("2019-07-04")
                    )


# extract movie data from ALL dates
# use R to generate a sequence of dates (see ?seq.POSIXit)
dates.list <- seq(ymd("2010-01-01"), ymd("2021-10-01"), by="days")

# We're going to make a list of data frames to collect the results
#    vector("list",[Number goes here])
#    results[[i]] will contain the movie data for date i
results <- vector("list", length(dates.list))

for(i.date in 1:length(dates.list))
{
  # print out our progress
  print(dates.list[i.date])

  url.text <- paste0("http://www.the-numbers.com/box-office-chart/daily/",
                     gsub("-","/",dates.list[i.date]))

  a <- scan(url.text,what="",sep="\n", encoding = "UTF-8")
  # if scan() does not work (common on Macs), like you get "403 Forbidden" errors use this slightly more complicated approach
  # library(httr) # this goes at the top
  # resp <- GET(url.text, user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/537.13+ (KHTML, like Gecko) Version/5.1.7 Safari/534.57.2"))
  # a <- content(resp, as="text")
  # a <- strsplit(a,"\n")[[1]]

  # search for the table
  i.table.start <- grep("<table",a)[2]
  i.table.end   <- grep("</table",a)[2]

  # check whether there's any movie data
  if(i.table.end-i.table.start > 5)
  {
    # find movies
    i <- grep("/movie/",a)
    i <- i[i>i.table.start & i<i.table.end]

    # get movie names
    data0 <- data.frame(movie = gsub("<[^>]*>","",a[i]),
                        gross = as.numeric(gsub("<[^>]*>|[,$]","",a[i+2])),
                        date  = dates.list[i.date])

    results[[i.date]] <- data0
  }
  else
  {
    cat("Skipping\n")
  }
}
library(mailR)
send.mail(from = "ivansanturian@gmail.com",
          to = c("ivansanturian@gmail.com"),
          subject = "Movies",
          body = "R has finished downloading all the movie data",
          smtp = list(host.name="smtp.gmail.com",
                      port     =465,
                      user.name="", # add your username
                      passwd   ="", # and password
                      ssl      =TRUE),
          authenticate = TRUE,
          send = TRUE)


# combine the results into one dataset
#   equivalent to rbind(results[[1]],results[[2]],...
#   allocating and deallocating memory is expensive...this does it once
movie.data <- do.call(rbind, results)


# parallel implementation
#   http://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf
library(doParallel)

detectCores()

cl <- makeCluster(2)
registerDoParallel(cl)

# should take 10*2=20 seconds
system.time( # time how long this takes
  foreach(i=1:10) %do% # run not in parallel
  {
     Sys.sleep(2)  # wait for 2 seconds
     return(i)
  }
)

# with two processors should take about 10 seconds
system.time( # time how long this takes
  foreach(i=1:10) %dopar% # run in parallel
  {
    Sys.sleep(2)  # wait for 2 seconds
    return(i)
  }
)

stopCluster(cl)

# get movie data using multiple processors
#   probably around 3 minutes
cl <- makeCluster(8)
registerDoParallel(cl)

timeStart <- Sys.time() # record the starting time
results <-
foreach(i.date=1:length(dates.list)) %dopar%
{
  url.text <- paste0("http://www.the-numbers.com/box-office-chart/daily/",
                     gsub("-","/",dates.list[i.date]))

  a <- scan(url.text,what="",sep="\n")

  # search for the table
  i.table.start <- grep("<table",a)[2]
  i.table.end   <- grep("</table",a)[2]

  # check whether there's any movie data
  if(i.table.end-i.table.start > 5)
  {
    # find movies
    i <- grep("/movie/",a)
    i <- i[i>i.table.start & i<i.table.end]

    # get movie names
    data0 <- data.frame(movie = gsub("<[^>]*>","",a[i]),
                        gross = as.numeric(gsub("<[^>]*>|[,$]","",a[i+2])),
                        date  = dates.list[i.date])
  }
  else
  { # if the page has no movie data
    data0 <- NULL
  }

  return(data0)
}
# calculate how long it took
timeEnd <- Sys.time()
timeEnd-timeStart

stopCluster(cl)


movie.data <- do.call(rbind,results)

# add day of week
movie.data$day.of.week <- wday(movie.data$date,label=TRUE)

# fun questions
# biggest single day take
movie.data[which.max(movie.data$gross),]
# biggest totals
a <- aggregate(gross~movie,data=movie.data, sum)
a[order(a$gross,decreasing = TRUE)[1:10],]
# gross by day of the week
aggregate(gross~day.of.week, data=movie.data, sum)

# inflation adjust
#   based on average movie ticket prices
a <- data.frame(year=2010:2021,
                adjustment=c(1.25,1.22,1.19,1.17,1.16,
                             1.15,1.14,1.12,1.09,1.07,
                             1.06,1.00))
i <- match(year(movie.data$date), a$year)
movie.data$grossAdj <- movie.data$gross * a$adjustment[i]

movie.data[which.max(movie.data$grossAdj),]
a <- aggregate(grossAdj~movie,data=movie.data, sum)
a[order(a$grossAdj,decreasing = TRUE)[1:10],]

# format(123456789, big.mark=",") puts commas in numbers


setwd("C:/Users/ivans/OneDrive/Desktop/rstuff-CRIM402/Rdata")
save(movie.data,file="movie revenue.RData",compress=TRUE)

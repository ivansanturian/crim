### PRILIMINARY STUFF ###
# load library for workign with SQL databases
library(sqldf)
library(rstudioapi)

# packages for creating maps
library(ggmap)
library(hexbin)

# Set working directory
setwd("C:/Users/ivans/OneDrive/Desktop/rstuff-CRIM402/Rdata")

# connect to the database
con <- dbConnect(SQLite(), dbname="philcrime.db")

# connect to google api key for ggmap
register_google(key = "AIzaSyDK2unry72zSRUYjuGkqkN2U5PfDQSOXrA")

### INITIAL BUILDING OF DATABASE ###
# working with crime data from Phildelphia, from 2006 - present
# https://www.opendataphilly.org/dataset/crime-incidents/resource/79134de9-56fa-41f2-b529-b660aaf1539b
scan(what="",file="philcrimes.csv", nlines=100, sep="\n")

# create a Database
con <- dbConnect(SQLite(), dbname="philcrime.db")
dbWriteTable(con, "crime",
             "philcrimes.csv",
             row.names=FALSE,
             header=TRUE,
             sep=",")


### SOME TESTING ###
# test to see if stuff works
res <- dbSendQuery(con, "
                   SELECT *
                   FROM crime")
dbFetch(res, n = 10) # just the first 10 rows
dbClearResult(res)

res <- dbSendQuery(con, "
                   SELECT *
                   FROM crime
                   WHERE text_general_code='Thefts'")
dbFetch(res, n = 10) # just the first 10 rows
dbClearResult(res)

# some crimes are way outside philadelphia
dbSendQuery(con,"
            UPDATE crime SET lng=null
            WHERE lng < -76
            ")
dbSendQuery(con,"
            UPDATE crime SET lat=null
            WHERE lat < 35
            ")

# some values for lng are still 0
dbSendQuery(con,"
            UPDATE crime SET lng=null
            WHERE lng = 0
            ")

# more attempts at the above
dbSendQuery(con,"
            UPDATE crime SET lat=null
            WHERE (lat='') OR (ABS(lat-0.0)<0.01)
            ")

dbSendQuery(con,"
            UPDATE crime SET lng=null
            WHERE (lng='') OR (ABS(lng-0.0)<0.01)
            ")


### PLOTTING 
# now that the database is created, start thinking about what we will plot. 

# store all longitude and latitude entries in dataframe a
# this returns a warning message that not all values in 'lng' and 'lat' columns
# are the same, perhaps leads to issue with creating a map
res <- dbSendQuery(con, "
   SELECT lng,lat
   FROM crime
   ")
a <- dbFetch(res, n = -1)
is(a)
tail(table(a$lng))
head(table(a$lat))
a[1:10,]
dbClearResult(res)


# consider running this first
plot(a$lng, a$lat,
     pch=".",
     xlab="Longitude", ylab="Latitude")

# create a map of philadelphia using GGmap
# for philly, latitude = pos, longitude = neg
phil.map <- ggmap(get_map(c(-75.5, 39.5, -74.5, 40.5), zoom = 10, scale = "auto", source = "stamen"))

phil.map


# add some points to the map... is it useful?
i <- sample(1:nrow(a),1000)
phil.map +
   geom_point(aes(x=lng,y=lat), data=a[i,],
              alpha=0.5, color="darkred", size = 1)

# customize with your own color scale (See colors())
phil.map +
   coord_cartesian() +
   stat_binhex(aes(x=lng, y=lat),
               bins=60, alpha = 2/4, data = a) +
   scale_fill_gradient('Crime Density',low="springgreen",high="hotpink")

# THE ACTUAL HOTSPOT MAP
phil.map +
   stat_density2d(aes(x=lng,y=lat,fill=..level..,alpha=..level..),
                  data = a[i,], geom = 'polygon') +
   scale_fill_gradient('Crime Density',low="green",high="red") +
   scale_alpha(range = c(.4, .75), guide = "none") +
   guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))






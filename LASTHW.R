# load packages that we need
library(lubridate)
library(sf)
library(jsonlite)
library(geojsonsf)

# set WD
setwd("C:/Users/ivans/OneDrive/Desktop/rstuff-CRIM402/Rdata")

# create geocodeARCGIS, will need it later
geocodeARCGIS <- function(address)
{
  a <- gsub(" +", "\\%20", address)
  a <- paste0("https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates?f=json&singleLine=",
              a,
              "&outFields=Match_addr,Addr_type")
  return( jsonlite::fromJSON(a) )
}

# create PPDmap, will need it for question 2
PPDmap <- geojson_sf("http://data.phl.opendata.arcgis.com/datasets/8dc58605f9dd484295c7d065694cdc0f_0.geojson")

# load both datasets
load("PPD OIS geocode safe.RData")
load("PPD OIS.RData")



# see what both datasets look like
head(ois)
head(gcOIS) 

# 1. shootings that were possibly imprecisely geocoded
table(gcOIS$loctype)

a <- subset(gcOIS, loctype %in% c("Locality", "StreetName"))

# LOCALITY

i <- which(gcOIS$loctype=="Locality")
gcOIS[i,]
ois$text[i]
ois[i,] # 12-06, 09-25, 09-27, 09-76, 08-35, 07-27, 16-26h

# Read through all "Locality" location types and none of their descriptions involve 
# any information regarding location. ois[379,] was purposely withheld.

# STREETNAME

i <- which(gcOIS$loctype=="StreetName")
gcOIS[i,]
ois$text[i]
ois[i,]

# 08-26 is a withheld address
# 08-38 is a withheld address

# The rest of the "StreetName" location types were mainly user input error or general
# vagueness when inputting the location into the system.


# FIXING GEOCODES
a <- textConnection(
  "addressOrig,                         addressFix
  60000 Christian St,                   6050 Christian St
  100 block of W. City Ave,             City Ave & N 53rd St 
  block of Lancaster Ave,               5450 Lancaster Ave
  16th Warton St,                       1650 Morris Street
  50th Walton,                          5000 Catharine St
  54th Wyalusing Ave,                   5400 Wyalusing Ave
  4th Wingohocking,                     W Wingohocking St & N 4th St
  Arrott Street near Frankford Avenue,  1550 Arrott St
  2400 block of Glenwood Drive,         2400 Glenwood Ave
  ")
a <- read.csv(a, strip.white = TRUE)
# make sure the original addresses match, no NAs!
i <- match(a$addressOrig, ois$location)
i

b <- lapply(paste0(a$addressFix,", Philadelphia, PA"), geocodeARCGIS)
b <- lapply(b, function(x)
{
  data.frame(lon=x$candidates$location[1,"x"],
             lat=x$candidates$location[1,"y"],
             score=x$candidates$score[1],
             loctype=x$candidates$attributes$Addr_type[1],
             addressGeo=x$candidates$attributes$Match_addr[1])
})
b <- do.call(rbind, b)
gcOIS[i,] <- b
gcOIS[i,]

# After this fix, 2 address geocoded incorrectly (both intersections)
# Also, attempting to recode incident ois[41, ]

a <- textConnection(
  "addressOrig,                         addressFix
   100 block of W. City Ave,            1 Oakland Terrace
   4th Wingohocking,                    400 W Wingohocking St
   65th St Phila,                       65th St
  ")
a <- read.csv(a, strip.white = TRUE)
# make sure the original addresses match, no NAs!
i <- match(a$addressOrig, ois$location)
i

b <- lapply(paste0(a$addressFix,", Philadelphia, PA"), geocodeARCGIS)
b <- lapply(b, function(x)
{
  data.frame(lon=x$candidates$location[1,"x"],
             lat=x$candidates$location[1,"y"],
             score=x$candidates$score[1],
             loctype=x$candidates$attributes$Addr_type[1],
             addressGeo=x$candidates$attributes$Match_addr[1])
})
b <- do.call(rbind, b)
gcOIS[i,] <- b
gcOIS[i,]


# Review + other re-geocoding?
table(gcOIS$loctype)

# localities and streetnames left are purposely ommitted
subset(gcOIS, loctype %in% c("Locality", "StreetName"))

# closer look at "StreetAddressExt"
subset(gcOIS, loctype=="StreetAddressExt")
ois[c(161, 383),]
# conclusion: they work, dunno why they are "Ext"

# done w question 1, save both files
save(ois, file = "PPD OIS geocode safe.RData")
save(gcOIS, file = "PPD OIS.RData")


# 2. OIS transported to Hospital at University of Pennsylvania ("HUP")
#                    or Penn Presbyterian Hospital ("Presby")
# apparently HUP shut down their LVL 1 trauma center in 2015 and moved it to Presby

?grepl
grepl("hospital|Hospital", ois$text)
ois[489,]

i <- grepl("hospital|Hospital", ois$text)
ois[i,]

# 12-39 went to HUP

ois[grepl("Presbyterian", ois$text),]
ois[grepl("University of Pennsylvania", ois$text),]

# IMPORTANT
oisTest <- ois
oisTest$text <- gsub("\n"," ",oisTest$text)


oisTest[grepl("University of Pennsylvania", oisTest$text),]
a <- grepl("University of Pennsylvania", oisTest$text) & grepl("Hospital|hospital", oisTest$text)
grepl("Hospital of the University of Pennsylvania", oisTest$text)
grepl("Hospital at the University of Pennsylvania", oisTest$text)

# after some testing and understanding of limitations, following regular expression will
# account for all incidents going to HUP or Presby

iHUP <- grepl("Hospital of the University of Pennsylvania", oisTest$text)
iPresby <- grepl("Presbyterian", oisTest$text)

gcOIS$hospital <- NA
gcOIS$hospital[iHUP] <- "HUP"
gcOIS$hospital[iPresby] <- "Presby"

#A. Create a map marking the location of HUP and "Presby", the location of officer-involved 
#   shootings resulting in the offender being transported to HUP or Presby, and the locations 
#   of all other shootings
plot(st_geometry(PPDmap))

# making gcOIS a SF 
a <- st_as_sf(gcOIS,
              coords=c("lon","lat"),
              crs=4326)

plot(st_geometry(PPDmap))
plot(st_geometry(a), add=TRUE, col="red", pch=16)
gcOIS <- a

# CLEAR PLOT FIRST
plot(st_geometry(PPDmap))

# plotting OIS where hospital isnt HUP or presby -> PURPLE
plot(st_geometry(subset(gcOIS, is.na(hospital))),
     add=TRUE,
     col=rgb(139/255, 71/255, 137/255, 0.5),
     pch=19)

# PRESBY PLOT

# plotting OIS where hospital is Presby -> ORANGE
plot(st_geometry(subset(gcOIS, hospital=="Presby")),
     add=TRUE,
     col=rgb(1, 130/255, 71/255, 0.5),
     pch=19,
     cex=1.5)

# Plotting exact Presby hospital -> Blue w red outline
a <- geocodeARCGIS("51 N 39th St, Philadelphia, PA 19104")
points(a$candidates$location[1,],
       col="red", bg="blue", pch=21, cex=2)
text(a$candidates$location[1,], " Presby", adj=0, cex=0.75)

# HUP PLOT
# plotting OIS where hospital is Presby -> ORANGE
plot(st_geometry(subset(gcOIS, hospital=="HUP")),
     add=TRUE,
     col=rgb(0, 130/255, 0, 0.5),
     pch=19,
     cex=1.5)

# Plotting exact HUP hospital -> Red w blue outline
a <- geocodeARCGIS("3400 Spruce St, Philadelphia, PA 19104")
points(a$candidates$location[1,],
       col="blue", bg="red", pch=21, cex=2)
text(a$candidates$location[1,], " HUP", adj=0, cex=0.75)


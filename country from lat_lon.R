
reverseGeoCode <- function(latlng) {
  require("XML")
  require("httr")
  latlng    <- as.numeric(latlng)
  latlngStr <- gsub(' ','%20', paste(round(latlng,2), collapse=","))
  url   <- "http://maps.google.com"
  path  <- "/maps/api/geocode/xml"
  query <- list(sensor="false",latlng=latlngStr)
  response <- GET(url, path=path, query=query)
  if (response$status !=200) {
    print(paste("HTTP Error:",response$status),quote=F)
    return(c(NA,NA))
  }
  xml    <- xmlInternalTreeParse(content(response,type="text"))
  status <- xmlValue(getNodeSet(xml,"//status")[[1]])
  if (status != "OK"){
    print(paste("Query Failed:",status),quote=F)
    return(c(NA,NA))
  }
  xPath   <- '//result[1]/address_component[type="country"]/long_name[1]'
  country <- xmlValue(getNodeSet(xml,xPath)[[1]])
  xPath   <- '//result[1]/address_component[type="administrative_area_level_1"]/long_name[1]'
  state   <- xmlValue(getNodeSet(xml,xPath)[[1]])
  return(c(state=state,country=country))
}

loc <- read.csv("/Users/arable/Desktop/device_locations.csv", stringsAsFactors = F)


st.cntry <- t(apply(loc,1,function(x)reverseGeoCode(x[5:6])))
result   <- cbind(df,st.cntry)
result

reverseGeoCode(loc[1, 5:6])



library(sp)
library(rworldmap)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # convert our list of points to a SpatialPoints object
  
  # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  indices$ADMIN  
  #indices$ISO3 # returns the ISO3 code 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}

loc$country <- coords2country(loc[,c(6,5)])
write.csv(loc, "/Users/arable/Desktop/loc_with_countres.csv")

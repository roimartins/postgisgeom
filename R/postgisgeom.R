#' PostGIs Polygons
#'
#' This function convert postgis polygons into r spatial objects 
#'
#' @param us User Databse Name
#' @param pw Password
#' @export
#' @examples
#' postgisgeom(us="Tomas",pw = "Greif12")
#' postgisgeom(us="Your", pw = "Name12")


#install_github("klutometis/roxygen")

new_change <- "uno"

#library(devtools)



library(RODBC)
library(RPostgreSQL)
library(rgeos)          # Geostatistical Package developed for R platform /  geometry operations
library(rgdal)

### get polygons from mpa areas ###



postgisgeom <- function(us, pw, envel ) {
  
  
  
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  con <- dbConnect(drv, dbname = "postgis",
                   host = "localhost", port = 5433,
                   user = us, password = pw)
  
  rm(pw) #removes the password
  
  
  
  rr <- dbGetQuery(con, "select gid,site_name, st_asewkt(geom) as geom from geography_general.ospar_mpas_simples")
  
  dbDisconnect(con)
  dbUnloadDriver(drv)
  
  
  geom <- rr$geom
  geomdesc <- rr$gid
  geomname<- rr$site_name
  
  spgeom <- strsplit(rr[, "geom"],";")
  spgeomprj <- lapply(spgeom, function (x) substr(x[1],6, nchar(x[1])))
  spgeomg <- lapply(spgeom, function (x) x[2])
  
  spmpadf <- rr[,c("gid","site_name")]
  row.names(spmpadf) <- spmpadf$gid
  
  geoms <- lapply(spgeomg, function (x) readWKT(x,p4s=CRS(paste0("+init=epsg:",spgeomprj))))
  
  listpol<-list()
  
  for (i in 1:length(geoms)) {
    geoms[[i]]@polygons[[1]]@ID <- as.character(geomdesc[i]  )
    listpol[[i]]<-geoms[[i]]@polygons[[1]]
  }
  
  spmmpas <- SpatialPolygons(listpol)
  proj4string(spmmpas) <- CRS("+init=epsg:4326")
  spmmpasdf <- SpatialPolygonsDataFrame(spmmpas,spmpadf, match.ID = T)
  
  return(spmmpasdf)
  
}

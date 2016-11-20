library(ggmap)
library(sp)
library(maptools)
library(rgdal)
library(ggplot2)
library(plyr)
library(rgeos)
library(extrafont) #do czcionek
library(SDMTools)
library(splancs)
library(FNN)
library(geosphere) #do obliczenia odleg³osci

loadfonts(device="win")
czcionka="Corbel"
kolory<-c("#FFD5B1", "#F99D4A", "#CD5C06") #skala kolor?w 
kolor_punktow <- "#6B0000" #  "#00CCBE"
Kolor_tla <- "#FFFFFF" #  "#f6f6f6" 

#Pozwolenia - mapy
# setwd("D:/KK/OneDrive/Wroclaw w Liczbach/Gotowe projekty/Dostempnosc transportowa")

#excel z wsp?lrzednymi bydynk?w
slupki <- read.table("SlupkiWspolrzedne.txt", sep=";")  
names(slupki)<-c("lon", "lat", "id", "kat" )
slupki$lon<-as.numeric(gsub(",", ".",slupki$lon))
slupki$lat<-as.numeric(gsub(",", ".",slupki$lat))

#granice osiedli
mapa2<- readOGR("D:/KK/OneDrive/Wroclaw w Liczbach/Granice/GraniceOsiedli", layer = "GraniceOsiedli") 
mapa2@proj4string # note projection details
mapa3 <- spTransform(mapa2, CRS("+proj=longlat +datum=WGS84"))
mapa3@proj4string # and after transforming
area.points <- fortify(mapa3)


n <- 20000
lon <- runif(n, min(area.points[,1]), max(area.points[,1]))
lat <- runif(n, min(area.points[,2]), max(area.points[,2]))
points <- cbind(lon, lat)

pn <- matrix(c(0,0),1,2)
    
for(i in 0:47){
    points2 <- pnt.in.poly(points, area.points[area.points$id==i,1:2])
    points2 <- subset(points, points2[,3]==1)
    pn <- rbind(pn, points2)
}

pn <- as.data.frame(pn)
pn <- pn[-1,]

#obliczenie odleg³oœci od przystanków
odleglosc<- 99999999

for (i in 1:nrow(pn)){
  odl<-99999999
  odl_temp <- data.frame()
  for (j in 1:nrow(slupki)){
    odl_temp <- distm (c(pn$lon[i], pn$lat[i] ), c( slupki$lon[j], slupki$lat[j]), fun = distHaversine)
    odl[j] <- odl_temp
  }
  odleglosc[i]<-min(odl)
}



#dodanie liczby ludnoœci do ka¿dego punktu
#ZAludnienie2014 - wczytanie shapów
Zaludnienie2014<- readOGR("D:/KK/OneDrive/Wroclaw w Liczbach/Gotowe projekty/Pozwolenia na budowe/dem-rejurb-rejstat", layer = "REJSTAT_20141231") 
Zaludnienie2014@proj4string # note projection details
Zaludnienie2014_2 <- spTransform(Zaludnienie2014, CRS("+proj=longlat +datum=WGS84"))
Zaludnienie2014_2@proj4string # and after transforming
Zaludnienie2014_2@data$id <- rownames(Zaludnienie2014_2@data)
Zaludnienie2014_2.df <- fortify(Zaludnienie2014_2)


#dopisanie gêstosci zaludnienia
gestosc_zal <- data.frame(Zaludnienie2014_2@data$LUD_NA_KM2)
gestosc_zal$id <- 0:578
names(gestosc_zal) <- c("LUD_NA_KM2", "id") 
gestosc_zal_z_ksztaltem<-join(Zaludnienie2014_2.df, gestosc_zal, by="id")

gestosc_zal_z_ksztaltem$lat <- gestosc_zal_z_ksztaltem$lat * Skalar

pkt <- pn

pn <- data.frame(matrix(c(0,0,0,0,0),1,5))
names(pn) <- c("lon", "lat", "odl", "odlkat", "LUD_NA_KM2")

for(i in 0:578){
  
  points2 <- pnt.in.poly(pkt[,1:2], gestosc_zal_z_ksztaltem[gestosc_zal_z_ksztaltem$id==i,1:2])
  points2 <- subset(pkt, points2[,3]==1)
  if ( nrow(points2) > 0 ) {
    points2$LUD_NA_KM2 <- gestosc_zal[i+1,1]
    pn <- rbind(pn, points2)
  }
  
}



pn <- pn[-1,]


#wielkoœæ Wroc³awia
wielkosc_km2 <- sum (Zaludnienie2014_2@data$SHAPE_Area) /1000000
sr_wielkosc_km2_na_pkt <- wielkosc_km2/nrow(pn)

pn$ile_ludzi <- pn$LUD_NA_KM2 * sr_wielkosc_km2_na_pkt


write.csv2(pn, file = "D:/KK/OneDrive/Wroclaw w Liczbach/Gotowe projekty/Dostempnosc transportowa/pkt.csv")

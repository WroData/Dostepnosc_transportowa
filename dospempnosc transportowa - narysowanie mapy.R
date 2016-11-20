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


pn <- read.csv2("pkt.csv", header = T)  

pn$odlkat<- ifelse(pn$odl<300, "<300m",ifelse(pn$odl<750, "300-750m",">750m"))
pn$odlkat<-factor(pn$odlkat, c("<300m" , "300-750m" , ">750m", "Przystanek" ))


#Przeskalowanie Y
Skalar <- 1  /   ((18.27/13.86)*((max(area.points$lat)-min(area.points$lat))/(max(area.points$long)-min(area.points$long))))
slupki$lat <- slupki$lat * Skalar
area.points$lat <- area.points$lat * Skalar


pn$odlkatkol<- ifelse(pn$odl<300, "#FFD5B1",ifelse(pn$odl<750, "#F99D4A","#CD5C06"))

kolory<-c("#FFD5B1", "#F99D4A", "#CD5C06") #skala kolor?w 
pn$odlkat<-factor(pn$odlkat, c("<300m" , "300-750m" , ">750m", "Przystanek" ))
sztucznypkt <- c(1000, slupki[1,1], slupki[1,2] , 0,"Przystanek",0,0,"#6B0000" )

pn <-rbind(pn , sztucznypkt)
pn$lon<-as.numeric(pn$lon)
pn$lat<-as.numeric(pn$lat)

kolor1 <- "#FFD5B1"
kolor2 <- "#F99D4A"
kolor3 <- "#CD5C06"
kolor_punktow <- "#6B0000" #  "#00CCBE"

mapa <- ggplot()+ 
  geom_point(data = pn, aes(x = lon, y = lat, colour=odlkat ), size=2)   +
  scale_color_manual(name = "Odleg³oœæ w linii prostej od\nnajblizszego przystanku", values = c("<300m" = kolor1,
                    "300-750m" = kolor2, ">750m" = kolor3, "Przystanek" = kolor_punktow), labels = c("<300m", "300-750m", ">750m", "Przystanek"))  +
  guides(colour = guide_legend(labels =NA,title = "Odleg³oœæ w linii prostej od\nnajbli¿szego przystanku",override.aes = list(alpha = 1, size=3))) +
  geom_point(data = slupki, aes(x = lon, y = lat, size="Przystanek", colour = "hjkdas"), size =2 ,  alpha = 0.6, col=kolor_punktow) +
  #guides(colour = guide_legend(labels ="Przystanek ",title = NULL,override.aes = list(alpha = 1, size=3))) +
  geom_polygon(aes(x = long, y = lat, group = group), data = area.points,  color = "gray", fill = NA, alpha = 0.1) +
  coord_equal() + 
  theme_bw()+
  labs(title="\nDostêpnoœæ transportowa Wroc³awia")+
  theme(panel.background = element_rect(fill = Kolor_tla),
        plot.background=element_rect(fill=Kolor_tla), #t?o najbardziej na zwen?trz
        panel.grid.major = element_blank(),#element_line(linetype="solid",color="#e0e0e0"), #wi?ksza siatka
        panel.grid.minor = element_blank(), #mniejsza siatka
        panel.border = element_blank(), #osie
        panel.border=element_rect(color="black"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        plot.title=element_text( size=25,family=czcionka,color="black"),
        axis.ticks.y=element_blank(),
        plot.margin = unit(c(0,0,0,0), "lines"),
        legend.background=element_rect(fill=Kolor_tla,linetype="solid",color="black"),
        legend.position = c(0.155, 0.20),
        legend.text=element_text(family=czcionka, size=13),
        legend.title=element_text(family=czcionka, size=15),
        plot.title=element_text(size=18,family=czcionka ), 
        legend.key=element_blank()) 

mapa
 



png(filename="mapa_wrzutka.png", bg=Kolor_tla, width = 14, height = 10, units = 'in', res = 500)
plot(mapa)
dev.off()
#warnings()
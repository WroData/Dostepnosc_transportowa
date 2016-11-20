library(ggplot2)
library(extrafont) #do czcionek

#####################################################################
#czeœæ transformacji dokonanych w Excelu


pkt <- read.csv2("wroclawianie2.csv", header = T )  

pkt2 <- cbind(pkt, rep(1,26))
names (pkt2) <- c("przedizal","ponizej_ilu", "ile_os", "procent", "skumulowany", "flaga") 
pkt2$flaga <- as.factor(pkt2$flaga)
pkt2$ile_os <- as.numeric(as.character(pkt2$ile_os))
pkt2$procent <- as.numeric(as.character(pkt2$procent))
pkt2$skumulowany <- as.numeric(as.character(pkt2$skumulowany)) 

loadfonts(device="win")
czcionka="Corbel"
kolory<-c("#FFD5B1", "#F99D4A", "#CD5C06") #skala kolor?w 
kolor_punktow <- "#F99D4A" #  "#00CCBE"
Kolor_tla <- "#FFFFFF" #  "#f6f6f6" 
kolor_wew_tla <- "#FFFFFF" #  "#f6f6f6" 
kolorTekstu<-c("black")

przekrojowy<-ggplot(pkt2[1:10,], aes(x=reorder(przedizal, -ponizej_ilu),y = skumulowany*100, fill = flaga), fill = kolor_punktow)+
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_manual(values=kolor_punktow) +
  labs(title = "   ", x="odleg³oœæ od przystanku",y="procent ludnoœci")+
  theme_bw() +
  theme(panel.background = element_rect(fill = kolor_wew_tla),
      plot.background=element_rect(fill=Kolor_tla), 
      panel.grid.minor = element_blank(), 
      axis.title=element_text( size=13 ,family=czcionka,color=kolorTekstu),
      axis.text=element_text(size=15,color=kolorTekstu,family=czcionka),
      plot.title=element_text(size=18,family=czcionka,color=kolorTekstu), 
      plot.margin = unit(c(0,1,1,1), "lines")) + 
  guides(fill=FALSE)


przekrojowy

png(filename="wykres_wrzutka.png", bg=Kolor_tla, width = 14, height = 5, units = 'in', res = 500)
przekrojowy
dev.off()
  
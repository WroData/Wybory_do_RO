library(rgdal)
library(ggplot2)
library(rgeos)
library(plyr)
library(sp)
library(extrafont) #do czcionek
loadfonts(device="win")

#Pozwolenia - mapy
setwd("D:/KK/OneDrive/Wroclaw w Liczbach/Gotowe projekty/Wybory do rad osiedla")

#excel z ilo�ci� kandydat�w
kandydaci <- read.csv2("kanydaci.csv",  header = TRUE, stringsAsFactors=FALSE)  

#granice osiedli
osiedla<- readOGR("D:/KK/OneDrive/Wroclaw w Liczbach/Gotowe projekty/Wybory do rad osiedla/GraniceOsiedli", layer = "GraniceOsiedli") 
osiedla <- spTransform(osiedla, CRS("+proj=longlat +datum=WGS84"))
osiedla@data
area.points <- fortify(osiedla)

#Nazwy osiedli
Nazwy_Osiedli<-data.frame((as.character(osiedla$NAZWAOSIED)))
names(Nazwy_Osiedli)<-c("Nazwy" )
Nazwy_Osiedli[,1]<-(as.character((Nazwy_Osiedli[,1])))
Encoding(Nazwy_Osiedli[,1]) <- "UTF-8"
Nazwy_Osiedli[,2] <- seq(0,47,1)
names(Nazwy_Osiedli)<-c("Nazwy", "id" )

#Dodanie nazw osiedli do ich granic 
area.points <- join(area.points, Nazwy_Osiedli, by="id")
#Dodanie informacji o kandydatach do granic osiedli 
area.points <- join(area.points, kandydaci, by="Nazwy")
area.points$zapelnienie <- area.points$il.kandydatow/area.points$prog 
area.points$zapelnienie_kat <- ifelse(area.points$zapelnienie == 1,  "1", 
                                      ifelse(area.points$zapelnienie < 1.5, "(1; 1,5)",
                                      ifelse(area.points$zapelnienie < 2,   "<1,5; 2)",
                                      ifelse(area.points$zapelnienie < 2.5, "<2; 2,5)", ">=2,5"))))
area.points$zapelnienie_kat <- factor(area.points$zapelnienie_kat, c("1", "(1; 1,5)","<1,5; 2)", "<2; 2,5)", ">=2,5"))
area.points$sr.wiek_kat     <- ifelse(area.points$sr.wiek < 40,  "<40", 
                                      ifelse(area.points$sr.wiek < 45, "<40; 45)",
                                      ifelse(area.points$sr.wiek < 50, "<45; 50)",
                                      ifelse(area.points$sr.wiek < 55, "<50; 55)", ">=55"))))
area.points$sr.wiek_kat     <- factor(area.points$sr.wiek_kat, c("<40", "<40; 45)", "<45; 50)", "<50; 55)", ">=55"))

#Srodki osiedli - aby p�niej doda� nazwy do wykresu 
�rodki_osiedli <- data.frame(gCentroid(osiedla,byid=TRUE))
�rodki_osiedli <- cbind(�rodki_osiedli, Nazwy_Osiedli )
�rodki_osiedli <- �rodki_osiedli[order(�rodki_osiedli$Nazwy), ]
names(�rodki_osiedli)<-c("long", "lat", "Nazwy")


#przeskalowanie mapy
Skalar <- 1  /   ((18.27/13.86)*((max(area.points$lat)-min(area.points$lat))/(max(area.points$long)-min(area.points$long))))
area.points$lat <- area.points$lat * Skalar
�rodki_osiedli$lat <- �rodki_osiedli$lat * Skalar


#okre�lenie zmiennych do wykresu
czcionka="Corbel" 
kolory6<-c("#ffffd4", '#fee391','#fec44f','#fe9929','#d95f0e','#993404') #skala 6 kolor�w 
kolory5<-c('#ffffd4','#fed98e','#fe9929','#d95f0e','#993404') #skala 5 kolor�w 
kolory5_odw<-c('#993404','#d95f0e','#fe9929','#fed98e','#ffffd4') #skala 5 kolor�w 
kolor_punktow <- "#6B0000" #  "#00CCBE"
Kolor_tla <- "#FFFFFF" #  "#f6f6f6" 

plot_list = list()
##############################################################################
## wykres ilo�� kandydat�w na miejsce
p = ggplot()+ 
  geom_polygon(aes(fill = zapelnienie_kat, x = long, y = lat, group = group),  data = area.points, alpha = 1,  color =  "gray50", size = 0.5) +
  coord_equal() + 
  theme_bw() +
  labs(title="\nIlo�� kandydat�w na liczb� mandat�w w wyborach do Rad Osiedli 2017") +
  geom_text(data=�rodki_osiedli, aes(x=long, y=lat, label=Nazwy),family=czcionka,  size=4, color= "black")  +
  scale_fill_manual(values = kolory5, guide = guide_legend(title = "Ilo�� kandydat�w\nna miejsce")) +
  theme(panel.background = element_rect(fill = Kolor_tla),
        plot.background=element_rect(fill=Kolor_tla), #t�o najbardziej na zwen�trz
        panel.grid.major = element_blank(),#element_line(linetype="solid",color="#e0e0e0"), #wi�ksza siatka
        panel.grid.minor = element_blank(), #mniejsza siatka
        panel.border = element_blank(), #osie
#        panel.border=element_rect(color="black"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        plot.title=element_text(size=25,family=czcionka,color="black", hjust = 0.5),
        axis.ticks.y=element_blank(),
        plot.margin = unit(c(0,0,0,0), "lines"),
#        legend.background=element_rect(fill=Kolor_tla,linetype="solid",color="black"),
        legend.text=element_text(family=czcionka, size=13),
        legend.title=element_text(family=czcionka, size=15),
        legend.key=element_blank()) 
plot_list[[1]] = p

##############################################################################
## wykres �redniej wieku
p = ggplot()+ 
  geom_polygon(aes(fill = sr.wiek_kat, x = long, y = lat, group = group),  data = area.points, alpha = 1,  color =  "gray50", size = 0.5) +
  coord_equal() + 
  theme_bw() +
  labs(title="\n�redni wiek kandydat�w w wyborach do Rad Osiedli 2017") +
  geom_text(data=�rodki_osiedli, aes(x=long, y=lat, label=Nazwy),family=czcionka,  size=4, color= "black")  +
  scale_fill_manual(values = kolory5, guide = guide_legend(title = "�redni na osielu\nwiek kandydat�w")) +
  theme(panel.background = element_rect(fill = Kolor_tla),
        plot.background=element_rect(fill=Kolor_tla), #t�o najbardziej na zwen�trz
        panel.grid.major = element_blank(),#element_line(linetype="solid",color="#e0e0e0"), #wi�ksza siatka
        panel.grid.minor = element_blank(), #mniejsza siatka
        panel.border = element_blank(), #osie
        #        panel.border=element_rect(color="black"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        plot.title=element_text(size=25,family=czcionka, color="black", hjust = 0.5),
        axis.ticks.y=element_blank(),
        plot.margin = unit(c(0,0,0,0), "lines"),
        #        legend.background=element_rect(fill=Kolor_tla,linetype="solid",color="black"),
        legend.text=element_text(family=czcionka, size=13),
        legend.title=element_text(family=czcionka, size=15),
        legend.key=element_blank()) 
plot_list[[2]] = p

##############################################################################
## rozk��d wieku kandydat�w
pelne_listy<- read.csv2("pe�ne listy.csv",  header = TRUE, stringsAsFactors=FALSE)  

p = ggplot(pelne_listy, aes(Wiek)) + 
  geom_histogram(aes(y = ..count..), colour = "grey50", binwidth = 1) + 
  geom_density(aes(y = ..density..*1300), colour = "#bd0026", fill =  "#bd0026", alpha = 0.1 ) +
  labs(title="\nRozk�ad wieku kandydat�w w wyborach do Rad Osiedli 2017",
       x="Wiek\n", y="\nIlo�� kandydat�w") +
  theme(panel.background = element_rect(fill = Kolor_tla),
        plot.background=element_rect(fill=Kolor_tla), #t�o najbardziej na zwen�trz
        panel.grid.major = element_line(linetype="solid",color="grey95"), #wi�ksza siatka
        panel.grid.minor = element_blank(), #mniejsza siatka
        panel.border = element_blank(), #osie
        #        panel.border=element_rect(color="black"),
        axis.title=element_text(family=czcionka, size=18),
        axis.text=element_text(family=czcionka, size=16),
        plot.title=element_text(size=25,family=czcionka, color="black", hjust = 0.5),
        plot.margin = unit(c(0,0,0,0), "lines"))

plot_list[[3]] = p

##############################################################################
## zapisyqanie jako png
png(filename="Ilosc kandydatoq na miejsce.png", bg=Kolor_tla, width = 14, height = 10, units = 'in', res = 300)
plot(plot_list[[1]] )
dev.off()

png(filename="�r qiek kandydatoq na osiedlach.png", bg=Kolor_tla, width = 14, height = 10, units = 'in', res = 300)
plot(plot_list[[2]] )
dev.off()

png(filename="Rozklad qieku.png", bg=Kolor_tla, width = 14, height = 10, units = 'in', res = 300)
plot(plot_list[[3]] )
dev.off()
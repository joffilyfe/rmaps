library(maps)
library(mapdata)
library(maptools) #Ler ESRI shapefiles
library(RColorBrewer)
library(ggplot2)
library(dplyr)
require("rgdal")
require("plyr")
# library(ggmap)
# library(rworldmap)

setwd("~/IFPB/TCC/mapas")
# Carrega a quantidade de casos de dengue por municipio no ano de 2016
casos_2016 <- read.csv("numero_casos_2016.csv")

par(mar=c(1,1,1,1))
map("world","Brazil")
# map(,,add=T)
map.axes()
map.scale(ratio = FALSE, cex = 0.7)
# abline(h = 0, lty = 2)
map.cities(country = "Brazil", minpop = 3000000, pch=19, cex=1.2)

# Carrega shape files
paraiba <- readShapePoly("./pb_municipios/25MUE250GC_SIR.shp")


# Projeta os dados
proj4string(paraiba)

# Seta a escala
par(mar=c(0,0,0,0))

# Plota o gráfico
plot(paraiba)

# O shape carregado em memoria sempre tem um digito a mais para os municipios, é preciso removelo
paraiba$ID_MUNICIP <- as.numeric(substr(as.character(paraiba$CD_GEOCMU), 1, 6))

# João Pessoa com código normalizado
paraiba$ID_MUNICIP==250750

# Adiciona cor a um município
plot(paraiba[paraiba$NM_MUNICIP=="JOÃO PESSOA",], add=T, col = "red")
plot(paraiba[paraiba$NM_MUNICIP=="GUARABIRA",], add=T, col = alpha("green", 0.5))

# Adiciona nomes aos municípios
#pts.PB <- getSpatialPolygonsLabelPoints(paraiba)
# text(pts.PB@coords, labels=paraiba$NM_MUNICIP)
# map.scale(y=-22.78,ratio=F,cex=0.7, font=1)

paraiba_with_n <- merge(paraiba, casos_2016)
paraiba_with_n$n[is.na(paraiba_with_n$n)] <- runif(26, 0, 0)
#map_data(paraiba_with_n)
#paraiba_with_n.k <- map_data(paraiba_with_n)
paraiba_with_n@data$ID_MUNICIP
#paraiba_with_n@data$id = rownames(paraiba_with_n@data)
#paraiba_with_n@data$id <- paraiba_with_n@data$ID_MUNICIP
paraiba_with_n@data$id = as.numeric(rownames(paraiba_with_n@data))-1
paraiba_with_n.points = fortify(paraiba_with_n, by="id")

paraiba_with_n.df = join(paraiba_with_n.points, paraiba_with_n@data, by="id")


ggplot(paraiba_with_n.df, aes(long, lat, group=group)) + 
  geom_polygon(aes(fill=n)) +
  #scale_fill_gradient(low = "red", high = "black") +
  scale_fill_gradientn(colours = pal[1:5]) +
  geom_path(color='black', size=0.1) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())

# Incluindo os números de casos ao mapa
result <- merge(paraiba, casos_2016)
# Adiciona o valor 0 aos municipios que não possuem casos de dengue
result$n[is.na(result$n)] <- runif(26, 0, 0)

# Verifica o numero de casos pelo código do município
result[result$ID_MUNICIP==250750,]$NM_MUNICIP

# Verificando quais municipios não possuem casos de dengue
result[is.na(result$n),]$NM_MUNICIP

pal <- colorRampPalette(brewer.pal(9, 'PuRd'))(length(result$n))
pal <- pal[with(result, findInterval(result$n, sort(unique(result$n))))]
pal
#col <- rep(grey(0.9), length(result@data$NM_MUNICIP))
#col[match(result$NM_MUNICIP, result@data$NM_MUNICIP)] <- pal


# PLOTAGEM DO MAPA DA PARAÍBA COM O NÚMERO DE CASOS POR MUNICÍPIO
plot(result, col = pal)
pts.PB <- getSpatialPolygonsLabelPoints(result[result$n >= 1,])
text(pts.PB@coords, labels=result$n[result$n >= 1], cex = 1)


# GARBAGE
# GARBAGE
# GARBAGE
# GARBAGE
# GARBAGE
# GARBAGE
# GARBAGE
# GARBAGE
# GARBAGE
plot(paraiba[paraiba$NM_MUNICIP=="JOÃO PESSOA",], add=T, col = "red")
plot(paraiba[paraiba$NM_MUNICIP=="GUARABIRA",], add=T, col = alpha("green", 0.5))
plot(paraiba[paraiba$NM_MUNICIP=="CONCEIÇÃO",], add=T, col = alpha("blue", 0.5))
legend('topright', legend = levels(result$NM_MUNICIP), col = pal, cex = 0.5, pch = 10)



ggplot(data = result, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "black") +
  coord_equal() +
  theme()










ggplot(data = result, aes(x= long, y = lat, group=group)) +
  geom_polygon(data=result, aes(fill=pal)) +
  geom_path(color='black', size=0.2) +
  scale_fill_brewer(palette='PuRd')

plot(result, col = result$n)
legend('topright', legend = levels(result$NM_MUNICIP), col = 1:223, cex = 0.8, pch = 1)



ggplot(data = result, aes(x=long, y=lat, group=group),
             fill="white", color="#7f7f7f", size=0.25) +
  geom_path(color='black', size=0.2) + coord_map() +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
                 panel.border = element_blank(),
                 panel.background = element_rect(fill = "transparent", colour = NA),
                 panel.grid = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 legend.position = "right") +
  scale_fill_manual(values=colorRampPalette(brewer.pal(9, 'Reds'))(1), name="NM_MUNICIP")



#########
#casos_pb <- count(casos_dengue_2015, ID_MUNICIP, sort = TRUE)
casos_pb <- dplyr::count(casos_dengue_2015, ID_MUNICIP, sort = TRUE)
casos_pb$codigo <- casos_pb$ID_MUNICIP


paraiba$codigo <- as.character(paraiba$CD_GEOCMU)
paraiba$codigo <- substr(paraiba$codigo, 1, 6)

paraiba$codigo <- as.numeric(paraiba$codigo)
result <- merge(casos_pb, paraiba, by = "codigo")
paraiba[result$codigo == 250970,]$NM_MUNICIP

ggplot(data = result, aes(long, lat, group=group)) +
  geom_path(color='black', size=0.2) +
  scale_fill_brewer(palette='PuRd')


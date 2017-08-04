library(maps)
library(mapdata)
library(maptools) #Ler ESRI shapefiles
library(RColorBrewer)
library(ggplot2)
library(dplyr)

#install.packages("rgdal")
#install.packages("raster")
library(rgdal)
library(raster)

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
pts.PB <- getSpatialPolygonsLabelPoints(result[result$n >= 1000,])
text(pts.PB@coords, labels=result$n[result$n >= 1000], cex = 1)


paraiba.pts <- getSpatialPolygonsLabelPoints(result[result$n >= 0,])
paraiba.pts <- result$n
paraiba.pts <- as.data.frame(paraiba.pts)
paraiba.pts[,3] <- result$n



# Criando mapa com GGPLOT2
casos_2016 <- read.csv("numero_casos_2016.csv")

# Carregamos um shape auxíliar
paraiba <- readShapePoly("./pb_municipios/25MUE250GC_SIR.shp")

# Adicionado no mapa da paraíba a quantidade de casos
result <- merge(paraiba, casos_2016)

# Adicionamos 0 nos casos que possuem NA
result$n[is.na(result$n)] <- runif(26, 0, 0)


# Criamos uma tabela com o ID dos municipios em sequência para que possamos determinar
# A quais municípios pertencem a quantidade de casos
tabela_municipios <- data.frame(id = c(0:222), ID_MUNICIP = c(s@data$CD_GEOCMU), Nome = c(s@data$NM_MUNICIP))
tabela_municipios$ID_MUNICIP <- as.numeric(substr(as.character(tabela_municipios$ID_MUNICIP), 1, 6))
tabela_municipios <- merge(tabela_municipios, result, by = "ID_MUNICIP")

# Carregamos outro shape em definitivo
s <- shapefile("./pb_municipios/25MUE250GC_SIR.shp")
municipios_fortificados <- fortify(s, by = "id")

# Fazemos o merge da tabela com o shape
municipios <- merge(municipios_fortificados, tabela_municipios, by = "id")

# Gerando o plot
ggplot(data = municipios, aes(x = long, y = lat)) +
  scale_fill_gradient(low = "#FFCDD2", high = "#D32F2F", space = "Lab",
                      na.value = "grey50", guide = "colourbar") +
  geom_polygon(data=municipios, aes(x=long, y=lat, group=group, fill = n)) +
  geom_path(data=municipios, aes(x=long, y=lat, group=group), color='black', size=0.1) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("") + ylab("") +
  ggtitle("Dengue - Notificações por municípios da Paraíba em 2015") +
  labs(fill = "Quantidade de casos")




# TOP EXAMPLE
states <- map_data("state")
sim_data <- data.frame(region=unique(states$region), Percent.Turnout=match(unique(states$region), unique(states$region)))
sim_data_geo <- merge(states, sim_data, by="region")
qplot(long, lat, data=sim_data_geo, geom="polygon", fill=Percent.Turnout, group=group)

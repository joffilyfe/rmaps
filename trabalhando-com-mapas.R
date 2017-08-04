library(maps)
library(mapdata)
library(maptools) #Ler ESRI shapefiles
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(rgdal)
library(raster)

setwd("~/IFPB/TCC/mapas")

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
  scale_fill_gradient(low = "#ffffff", high = "red") +
  geom_polygon(data=municipios, aes(x=long, y=lat, group=group, fill = n)) +
  geom_path(data=municipios, aes(x=long, y=lat, group=group), color='black', size=0.5) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("") + ylab("") +
  ggtitle("Dengue - Notificações por municípios da Paraíba em 2015") +
  labs(fill = "Quantidade de casos")


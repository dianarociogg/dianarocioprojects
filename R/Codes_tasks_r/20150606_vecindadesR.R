rm(list=ls())
setwd("d:/Users/Lisethe/Desktop/vecindades")

library(sp)
library(rgdal)
library(spdep)
library(dplyr)
library(readxl)
library(bibtex)
library(adespatial)
#library(geoR)
#trellis.par.set(sp.theme())
#library(gstat)
#### exportando referencias paquetes usados
#paquetes<-c("sp","rgdal","spdep","dplyr","bibtex","readxl","adespatial")
#write.bib(paquetes,file="paquetesR.bib")


## cargando datas y shp
bog=readOGR(dsn = "/Users/Lisethe/Desktop/vecindades/bogota2.shp")
polig=polygons(bog)
#bog$NOMBR[]
datos <- read_excel("variableUPZ (1).xlsx")

##### Mapa base
plot(polig, border="grey")



############ CRITERIOS basados en grafos

##para generar los id de las regiones, debo renombrar unaUPZ con nombre repetido(una parte de ella esta separada)
nombres=bog@data[["NOMBR"]]
nombres[64]<-"Laflora"
levels(nombres)<-c(levels(nombres),"Laflora")
nombres[64]<-'Laflora'
nombres

### CRITERIOS CONTIGUIDAD
v1reina=poly2nb(bog,row.names =nombres,queen=T) # reina
v1roca=poly2nb(bog,row.names = nombres,queen=F) #roca
#graficos:
{
plot(polig, border="grey")
plot(v1reina, coordinates(bog), add=T,  col='tomato')
title(main=paste("Criterio de contiguidad Reina", "por UPZ", sep = "\n"), cex.main=0.6)
plot(polig, border="grey")
plot(v1roca, coordinates(bog), add=T,  col='turquoise4')
}

#### criterio k vecinos
vec2= knn2nb(knearneigh(coordinates(bog), k = 2), row.names = nombres)
vec4= knn2nb(knearneigh(coordinates(bog), k = 4), row.names = nombres)
vec6= knn2nb(knearneigh(coordinates(bog), k = 6), row.names = nombres)
#graficos:
{
plot(polig, border="grey")
plot(vec2, coordinates(bog), add=T,  col='magenta2')

plot(polig, border="grey")
plot(vec4, coordinates(bog), add=T,  col='springgreen2')

plot(polig, border="grey")
plot(vec6, coordinates(bog), add=T,  col='royalblue2')
}

############ CRITERIOS basados en grafos
v2 <- tri2nb(coordinates(bog), row.names =nombres) #triangulacion
v3 <- graph2nb(gabrielneigh(coordinates(bog)), row.names =nombres)# gabriel
v4<- graph2nb(relativeneigh(coordinates(bog)), row.names =nombres) # relativo
{
plot(polig, border="grey")
plot(v2, coordinates(bog), add=T, col='violetred1')
title(main=paste("Red con Criterio Triangulacion", "por UPZ", sep = "\n"), cex.main=0.6)

plot(polig, border="grey")
plot(v3, coordinates(bog), add=T, col='violetred1')

plot(polig, border="grey")
plot(v4, coordinates(bog), add=T, col='violetred1')
title(main=v4,coordinates(bog), add=T, col='violetred1')
title(main=paste("Red con Criterio Relativo", "por UPZ", sep = "\n"), cex.main=0.6)
}

## CRITERIOS BASADOS EN DISTANCIAS
V5 <- knn2nb(knearneigh(coordinates(bog), k = 1), row.names = nombres) #un solo vecino 
K <- unlist(nbdists(V5, coordinates(bog)))
summary(K)
max_1nn <- max(K)
v6 <- dnearneigh(coordinates(bog), d1 = 0, d2 = 0.75 * max_1nn, row.names = nombres)
v7 <- dnearneigh(coordinates(bog), d1 = 0, d2 = 1 * max_1nn, row.names = nombres)
v8 <- dnearneigh(coordinates(bog), d1 = 0, d2 = 1.5 * max_1nn, row.names = nombres)
{
plot(polig, border="grey")
plot(v6, coordinates(bog), add=T,  col='darkorchid3')
title(main=paste("Red Criterio distancia 2617 m ", "por UPZ", sep = "\n"), cex.main=0.6)

plot(polig, border="grey")
plot(v7, coordinates(bog), add=T,col='darkorchid3')
title(main=paste("Red Criterio distancia 3489 m ", "por UPZ", sep = "\n"), cex.main=0.6)

plot(polig, border="grey")
plot(v8, coordinates(bog), add=T,col='darkorchid3')
title(main=paste(" Red Criterio distancia 5234 m ", "por UPZ", sep = "\n"), cex.main=0.6)
}


#explorando otras matrices de pesos espaciales:
listw.explore() # herramienta interactiva para definir matriz de pesos espaciales, usando distintas opciones y genera el codigo que debe usarse.

### EXPORTO LAS MATRICES
#write.nb.gal(v2,"triangulacion.gal",oldstyle = T ,ind=nombres)
#write.nb.gal(v3,"gabriel.gal")
#write.nb.gal(v3,"relativo.gal")
#write.nb.gal(v3,"distancia1.gal")
#write.nb.gal(v3,"distancia2.gal")
#write.nb.gal(v3,"distancia3.gal")
###############################################################################################################
###############################################################################################################

### AUTOCORRELACION ESPACIAL GLOBAL
#MORAN
datosM=as.matrix(datos[,-c (1,46:49)])
nomb<-datos$NOMBR
rownames(datosM) <-nomb
datosM=as.data.frame(datosM)

#conversion a listas de vecinos
{
wq=nb2listw(v1reina,style = "W", zero.policy = TRUE) # zero.policy TRUE permit the weights list to be formed with zero-length weights vectors
wr=nb2listw(v1roca,style = "W", zero.policy = T)
wv2nb=nb2listw(vec2,style = "W", zero.policy = T)
wv4nb=nb2listw(vec4,style = "W", zero.policy = T)
wv6nb=nb2listw(vec6,style = "W", zero.policy = T)
wtrian=nb2listw(v2,style = "W", zero.policy = T)
wgab=nb2listw(v3,style = "W", zero.policy = T)
wrel=nb2listw(v4,style = "W", zero.policy = T)
wd1=nb2listw(v6,style = "W", zero.policy = T)
wd2=nb2listw(v7,style = "W", zero.policy = T)
wd3=nb2listw(v8,style = "W", zero.policy = T)
}

#definicion de vectores de salida de resultados
{
moranreina<-0
MoranReina<-0
MoranReinavalP<-0

moranroca<-0
MoranRoca<-0
MoranRocavalP<-0

moran2nb<-0
Moran2nb<-0
Moran2nbvalP<-0

moran4nb<-0
Moran4nb<-0
Moran4nbvalP<-0

moran6nb<-0
Moran6nb<-0
Moran6nbvalP<-0

morantrian<-0
MoranTrian<-0
MoranTrianvalP<-0

morangab<-0
MoranGab<-0
MoranGabvalP<-0

moranrel<-0
MoranRel<-0
MoranRelvalP<-0

moranrd1<-0
Morand1<-0
Morand1valP<-0

moranrd2<-0
Morand2<-0
Morand2valP<-0

moranrd3<-0
Morand3<-0
Morand3valP<-0
}


# CALCULO ESTADISTICO
# style: W= filas estandarizadas (suma de todos los links es n), C=globalmente estandarizada, U= C/numero de vecinos, S=esquema de 
#estabilizacion de la varianza Tiefelsdorf
for(i in seq_along(datosM)){
  moranreina[i]=list(moran.test(datosM[[i]],wq, zero.policy = T))
  MoranReina[i]= moranreina[[i]][["estimate"]][["Moran I statistic"]]
  MoranReinavalP[i]=moranreina[[i]][["p.value"]]
  
  moranroca[i]=list(moran.test(datosM[[i]],wr, zero.policy = T))
  MoranRoca[i]= moranroca[[i]][["estimate"]][["Moran I statistic"]]
  MoranRocavalP[i]=moranroca[[i]][["p.value"]]
  
  moran2nb[i]=list(moran.test(datosM[[i]],wv2nb, zero.policy = T))
  Moran2nb[i]= moran2nb[[i]][["estimate"]][["Moran I statistic"]]
  Moran2nbvalP[i]=moran2nb[[i]][["p.value"]]
  
  moran4nb[i]=list(moran.test(datosM[[i]],wv4nb, zero.policy = T))
  Moran4nb[i]= moran4nb[[i]][["estimate"]][["Moran I statistic"]]
  Moran4nbvalP[i]=moran4nb[[i]][["p.value"]]
  
  moran6nb[i]=list(moran.test(datosM[[i]],wv6nb, zero.policy = T))
  Moran6nb[i]= moran6nb[[i]][["estimate"]][["Moran I statistic"]]
  Moran6nbvalP[i]=moran6nb[[i]][["p.value"]]
  
  morantrian[i]=list(moran.test(datosM[[i]],wtrian, zero.policy = T))
  MoranTrian[i]= morantrian[[i]][["estimate"]][["Moran I statistic"]]
  MoranTrianvalP[i]=morantrian[[i]][["p.value"]]
  
  morangab[i]=list(moran.test(datosM[[i]],wgab, zero.policy = T))
  MoranGab[i]= morangab[[i]][["estimate"]][["Moran I statistic"]]
  MoranGabvalP[i]=morangab[[i]][["p.value"]]
  
  moranrel[i]=list(moran.test(datosM[[i]],wrel, zero.policy = T))
  MoranRel[i]= moranrel[[i]][["estimate"]][["Moran I statistic"]]
  MoranRelvalP[i]=moranrel[[i]][["p.value"]]
  
  moranrd1[i]=list(moran.test(datosM[[i]],wd1, zero.policy = T))
  Morand1[i]= moranrd1[[i]][["estimate"]][["Moran I statistic"]]
  Morand1valP[i]=moranrd1[[i]][["p.value"]]
  
  moranrd2[i]=list(moran.test(datosM[[i]],wd2, zero.policy = T))
  Morand2[i]= moranrd2[[i]][["estimate"]][["Moran I statistic"]]
  Morand2valP[i]=moranrd2[[i]][["p.value"]]
  
  moranrd3[i]=list(moran.test(datosM[[i]],wd3, zero.policy = T))
  Morand3[i]= moranrd3[[i]][["estimate"]][["Moran I statistic"]]
  Morand3valP[i]=moranrd3[[i]][["p.value"]]
  }

variables<-names(datosM)
ResultadosMoran<-data.frame(variables, MoranReina,MoranReinavalP, MoranRoca,MoranRocavalP,Moran2nb,Moran2nbvalP,Moran4nb,Moran4nbvalP,
                            Moran6nb,Moran6nbvalP,MoranTrian,MoranTrianvalP,MoranGab,MoranGabvalP,MoranRel,MoranRelvalP,
                            Morand1,Morand1valP,Morand2,Morand2valP,Morand3,Morand3valP)
#writexl::write_xlsx(ResultadosMoran, path="ResultadosMoran.xlsx")

#Scatter plot moran global
moran.plot(datosM$basurano,wv2nb, pch=18)
moran.plot(datosM$Ucom,wv2nb, pch=20)
moran.plot(datosM$cuarto,wv2nb, pch=20)

moran.plot(datosM$basurasi,wv4nb, pch=20)
moran.plot(datosM$basurano,wv2nb, pch=20)

#manualmente:
#datosM$esc_basurano <- scale(datosM$basurano)  %>% as.vector()
#datosM$lag_basurano <- lag.listw(wv4nb, datosM$basurano)
#summary(datosM$esc_basurano)
#summary(datosM$lag_basurano)
#summary(datosM$basurano)

#x <- datosM$esc_basurano
#y <- datosM$lag_basurano %>% as.vector()
#xx <- data.frame(datosM$esc_basurano , datosM$lag_basurano %>% as.vector())
#moran.plot(datosM$esc_basurano, wv4nb)

#library(ggplot2)
#ggplot(xx, aes(x, y)) + geom_point() + geom_smooth(method = 'lm', se = F) + geom_hline(yintercept = 0, linetype = 'dashed') + geom_vline(xintercept = 0, linetype = 'dashed') 


#################### moran local
#basurano
{
moranlocal=localmoran(datosM$basurano,wv2nb,zero.policy = T)
summary(moranlocal)
#manualmente:
datosM$esc_basurano <- scale(datosM$basurano)  %>% as.vector() #variable escaladada
datosM$lag_basurano <- lag.listw(wv2nb, datosM$basurano) # rezago espacial
# identify the moran plot quadrant for each observation
datosM$quad_sig <- NA
bog@data[(datosM$esc_basurano >= 0 & datosM$lag_basurano >= 0) & (moranlocal[, 5] <= 0.05), "quad_sig"] <- 1
bog@data[(datosM$esc_basurano <= 0 & datosM$lag_basurano <= 0) & (moranlocal[, 5] <= 0.05), "quad_sig"] <- 2
bog@data[(datosM$esc_basurano >= 0 & datosM$lag_basurano <= 0) & (moranlocal[, 5] <= 0.05), "quad_sig"] <- 3
bog@data[(datosM$esc_basurano >= 0 & datosM$lag_basurano <= 0) & (moranlocal[, 5] <= 0.05), "quad_sig"] <- 4
bog@data[(datosM$esc_basurano <= 0 & datosM$lag_basurano >= 0) & (moranlocal[, 5] <= 0.05), "quad_sig"] <- 5  #WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS
# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)
# Set the corresponding labels for the thematic map classes
labels <- c("high-High", "low-Low", "High-Low", "Low-High", "Not Signif.")
# see ?findInterval - This is necessary for making a map
np <- findInterval(bog$quad_sig, breaks)
# Assign colors to each map class
colors <- c("red", "blue", "lightpink", "skyblue2", "white")
centroides <- coordinates(polig)
plot(bog, col = colors[np])  #colors[np] manually sets the color for each county
text(centroides,as.character(bog@data$NOMBR),cex=0.7)
mtext("Local Moran's I No GARBAGE", cex = 1.5, side = 3, line = 1)
legend("topleft", legend = labels, fill = colors, bty = "n")
# codigo tomado de: http://rstudio-pubs-static.s3.amazonaws.com/4938_b5fc230d586c48b291627ff6ea484d2e.html
}

datosM$signif<-NA
bog@data[(moranlocal[,5]<=0.05),"UPZ_signif"]<-1
bog@data[(moranlocal[,5]>0.05),"UPZ_signif"]<-1
centroides <- coordinates(polig)
plot(bog, col = colors[np])  #colors[np] manually sets the color for each county
text(centroides,as.character(baog@dat$CODIGO_U),cex=0.7)

#Roedorno
{
moranlocalroedores=localmoran(datosM$roedno,wv4nb,zero.policy = T)
summary(moranlocalroedores)
#manualmente:
datosM$esc_roedoresno <- scale(datosM$roedno)  %>% as.vector() #variable escaladada
datosM$lag_roedoresno <- lag.listw(wv4nb, datosM$roedno) # rezago espacial

# identify the moran plot quadrant for each observation
datosM$quad_sigroedores <- NA
bog@data[(datosM$esc_roedoresno >= 0 & datosM$lag_roedoresno >= 0) & (moranlocalroedores[, 5] <= 0.05), "quad_sigroedores"] <- 1
bog@data[(datosM$esc_roedoresno <= 0 & datosM$lag_roedoresno <= 0) & (moranlocalroedores[, 5] <= 0.05), "quad_sigroedores"] <- 2
bog@data[(datosM$esc_roedoresno>= 0 & datosM$lag_roedoresno<= 0) & (moranlocalroedores[, 5] <= 0.05), "quad_sigroedores"] <- 3
bog@data[(datosM$esc_roedoresno>= 0 & datosM$lag_roedoresno <= 0) & (moranlocalroedores[, 5] <= 0.05), "quad_sigroedores"] <- 4
bog@data[(datosM$esc_roedoresno<= 0 & datosM$lag_roedoresno >= 0) & (moranlocalroedores[, 5] <= 0.05), "quad_sigroedores"] <- 5  #WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS
# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)
# Set the corresponding labels for the thematic map classes
labels <- c("high-High", "low-Low", "High-Low", "Low-High", "Not Signif.")
# see ?findInterval - This is necessary for making a map
np2 <- findInterval(bog$quad_sigroedores, breaks)
# Assign colors to each map class
colors <- c("tomato", "blue", "lightpink", "skyblue2", "white")
centroides <- coordinates(polig)
plot(bog, col = colors[np2])  #colors[np] manually sets the color for each county
text(centroides,as.character(bog@data$CODIGO_U),cex=0.7)
mtext("Local Moran's I RoedoresNO", cex = 1.5, side = 3, line = 1)
legend("topleft", legend = labels, fill = colors, bty = "n")
# codigo tomado de: http://rstudio-pubs-static.s3.amazonaws.com/4938_b5fc230d586c48b291627ff6ea484d2e.html
}


### defino funcion
mapamoranlocal<-function(df,dfespacial,variable,matrizpesos){
  moranlocal=localmoran(variable,matrizpesos,zero.policy = T)
  df$var_escalada<- scale(variable)  %>% as.vector() #variable escaladada
  df$var_rezagada <- lag.listw(matrizpesos, variable) # rezago espacial
  # identify the moran plot quadrant for each observation
  df$quad_sig <- NA
  dfespacial@data[(df$var_escalada>= 0 & df$var_rezagada>= 0) & (moranlocal[, 5] <= 0.05), "quad_sig"] <- 1
  dfespacial@data[(df$var_escalada <= 0 & df$var_rezagada<= 0) & (moranlocal[, 5] <= 0.05), "quad_sig"] <- 2
  dfespacial@data[(df$var_escalada>= 0 & df$var_rezagada <= 0) & (moranlocal[, 5] <= 0.05), "quad_sig"] <- 3
  dfespacial@data[(df$var_escalada >= 0 & df$var_rezagada <= 0) & (moranlocal[, 5] <= 0.05), "quad_sig"] <- 4
  dfespacial@data[(df$var_escalada<= 0 & df$var_rezagada >= 0) & (moranlocal[, 5] <= 0.05), "quad_sig"] <- 5  #WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS  
  breaks <- seq(1, 5, 1) # Set the breaks for the thematic map classes
  labels <- c("high-High", "low-Low", "High-Low", "Low-High", "Not Signif.")
  np <- findInterval(dfespacial$quad_sig, breaks)
  colors <- c("red", "blue", "lightpink", "skyblue2", "white")
  #centroides <- coordinates(polygons(dfespacial))
  plot(dfespacial, col = colors[np])  #colors[np] manually sets the color for each county
  #text(centroides,as.character(dfespacial@data$CODIGO_U),cex=0.7)
  mtext("Local Moran's I", cex = 1.5, side = 3, line = 1)
  legend("topleft", legend = labels, fill = colors, bty = "n")
  }
  
mapamoranlocal(datosM,bog,datosM$basurasi,wv4nb)
mapamoranlocal(datosM,bog,datosM$trab,wv4nb)
mapamoranlocal(datosM,bog,datosM$ventino,wv4nb)
mapamoranlocal(datosM,bog,datosM$derrno,wv4nb)
mapamoranlocal(datosM,bog,datosM$basurano,wv4nb)
mapamoranlocal(datosM,bog,datosM$hogar,wv4nb)
mapamoranlocal(datosM,bog,datosM$Ucom,wv4nb)

mapamoranlocal(datosM,bog,datosM$Ucom,wv2nb)
mapamoranlocal(datosM,bog,datosM$cuarto,wv2nb)

# codigo tomado de: http://rstudio-pubs-static.s3.amazonaws.com/4938_b5fc230d586c48b291627ff6ea484d2e.html




#########################################
# Geary
geary.test(datosM$hogar,wv2nb, randomisation = T, zero.policy = T)
geary.test(datosM$trab,wv2nb, randomisation = T, zero.policy = T)
geary.test(datosM$ventino,wv2nb, randomisation = T, zero.policy = T)
geary.test(datosM$derrno,wv2nb, randomisation = T, zero.policy = T)
geary.test(datosM$roedno,wv2nb, randomisation = T, zero.policy = T)

geary.test(datosM$Ucom,wv2nb, randomisation = T, zero.policy = T)
geary.test(datosM$cuarto,wv2nb, randomisation = T, zero.policy = T)
geary.test(datosM$basurano,wv2nb, randomisation = T, zero.policy = T)




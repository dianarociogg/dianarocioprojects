## Este script calcula el valor máximo de PD por municipio para cada año entre 2009 - 2015
## Parte de un archivo que contiene la ubicación reportada a final de mes por cada PD entre Octubre de 2009 y Diciembre de 2015
## Calcula el Número de PD para el final de cada mes para cada municipio
## Luego calcula cual fue el valor máximo para ese municipio en cada año

library(classInt)
library(reshape2)
library(stringr)
library(foreign)


#Leyendo registro ubicacion por mes por DM (Correo Viviam 18/04/2016)
totalpprxmun<-read.table("C:/DATOS/SOLICITUDES/SEGURIDAD/Capturasmulti/datosxppr.txt",sep="\t",header=TRUE)
# Seleccion de columnas (solo municipio, omite columnas de Dpto)
a<-c(1,seq(2,151,by=2))
pprmes<-totalpprxmun[,a]
# Convirtiendo la tabla de formato wide a long para sacar cifras
pprmesl<-melt(pprmes,id.vars=c("CODA"))# La advertencia no aplica porque todas las columnas tienen el mismo tipo valor http://stackoverflow.com/questions/25688897/reshape2-melt-warning-message
#Extrayendo el año para cada observacion
pprmesl$anio<-str_sub(pprmesl$variable,start=-4)
head(pprmesl)

#Suma de PD por mes para hallar total PD por mes por municipio. La información viene por individuo por mes, se consolida cuantos individuos reportaron el mpio como lugar de residencia
lpprmesl<-split(pprmesl,pprmesl$variable)
#Cuantos por Municipio para cada mes
fun00<-function(x){tmp_df<-data.frame(table(x$value))
			 names(tmp_df)<-c("Mun","NumPPR")
			 print(dim(tmp_df))
			 return(tmp_df)}
tablaxmun<-lapply(lpprmesl,fun00)
head(tablaxmun[[1]])

fun01<-function(x){tmp_df = data.frame(tablaxmun[[x]])
			 tmp_name = names(tablaxmun)[x]
			 tmp_df$ANIOMES = paste(str_sub(tmp_name,start=-4),
			     str_sub(tmp_name,start=-6,end=3), sep="-")
			 tmp_df$anio<-str_sub(tmp_name,start=-4)
			 return(tmp_df)}
tablaxmunmes<-lapply(1:length(tablaxmun),fun01)
str(tablaxmunmes)

#Calculando maximo de PD presentado en el año para cada municipio
df <-do.call("rbind", tablaxmunmes)
#df2<-subset(df, df$Mun != 0, select = -ANIOMES)
df2<-df[df$Mun != "0",c(1:2,4)] # Removiendo los registros que no reportaron municipio
head(df); head(df2) # comparando datos iniciales con el data frame de los datos removidos
dim(df);dim(df2)
#ldf<-split(df,df$anio)
es1<-aggregate(NumPPR~., df2, FUN= max) # calculando el máximo
head(es1)
dim(es1)
pprxmun<-unique(es1) # removiendo registros duplicados: si el municipio presento dos veces el valor minimo hay que remover el duplicado ( Mpio 99999 2010/02: 4 = Mpio 99999 2011/04: 4)
head(pprxmun[order(pprxmun[,1]), ])
tabla_ppr<-reshape(pprxmun, idvar = "Mun", timevar = "anio", direction = "wide") # cambiando el formato de la tabla: filas -> municipios, columnas -> dato PD por año
getwd()
write.table(tabla_ppr,"C:/DATOS/SOLICITUDES/20160607_BasesLeo/PPRMun.txt",sep="\t") # exportando a txt en la carpeta deseada


#### Para cada ExGrupo ######
#Leyendo registro ubicacion por mes por DM (Correo Viviam 18/04/2016)
CODAEG<-read.table("C:/DATOS/SOLICITUDES/20160607_BasesLeo/CODA_ExGrupo.txt",sep="\t",header=TRUE)
head(CODAEG);dim(CODAEG)# 58028x2
head(pprmes);dim(pprmes)# 51392x76
CODAEXMR<-merge(pprmes,CODAEG, by="CODA", all.x=TRUE)
head(CODAEXMR)
summary(CODAEXMR$Exgrupo)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
VentasVehiculos<-read.csv("data-yJ93v.csv",header=TRUE,sep=",")
str(VentasVehiculos)
dfventas<-VentasVehiculos[,c(1,4)]
write.table(dfventas,"dfventas.txt")


#dfventas$total<-dfventas[,4]+dfventas[,2]+dfventas[,3]


Matriculas<-read.csv("Matriculacions_mensuals_de_vehicles_electrificats_i_h_brids_a_Catalunya.csv",header=TRUE,sep=",")
str(Matriculas)
View(Matriculas)

table(Matriculas$Municipi,Matriculas$Tecnologia,Matriculas$Any)
Matriculas$Uts=as.numeric(Matriculas$Uts)
Matriculas$Any=as.numeric(Matriculas$Any)
MatricMunAny<-aggregate(Uts  ~   Codi.IDESCAT + Any, data = Matriculas, sum)




Recarregas<-jsonlite::fromJSON("C:/Users/usuari/OneDrive - SITEP/Documents/SITEP/Vehicles/InformacioCarregues.json")
str(Recarregas)

Recarregas$consum  = as.numeric(Recarregas$consum)
Recarregas$temps_recarrega_css_combo  = as.numeric(Recarregas$temps_recarrega_css_combo)
Recarregas$temps_recarrega_chademo = as.numeric(Recarregas$temps_recarrega_chademo)
Recarregas$temps_recarrega_mennekes_2 = as.numeric(Recarregas$temps_recarrega_mennekes_2)
Recarregas$temps_recarrega_mennekes_1 = as.numeric(Recarregas$temps_recarrega_mennekes_1)
Recarregas$data = as.Date(Recarregas$data)
Recarregas$estat_css_combo = as.numeric(Recarregas$estat_css_combo)
Recarregas$temp_recarrega<-Recarregas$temps_recarrega_css_combo+Recarregas$temps_recarrega_chademo

R2<-na.omit(Recarregas)
Recarregas$temp_recarrega
str(Recarregas)


# Recarregas por municipio


chademo=Recarregas[!is.na(Recarregas$temps_recarrega_chademo) & Recarregas$temps_recarrega_chademo !=0 ,
                   c("codimun","temps_recarrega_chademo","nom")]

chademo$tempo_mins=chademo$temps_recarrega_chademo/60
median(chademo$tempo_mins)
csscombo=Recarregas[!is.na(Recarregas$temps_recarrega_css_combo) & 
                      Recarregas$temps_recarrega_css_combo !=0 &
                      Recarregas$nom != 'EdRR Gandesa (AC-DC50kW)' &
                      Recarregas$nom != 'EdRR Gandesa2 (AC-DC50kW)',
                    c("codimun","temps_recarrega_css_combo","nom")]
csscombo$tempo_mins=csscombo$temps_recarrega_css_combo/60

summary(chademo)
boxplot(chademo$tempo_mins)
summary(csscombo)
boxplot(csscombo$tempo_mins)


chademo_muniR=split(chademo,chademo$codimun)
csscombo_muniR=split(csscombo,csscombo$codimun)



#funprom=function(x){mean(na.omit(x[,2]))}

funcntRecarregas<-function(x){#sum(sign(diff(na.omit(x[,2])))==-1)
  sum(sign(diff(na.omit(x[,2])))==-1)
  }
funmaxtiempoRecarregas<-function(x){median(na.omit(x[sign(diff(na.omit(x[,2])))==-1,c("tempo_mins")]))}



median_chademo=lapply(chademo_muniR,funmaxtiempoRecarregas)



median_csscombo=lapply(csscombo_muniR,funmaxtiempoRecarregas)

medians1=data.frame(unlist(do.call(rbind, median_chademo)))
medians2=data.frame(unlist(do.call(rbind, median_csscombo)))

medians <- merge(medians1, medians2,by = 'row.names', all = TRUE)
medians$promedio=ifelse(is.na(medians[,2]),medians[,3],(medians[,2]+medians[,3])/2)
View(medianxMun)



medianxMun=medians[,c(1,4)]


actual_chademo
actual_csscombo


conteochademo=lapply(csscombo_muniR,funcntRecarregas)
conteochademo
conteocsscombo=lapply(chademo_muniR,funcntRecarregas)
conteocsscombo

conteos1=data.frame(unlist(do.call(rbind, conteochademo)))
conteos2=data.frame(unlist(do.call(rbind, conteocsscombo)))
conteos <- merge(conteos1, conteos2,by = 'row.names', all = TRUE)
colnames(conteos)=c("Row.names","cntChadeMo","cntCss")
conteos$cnts=round((conteos$cntChadeMo + conteos$cntCss)/91)
conteosxMun=conteos[,c(1,4)]


View(conteosxMun)


actual=merge(medianxMun, conteosxMun,by = 'Row.names', all = TRUE)
actual$minutosprom=actual$promedio*actual$cnts
actual$actual=actual$minutosprom/1080*100

View(actual)

write.table(actual,"actual.txt",sep="\t")

# Load library
library('sf')

# Load shapefile
geomuni <- read_sf("divisions-administratives-v2r1-municipis-50000-20220801.shp")
actualgeo<-merge(geomuni,actual, by.y="Row.names",by.x="CODIMUNI")

plot(actualgeo["actual"])
st_write(actualgeo, "Actual.shp")



promediocsscombo=lapply(chademo_muniR,funmaxtiempoRecarregas)
promediocsscombo
promediochademo=lapply(csscombo_muniR,funmaxtiempoRecarregas)
promediochademo





R2<-Recarregas[,c(13,2,14,17)]
R2<-na.omit(R2)
head(R2)
R2$tmpID<-paste(R2$nom,R2$data,sep = "_")
head(R2)

R2<-R2[complete.cases(R2),]

c_u<-split(R2,R2$tmpID)
funcntRecarregas<-function(x){sum(sign(diff(x$temp_recarrega))==-1)}
cntRecarregas<-lapply(c_u,funcntRecarregas)
funpromRecarregas<-function(x){max(x$temp_recarrega)}
maxRecarregas<-lapply(c_u,funpromRecarregas)

mapply("/",maxRecarregas,cntRecarregas,SIMPLIFY = FALSE)

cntR1<-as.data.frame(unlist(do.call("rbind",cntRecarregas)))
cntR1  #### Ojo forecast!
toForecast<-split(cntR1,cntR1$estacion)
cntR1$tmpID<-rownames(cntR1)

str(cntR1)
cntR1$estacion<-str_sub(rownames(cntR1),1,-12)
str(cntR1)
RecarregaEstAny<-aggregate(V1  ~   estacion, data = cntR1, sum)

R3<-unique(R2[,1:2])

cnt_mun1<-merge(x = RecarregaEstAny, y = R3, by.x = "estacion", by.y="nom", all.x = TRUE)
View(cnt_mun1)
colnames(cnt_mun1)<-c("estacion","cntRecarregas","codimun")

RecarregascntMun2022<-aggregate(cntRecarregas  ~  codimun, data = cnt_mun1, sum)
RecarregascntMun2022<-RecarregascntMun2022[,c(1,3)]


s_estacions <- rgdal::readOGR("https://analisi.transparenciacatalunya.cat/resource/tb2m-m33b.geojson") 

s_estacions$longitud  = as.numeric(s_estacions$longitud)
s_estacions$latitud  = as.numeric(s_estacions$latitud)
s_estacions$kw = as.numeric(s_estacions$kw)

summary(s_estacions)
View(s_estacions@data)

MuniPoly<-read_sf("municipios.geojson")
Muni1<-merge(MuniPoly,RecarregascntMun2022, by.y="codimun",by.x="CODIMUNI")
Muni2<-merge(Muni1,MatricMunAny[MatricMunAny$Any==2022,], by.y="Codi.IDESCAT",by.x="CODIMUNI")

write_sf(obj = Muni2, dsn = "Data.geojson")



R3=R2
R3$tmpID<-paste(R3$nom,R3$data,sep = "_")
R4<-split(R3,R3$tmpID)
funcntRecarregas<-function(x){sum(sign(diff(x$temp_recarrega))==-1)}
cntRecarregasDia<-lapply(R4,funcntRecarregas)


cntRecarregasDiaEstacion=data.frame(unlist(do.call(rbind, cntRecarregasDia)))
cntRecarregasDiaEstacion$ID=row.names(cntRecarregasDiaEstacion)
cntRecarregasDiaEstacion$nom=sub("_.*", "", cntRecarregasDiaEstacion$ID) 
cntRecarregasDiaEstacion$date=sub(".*_", "", cntRecarregasDiaEstacion$ID)  
cntRecarregasDiaEstacion$date<-as.Date(cntRecarregasDiaEstacion$date,format = "%Y-%m-%d")
dayOfYear1 = as.numeric(format(cntRecarregasDiaEstacion[1,3], "%j"))

ts1=cntRecarregasDiaEstacion[,c(4,3,1)]
colnames(ts1)=c("nom","date","cnt")


ts1=ts(tmp_df[,1], start = c(2022, dayOfYear1),frequency = 365)
ts2=split(ts1,ts1$nom)


funmean=function(x){median(x$cnt)}
lapply(ts2,funmean)








#timeseries per estacion
funts<-function(i){ tmp_df<-as.data.frame(ts2[i])
                    tmp_df$date<-as.Date(str_sub(ts2[,2],-10,-1),format = "%Y-%m-%d")
                    dayOfYear1 = as.numeric(format(tmp_df[1,5], "%j"))
                    ts(tmp_df[,1], start = c(2022, dayOfYear1),frequency = 365)}
asi<-lapply(1:length(R3),funts)


tmp_df=R3[[1]]
tmp_df$date<-as.Date(str_sub(tmp_df[,3],-10,-1),format = "%Y-%m-%d")
dayOfYear1 = as.numeric(format(tmp_df[1,3], "%j"))
                    ts(tmp_df[,1], start = c(2022, dayOfYear1),frequency = 365)


ggplot(tmp_df, aes(x=date, y=temp_recarrega)) + geom_line()
                         xlab("")
                    p

                    
                    
                    decompose(asi[[1]], "multiplicative")


str(Muni2)
read.csv("LIMADM_MUNICIPI.csv",header=TRUE,sep=",")


datoxmun<-split(Muni2,Muni2$CODIMUNI)
fun<-function(x){datoxmun[[1]]$cntRecarregas
  #cbind(datoxmun[[1]]$cntRecarregas,datoxmun[[1]]$Uts)
  }
lapply(1:length(datoxmun),fun)

cor(log(Muni2$Uts),log(Muni2$cntRecarregas))

library(datasets)
library(plotly)
library(forecast)
nileFc <- as.ts(forecast(tmp_df, h = 20))
colnames(nileFc) <- make.names(colnames(nileFc))
plot_ly() %>% 
  add_lines(x =  time(Nile), y =  Nile, name = "Flow") %>% 
  add_lines(x =  time(nileFc), y = nileFc[, "Point.Forecast"],
            type="surface", name = "Est. Median", color = I("green")) %>%
  add_lines(x =  time(nileFc), y = nileFc[, "Lo.95"], 
            type="surface", name = "Lower/Upper 95%", 
            color = I("brown")) %>%
  add_lines(x =  time(nileFc), y = nileFc[, "Hi.95"], 
            type="surface", name = "Lower/Upper 95%", color = I("brown"),
            showlegend = F)



Recarregas<-Muni2$cntRecarregas




Muni2$cntRecarregas/sum(Muni2$cntRecarregas)
b<-Muni2$Uts/sum(Muni2$Uts)

cor(a,b)




MatricMunAny
MatricMunAny$date<-as.Date(str_sub(tmp_df[,2],-10,-1),format = "%Y-%m-%d")
dayOfYear1 = as.numeric(format(tmp_df[1,5], "%j"))
ts(tmp_df[,1], start = c(2022, dayOfYear1),frequency = 365)}



# Libraries
library(ggplot2)
library(dplyr)

# Dummy data
data <- data.frame(
  day = as.Date("2017-06-14") - 0:364,
  value = runif(365) + seq(-140, 224)^2 / 10000
)

# Most basic bubble plot
p <- ggplot(data, aes(x=day, y=value)) +
  geom_line() + 
  xlab("")
p


load(file="R3.RDa")

funcntRecarregas<-function(x){sum(sign(diff(na.omit(x$temp_recarrega)))==-1)}
cntRecarregasDia<-lapply(R4,funcntRecarregas)

cntRecarregasDiaEstacion=data.frame(unlist(do.call(rbind, cntRecarregasDia)))
cntRecarregasDiaEstacion$ID=row.names(cntRecarregasDiaEstacion)
cntRecarregasDiaEstacion$nom=sub("_.*", "", cntRecarregasDiaEstacion$ID) 
cntRecarregasDiaEstacion$date=sub(".*_", "", cntRecarregasDiaEstacion$ID)  
cntRecarregasDiaEstacion$date<-as.Date(cntRecarregasDiaEstacion$date,format = "%Y-%m-%d")
str(cntRecarregasDiaEstacion)

dayOfYear1 = as.numeric(format(cntRecarregasDiaEstacion[1,4], "%j")) # primer registro de fecha

ts1=cntRecarregasDiaEstacion[,c(3,4,1)]
colnames(ts1)=c("nom","date","cnt")
str(ts1)

ts2=split(ts1,ts1$nom)

library(stringr)
funmean=function(x){median(x$cnt)}

cntmedioporestacion=lapply(ts2,funmean)

tmp_df=ts2[[1]]
head(tmp_df)
tmp_df$date<-as.Date(str_sub(tmp_df[,2],-10,-1),format = "%Y-%m-%d")
dayOfYear1 = as.numeric(format(tmp_df[1,2], "%j"))
ts(tmp_df[,3], start = c(2022, dayOfYear1),frequency = 365)



#timeseries per estacion
funts<-function(i){ tmp_df<-as.data.frame(ts2[i])
tmp_df$date<-as.Date(str_sub(tmp_df[,2],-10,-1),format = "%Y-%m-%d")
dayOfYear1 = as.numeric(format(tmp_df[1,2], "%j"))
ts(tmp_df[,3], start = c(2022, dayOfYear1),frequency = 365)}
asi<-lapply(1:length(R3),funts)

uno=asi[[1]]
dos=ts2[[2]]

library(dplyr)
library(lubridate)


dos <- mutate(dos, MonthYear = paste(year(date),formatC(month(date), width = 2, flag = "0")))
str(dos)
xmonth <- aggregate(dos$cnt, by = list(dos$MonthYear), FUN = function(x) mean(x, na.rm=T))
xweek2 <- mutate(dos, Week = week(date))
#xweek <-aggregate(xweek2$cnt, by = list(xweek2$Week), FUN = function(x) mean(x, na.rm=T))
myts <- ts(xweek2$cnt, start = c(26), end = c(34))
plot(myts)

funtsweek <- function(x) {
  dos <- mutate(x, MonthYear = paste(year(date),formatC(month(date), width = 2, flag = "0")))
  xweek2 <- mutate(dos, Week = week(date))
  ts(xweek2$cnt, start = c(26), end = c(34))
  }
tsweeks=lapply(ts2,funtsweek)

lapply(tsweeks,plot)



funmodel <- function(x) {mdl <- tsglm(xweek2$cnt, model = list(past_obs = 1, past_mean = 1), distr = "poisson")
  
}




mdl <- tsglm(xweek2$cnt, model = list(past_obs = 1, past_mean = 1), distr = "poisson")


myds_month <- decompose(myts)
plot(myds_month)




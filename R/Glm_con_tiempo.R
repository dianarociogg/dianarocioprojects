rm(list=ls())
library(Amelia)
library(glm2)
library(xtable)

setwd("C:/DATOS/SOLICITUDES/20160607_BasesLeo/20160805_IVRVPD/Multitemp")

Datos<-read.table("DatosMT.txt",header=T,sep="\t",de=",")
head(Datos)
str(Datos)
Datos<-Datos[,-which(names(Datos) %in% c("TiempoProceso"))]



DatosHom<-read.table("DatosMTHomicidios.txt",header=T,sep="\t",de=",")
head(DatosHom)
str(DatosHom)
DatosHom<-DatosHom[DatosHom$ING == "SI",-which(names(DatosHom) %in% c("TiempoProceso","GF","ING"))]
head(DatosHom)
str(DatosHom)

Data<-rbind(Datos,DatosHom)
head(Data)
str(Data)
summary(Data)

sapply(Data, function(x){print(summary(x))})

Data[Data$Sexo==" F ",c("Sexo")]<-"F"
Data[Data$Sexo==" M ",c("Sexo")]<-"M"


summary(Data$EC)
str(Data)
#Año Desmovilizacion
hist((Data[Data$AnoBase_Hechos == 2015,5]),col="lightblue",freq=F) # 2003-2006 (Colectivas e individuales) y 2007 - 2015 (Solo individuales)
#Año Ingreso
x11()
hist((Data[Data$AnoBase_Hechos == 2015,6]),col="lightgreen",freq=F) # 2003-2006 (Colectivas e individuales), 2007 - 2011 (Individuales rezagadas), 2012 - 2015 (Individuales - proceso de paz))

#Data<-Data[Data$EstadoCalculado != "Fallecido",]

DataInicial<-Data
write.table(DataInicial,"DatosEntrada.csv",sep=";",row.names=F)


Data$AnoDM<-ifelse(Data$AnoDM <= 2009  ,"2003_2009","2010_2015")
Data$AnoDM<-as.factor(Data$AnoDM)
Data$AnoIng<-ifelse(is.na(Data$AnoIng),
			"",ifelse(Data$AnoIng<= 2009  ,"2003_2009","2010_2015"))
Data$AnoIng<-as.factor(Data$AnoIng)
summary(Data$AnoIng)

Data$Edad<-ifelse(is.na(Data$Edad),"",ifelse(Data$Edad < 18 | Data$Edad >= 100,"Inconsistente",
			ifelse(Data$Edad >= 18 & Data$Edad <= 40,"Adulto Joven","Adulto Mayor")))
summary(Data$Edad)
Data$Edad<-as.factor(Data$Edad)

Data$ExGrupo<-as.character(Data$ExGrupo)
str(Data$ExGrupo)
Data$ExGrupo<-ifelse(Data$ExGrupo == "SINDATO" |Data$ExGrupo == "EPL" |Data$ExGrupo == "ERG" | Data$ExGrupo== "ERP",
			   "OTRO", Data$ExGrupo )
Data$ExGrupo<-as.factor(Data$ExGrupo)
summary(Data$ExGrupo)

summary(Data$NivelFpT)

Data$NivelFpT<-ifelse(Data$NivelFpT == "NO ha tomado cursos FpT","Sin FpT",
			    "Con FpT")
Data$NivelFpT<-as.factor(Data$NivelFpT)
summary(Data$NivelFpT)

summary(Data$NivelEd)
Data$NivelEd<-ifelse(Data$NivelEd == "ALFABETIZACION" | Data$NivelEd == "C1" | Data$NivelEd == "C2",
				     "ALF_BP", ifelse(Data$NivelEd == "C3" | Data$NivelEd == "C4",
				     "BS",ifelse(Data$NivelEd == "C5" |Data$NivelEd == "C6","MV","SIN")))
Data$NivelEd<-as.factor(Data$NivelEd)
summary(Data$NivelEd)
Data$NivelEd<-as.factor(Data$NivelEd)

summary(Data$Reincidencia)
Data[Data$Reincidencia=="",c("Reincidencia")]<-"NO"
summary(Data$Reincidencia)
str(Data)

Data[Data == ""] <- NA
Data<-droplevels(Data)
str(Data)


lapply(3:length(names(Data)),function(x){ print(names(Data)[x])
							print(levels(Data[,x]))})
sapply(Data,function(x){summary(x)})
sapply(Data,function(x) sum(is.na(x)))
sapply(Data, function(x) length(unique(x)))
par(mar=c(6,1,1,1))
missmap(Data, col=c("deeppink","olivedrab3"), main = "",y.labels=NULL,y.at=NULL,y.cex=0.01,x.cex=0.8)
str(Data)
dim(Data)
Data2<-Data[complete.cases(Data),]
dim(Data2)

Data2<-droplevels(Data2)
str(Data2)

lapply(3:length(Data2),function(x){
						   tmp_tbl<-table(Data2$Homicidio,Data2[,x])
						   #print(names(Data2)[x])
						   #print(chisq.test(tmp_tbl))
						   #print(chisq.test(tmp_tbl,simulate.p.value = TRUE))
						   #xtable(prop.table(tmp_tbl)*100)
						   xtable(tmp_tbl)
						})

chisq.test(Data2$Homicidio,Data2[,14])
a<-sapply(Data2[,c(3:13)],function(x) contrasts(x))
lapply(1:length(a),function(x){print.xtable(xtable(a[[x]]), 
							type="html", 
							file=paste(names(a)[x],"_Contrast.html",sep=""))})

set.seed(2)
sp <- split(Data2, list(Data2$Homicidio))
samples <- lapply(sp, function(x) x[sample(1:nrow(x), (nrow(x)*.80), FALSE),])
csamples<- lapply(sp, function(x) x[-sample(1:nrow(x), (nrow(x)*.80), FALSE),])

train <- do.call(rbind, samples)
test <- do.call(rbind, csamples)


model <- glm2(Homicidio ~.,family=binomial,data=train[,c(2:10,12:13)],maxit=100)
model1 <- glm2(Homicidio ~.,family=binomial(link = "logit"),data=train[,c(2:14)],maxit=100)

#?glm
summary(model)
summary(model1)

anova(model, test="Chisq")
anova(model1, test="Chisq")

library(pscl)
pR2(model)

library(ROCR)
p <- predict(model1, newdata=test, type="response")
pr <- prediction(p, test$Homicidio)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

library(MKmisc)
HLgof.test(fit = fitted(model1), obs = train$Homicidio)

library(ResourceSelection)
hoslem.test(train$Homicidio, fitted(model), g=10)


#https://www.r-bloggers.com/evaluating-logistic-regression-models/


fittedtrain <- predict(model,type='response')
validft<-cbind(train[,c(1:2),],fittedtrain)
head(validft)
validft$homipred<-ifelse(validft$fittedtrain > 0.5,"Hom_P","Vivo_P")
head(validft)
fittedtest <- predict(model,newdata=test[,-c(1,2)],type='response')
validfte<-cbind(test[,c(1:2)],fittedtest)
head(validfte)
validfte$homipred<-ifelse(validfte$fittedtest > 0.5,"Hom_P","Vivo_P")
head(validfte)

dim(validft)
dim(validfte)
validaciontotal<-rbind(validft[,c(1:2,4)],validfte[,c(1:2,4)])
dim(validaciontotal)
xtable(table(validaciontotal$Homicidio,validaciontotal$homipred))
a<-table(validaciontotal$Homicidio,validaciontotal$homipred)
a<-a[,c(2:1)]
print(paste('Accuracy',sum(diag(a))/sum(a)))


a

#misClasificError <- mean(fitted.results != test$Homicidio)
#print(paste('Accuracy',1-misClasificError))

write.table(validft,"predtrain.csv",sep=";",row.names=F,dec=",")
write.table(validfte,"predtest.csv",sep=";",row.names=F,dec=",")




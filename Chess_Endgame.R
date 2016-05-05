#Carga los datos
krk <- read.csv("chess.csv", sep=",", header=T)

###EXPLORACIӓN DE LOS DATsO
attach(krk)

# p1=as.matrix(round(margin.table(prop.table(table(Bkblk)),1),2)*100)
# p2=as.matrix(round(margin.table(prop.table(table(Bknwy)),1),2)*100)
# p3=as.matrix(round(margin.table(prop.table(table(Bkon8)),1),2)*100)
# p4=as.matrix(round(margin.table(prop.table(table(Bkona)),1),2)*100)
# p5=as.matrix(round(margin.table(prop.table(table(Bkspr)),1),2)*100)
# p6=as.matrix(round(margin.table(prop.table(table(Bkxbq)),1),2)*100)
# p7=as.matrix(round(margin.table(prop.table(table(Bkxcr)),1),2)*100)
# p8=as.matrix(round(margin.table(prop.table(table(Bkxwp)),1),2)*100)
# p9=as.matrix(round(margin.table(prop.table(table(Blxwp)),1),2)*100)
# p10=as.matrix(round(margin.table(prop.table(table(Bxqsq)),1),2)*100)																				
# p11=as.matrix(round(margin.table(prop.table(table(Cntxt)),1),2)*100)
# p12=as.matrix(round(margin.table(prop.table(table(Dsopp)),1),2)*100)
# p13=as.matrix(round(margin.table(prop.table(table(Dwipd)),1),2)*100)
# p14=as.matrix(round(margin.table(prop.table(table(Hdchk)),1),2)*100)
# p15=as.matrix(round(margin.table(prop.table(table(Katri5)),1),2)*100)
# p16=as.matrix(round(margin.table(prop.table(table(Mulch)),1),2)*100)
# p17=as.matrix(round(margin.table(prop.table(table(Qxmsq)),1),2)*100)
# p18=as.matrix(round(margin.table(prop.table(table(R2ar8)),1),2)*100)
# p19=as.matrix(round(margin.table(prop.table(table(Reskd)),1),2)*100)
# p20=as.matrix(round(margin.table(prop.table(table(Reskr)),1),2)*100)
# p21=as.matrix(round(margin.table(prop.table(table(Rimmx)),1),2)*100)
# p22=as.matrix(round(margin.table(prop.table(table(Rkxwp)),1),2)*100)
# p23=as.matrix(round(margin.table(prop.table(table(Rxmsq)),1),2)*100)
# p24=as.matrix(round(margin.table(prop.table(table(Simpl)),1),2)*100)
# p25=as.matrix(round(margin.table(prop.table(table(Skach)),1),2)*100)
# p26=as.matrix(round(margin.table(prop.table(table(Skewr)),1),2)*100)
# p27=as.matrix(round(margin.table(prop.table(table(Skrxp)),1),2)*100)
# p28=as.matrix(round(margin.table(prop.table(table(Spcop)),1),2)*100)
# p29=as.matrix(round(margin.table(prop.table(table(Stlmt)),1),2)*100)
# p30=as.matrix(round(margin.table(prop.table(table(Thrsk)),1),2)*100)
# p31=as.matrix(round(margin.table(prop.table(table(Wkcti)),1),2)*100)
# p32=as.matrix(round(margin.table(prop.table(table(Wkna8)),1),2)*100)
# p33=as.matrix(round(margin.table(prop.table(table(Wknck)),1),2)*100)
# p34=as.matrix(round(margin.table(prop.table(table(Wkovl)),1),2)*100)
# p35=as.matrix(round(margin.table(prop.table(table(Wkpos)),1),2)*100)
# p36=as.matrix(round(margin.table(prop.table(table(Wtoeg)),1),2)*100)
# p37=as.matrix(round(margin.table(prop.table(table(Class)),1),2)*100)

#Correlaci?n entre variables
drops <- c(13, 15, 36)
Corr1 <- round(cor(krk[,-drops], method="spearman")*100)
print(Corr1)

#Mapa de calor de la correlaci?n
library(gplots)
generate_heat_map <- function(correlationMatrix, title)
{
  heatmap.2(x = correlationMatrix,    
            cellnote = correlationMatrix,   
            main = title,  		
            symm = T,			
            dendrogram="none",		
            Rowv = F,			
            trace="none",			
            density.info="none",		
            notecol="black")		  
}
generate_heat_map(Corr1, "Correlaciones entre variables")

#An?lisis cluster
library(Hmisc)
v <- varclus(~ Bkblk + Bknwy + Bkon8 + Bkona + Bkspr + Bkxbq + Bkxcr + Bkxwp + Blxwp + Bxqsq + Cntxt + Dsopp + Dwipd + Hdchk + 
              Katri5 + Mulch + Qxmsq + R2ar8 + Reskd + Reskr + Rimmx + Rkxwp + Rxmsq + Simpl + Skach + Skewr + Skrxp + Spcop + Stlmt + 
              Thrsk + Wkcti + Wkna8 + Wknck + Wkovl + Wkpos + Wtoeg + Class, data=krk)
plot(v)

#Test U de Mann-Whitney y Chi cuadrado de Pearson para selecci?n de predictores
s <- spearman2(Class ~ Bkblk + Bknwy + Bkon8 + Bkona + Bkspr + Bkxbq + Bkxcr + Bkxwp + Blxwp + Bxqsq + Cntxt + Dsopp + Dwipd + Hdchk + 
                Katri5 + Mulch + Qxmsq + R2ar8 + Reskd + Reskr + Rimmx + Rkxwp + Rxmsq + Simpl + Skach + Skewr + Skrxp + Spcop + Stlmt + 
                Thrsk + Wkcti + Wkna8 + Wknck + Wkovl + Wkpos + Wtoeg, data=krk, p=2)
plot(s)
s <- chiSquare(Class ~ Bkblk + Bknwy + Bkon8 + Bkona + Bkspr + Bkxbq + Bkxcr + Bkxwp + Blxwp + Bxqsq + Cntxt + Dsopp + Dwipd + Hdchk + 
                Katri5 + Mulch + Qxmsq + R2ar8 + Reskd + Reskr + Rimmx + Rkxwp + Rxmsq + Simpl + Skach + Skewr + Skrxp + Spcop + Stlmt + 
                Thrsk + Wkcti + Wkna8 + Wknck + Wkovl + Wkpos + Wtoeg, data=krk)
plot(s)

#Cruce de los valores de los predictores frente a objetivo
c1 <- with(krk, table(Rimmx, Class))
c2 <- with(krk, table(Bxqsq, Class))
c3 <- with(krk, table(Wknck, Class))
c4 <- with(krk, table(Bkxwp, Class))
c5 <- with(krk, table(Katri5, Class))
c6 <- with(krk, table(Wkna8, Class))

###PARTICIӓN DE LA MUESTRA
indices <- sample(1:nrow(krk), size=0.5*nrow(krk)) 
#Datos de entrenamiento
Entrenamiento50 <- krk[indices,]
#Datos de prueba
Prueba50 <- krk[-indices,]

###MODELO DE REGRESIӓN LOG͍STICA
#Usando la funci?n glm (modelo lineal generalizado)
Modelo50 <- glm(Class ~ Bkblk + Bknwy + Bkon8 + Bkona + Bkspr + Bkxbq + Bkxcr + Bkxwp + Blxwp + Bxqsq + Cntxt + Dsopp + Dwipd + 
                     Katri5 + Mulch + Qxmsq + R2ar8 + Reskd + Rimmx + Rkxwp + Rxmsq + Simpl + Skewr + Skrxp + 
                     Thrsk + Wkcti + Wkna8 + Wknck + Wkovl + Wkpos + Wtoeg,
                   family=binomial, data = Entrenamiento50)

#Modelo mediante stepwise
MS <- step(glm(Class ~ Bkblk + Bknwy + Bkon8 + Bkona + Bkspr + Bkxbq + Bkxcr + Bkxwp + Blxwp + Bxqsq + Cntxt + Dsopp + Dwipd + 
                Katri5 + Mulch + Qxmsq + R2ar8 + Reskd + Rimmx + Rkxwp + Rxmsq + Simpl + Skewr + Skrxp + 
                Thrsk + Wkcti + Wkna8 + Wknck + Wkovl + Wkpos + Wtoeg,
              family=binomial, data = Entrenamiento50))
# Bkblk + Bknwy + Bkon8 + Bkona + Bkxbq + Bkxcr + Bxqsq + Cntxt + Dsopp + Katri5 + Mulch + Qxmsq + R2ar8 + Rimmx + Rxmsq + Skrxp + Wkcti + Wkna8
# + Wknck + Wkovl + Wkpos 
# AIC: 249.97

#Modelo mediante backward (el mismo)
MB <- step(glm(Class ~ Bkblk + Bknwy + Bkon8 + Bkona + Bkspr + Bkxbq + Bkxcr + Bkxwp + Blxwp + Bxqsq + Cntxt + Dsopp + Dwipd + 
                Katri5 + Mulch + Qxmsq + R2ar8 + Reskd + Rimmx + Rkxwp + Rxmsq + Simpl + Skewr + Skrxp + 
                Thrsk + Wkcti + Wkna8 + Wknck + Wkovl + Wkpos + Wtoeg,
              family=binomial, data = Entrenamiento50), direction="backward")
# Bkblk + Bknwy + Bkon8 + Bkona + Bkxbq + Bkxcr + Bxqsq + Cntxt + Dsopp + Katri5 + Mulch + Qxmsq + R2ar8 + Rimmx + Rxmsq + Skrxp + Wkcti + Wkna8
# + Wknck + Wkovl + Wkpos 
# AIC: 249.97

MF <- step(glm(Class ~ Bkblk + Bknwy + Bkon8 + Bkona + Bkspr + Bkxbq + Bkxcr + Bkxwp + Blxwp + Bxqsq + Cntxt + Dsopp + Dwipd + 
                Katri5 + Mulch + Qxmsq + R2ar8 + Reskd + Rimmx + Rkxwp + Rxmsq + Simpl + Skewr + Skrxp + 
                Thrsk + Wkcti + Wkna8 + Wknck + Wkovl + Wkpos + Wtoeg,
              family=binomial, data = Entrenamiento50), direction="forward")
# Bkblk + Bknwy + Bkon8 + Bkona + Bkspr + Bkxbq + Bkxcr + Bkxwp + Blxwp + Bxqsq + Cntxt + Dsopp + Dwipd + Katri5 + Mulch + Qxmsq + R2ar8 + Reskd
# + Rimmx + Rkxwp + Rxmsq + Simpl + Skewr + Skrxp + Thrsk + Wkcti + Wkna8 + Wknck + Wkovl + Wkpos + Wtoeg
# AIC: 262.14

#Comprobaci?n de la bondad del modelo
pr <- predict(MS, Entrenamiento50, type="response")
pr2 <- round(pr, 2)
#Histograma de la distribuci?n de las probabilidades para la variable objetivo
hist(pr, breaks=20, col="blue")
hist(pr[Entrenamiento50$Class==1], col="red", breaks=20, add=TRUE)
#Tabla de precisi?n con corte en 0.5
table(actual=Prueba50$Class, predicted=pr>.5)

#M?todo adicional de comprobaci?n
Solucion50 <- fitted.values(MS)
Solucion50 <- round(Solucion50)
#Tabla de rendimiento del modelo
table(Solucion50, Prueba50$Class)

#Curva ROC
library(ROCR)
Prueba50$puntuacion <- predict(MS,type='response',Prueba50)
pred <- prediction(Prueba50$puntuacion,Prueba50$Class)
rendimiento <- performance(pred, "tpr", "fpr")
plot(rendimiento)

#Confianza maxima verosimilitud
confint(Modelo50)
#Confianza error estandar
confint.default(Modelo50)
#Coeficiente modelo
exp(coef(Modelo50))

###Usando el m?todo de verosimilitud penalizada propuesto Firth para evitar problemas de separaci?n
library(logistf)
MFirth <- logistf(Class ~ Bkblk + Bknwy + Bkon8 + Bkona + Bkspr + Bkxbq + Bkxcr + Bkxwp + Blxwp + Bxqsq + Cntxt + Dsopp + Dwipd + 
                   Katri5 + Mulch + Qxmsq + R2ar8 + Reskd + Rimmx + Rkxwp + Rxmsq + Simpl + Skewr + Skrxp + 
                   Thrsk + Wkcti + Wkna8 + Wknck + Wkovl + Wkpos + Wtoeg,
                      family=binomial, data = Entrenamiento50)

MFirth2 <- update(MFirth, Class ~ Bkblk + Bknwy + Bkon8 + Bkona + Bkxbq + Bkxcr + Bxqsq + Cntxt + Dsopp + Katri5 + Mulch + Qxmsq + 
                   R2ar8 + Rimmx + Rxmsq + Skrxp + Wkcti + Wkna8 + Wknck + Wkovl + Wkpos)

#Comprobaci?n de la bondad del modelo
pr2 <- round(MFirth$predict, 2)
#Tabla de precisi?n con corte en 0.5
t1 <- table(actual=Prueba50$Class, predicted=pr2>.5)

#Curva ROC
pred <- prediction(MFirth2$predict,Prueba50$Class)
rendimiento <- performance(pred, "tpr", "fpr")
plot(rendimiento)

###Usando el m?todo de verosimilitud penalizada propuesto Firth para evitar problemas de separaci?n
library(brglm)
MBias <- brglm(Class ~ Bkblk + Bknwy + Bkon8 + Bkona + Bkspr + Bkxbq + Bkxcr + Bkxwp + Blxwp + Bxqsq + Cntxt + Dsopp + Dwipd + 
                   Katri5 + Mulch + Qxmsq + R2ar8 + Reskd + Rimmx + Rkxwp + Rxmsq + Simpl + Skewr + Skrxp + 
                   Thrsk + Wkcti + Wkna8 + Wknck + Wkovl + Wkpos + Wtoeg,
                 family=binomial, data = Entrenamiento50)


#Comprobaci?n de la bondad del modelo
pr <- predict(MBias2, Entrenamiento50, type="response")
pr2 <- round(pr, 2)
#Tabla de precisi?n con corte en 0.5
table(actual=Prueba50$Class, predicted=pr>.5)

#Curva ROC
Prueba50$puntuacion <- predict(MBias2,type='response',Prueba50)
pred <- prediction(Prueba50$puntuacion,Prueba50$Class)
rendimiento = performance(pred, "tpr", "fpr")
plot(rendimiento)


MBias2 <- brglm(Class ~ Bkblk + Bknwy + Bkon8 + Bkona + Bkxbq + Bkxcr + Bxqsq + Cntxt + Dsopp + Katri5 + Mulch + Qxmsq + 
                 R2ar8 + Rimmx + Rxmsq + Skrxp + Wkcti + Wkna8 + Wknck + Wkovl + Wkpos,
              family=binomial, data = Entrenamiento50)
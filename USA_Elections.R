#Carga de paquetes
library(gplots)
library(psych)
library(Hmisc)
library(rms)

#Carga los datos#
counties <- read.csv("counties.csv", sep=";")

describe(counties[,-(1:4)]) #función describe del paquete psych o del paquete Hmisc

Corr1 <- round(cor(counties[,-(1:4)], method="spearman")*100)
print(Corr1)

### Genera mapa de calor

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

seniors <- counties$age6574 + counties$age75
label(seniors) <- "% age >= 65, 1990"
logpob <- logb(counties$pop.density+1, 10)
label(logpob) <- "log 10 of 1992 pop per 1990 miles^2"
logcrime <- logb(counties$crime+1, 10)
label(logcrime) <- "log 10 of serious crimes per 100,000 1991"
logincome <- logb(counties$income, 10)
label(logincome) <- "log 10 of median family income, 1989 dollars"

dd <- datadist(counties)
dd <- datadist(dd, seniors, logpob, logcrime, logincome)
options(datadist='dd')

v <- varclus(~ pop.density + pop.change + seniors + crime + college +
                income + farm + democrat + republican + Perot +
                white + black + turnout, data=counties)
plot(v)

s <- summary(democrat ~ pop.density + pop.change + seniors + crime + college +
                income + farm + white + turnout, data=counties)
plot(s, cex.labels=.6)

s <- spearman2(democrat ~ logpob + pop.change + seniors + logcrime +
                college + logincome + farm + white + turnout,
                data=counties, p=2)
plot(s)

# Modelo con interacción restringida entre college*income
f <- ols(democrat ~ rcs(logpob,4) + rcs(pop.change,3) +
                rcs(seniors,3) + logcrime + rcs(college,5) + rcs(logincome,4) +
                rcs(college,5) %ia% rcs(logincome,4) +
                rcs(farm,3) + rcs(white,5) + rcs(turnout,3), data=counties)
#latex(f)

r <- resid(f)
xYplot(r ~ fitted(f), method='quantile', nx=200,
       ylim=c(-10,10), xlim=c(20,60),
       abline=list(h=0, lwd=.5, lty=2),
       aspect='fill')

p1 = xYplot(r ~ white, method='quantile', nx=200,
            ylim=c(-10,10), xlim=c(40,100),
            abline=list(h=0, lwd=.5, lty=2),
            aspect='fill', data=counties)

p2 = xYplot(r ~ logpob, method="quantile", nx=200,
            ylim=c(-10,10), xlim=c(0,3.5),
            abline=list(h=0, lwd=.5, lty=2),
            "fill", data=counties)

print(p1, split=c(1,1,1,2), more=T)
print(p2, split=c(1,2,1,2), more=T)

qqmath(~r | cut2(logpob,g=4))

# Modelo con interacciones todo * todo
f2 <- ols(democrat ~ (rcs(logpob,4) + rcs(pop.change,3) +
                       rcs(seniors,3) + logcrime + rcs(college,5) + rcs(logincome,4) +
                       rcs(farm,3) + rcs(white,5) + rcs(turnout,3))^2, data=counties)

lm(f2)
f$stats # R2 = 0.5085956 
f2$stats # R2 = 0.6164077

g <- update(f, x=T)
w <- which.influence(g, 0.3)
dffits <- resid(g, 'dffits')
show.influence(w, data.frame(counties, logpob, seniors, logincome, dffits),
               report=c('democrat','dffits'), id=counties$county)

an <- anova(f, tol=1e-13)
an
plot(an, what='partial R2')

incomes <- seq(22900, 32800, length=4)
show.pts <- function(college.pts, income.pt) {
  s = abs(income - income.pt) < 1650
  x = college[s]
  x = sort(x[!is.na(x)])
  n = length(x)
  low = x[10]; high = x[n-9]
  college.pts >= low & college.pts <= high
}

plot(Predict(f, college=NA, logincome=incomes, conf.int=F))

plot(Predict(f))

s <- summary(f)
options(digits=4)
plot(s)

f <- Newlabels(f, list(turnout='voter turnout (%)'))
nomogram(f, interact=list(income=incomes),
         turnout=seq(30,100,by=10),
         lplabel='estimated % voting Democratic',
         cex.var=.8, cex.axis=.75)

plot(nomogram(f, interact=list(logincome=incomes), turnout=seq(30,100,10)))

attach(counties)
plot(counties$state, counties$democrat, main="Distribución del voto:", 
     xlab="Estados de EEUU ", ylab="Voto demócrata ", pch=19)

attach(counties)
plot(counties$democrat, counties$state, main="Gráfico de dispersión", 
     xlab="Democrata ", ylab="Republicano ", pch=19)

model.logistic <- glm(counties$democrat~counties$logcrime+counties$logincome, family=binomial("logit"))

plot(counties$democrat ~ counties$conf)
plot(counties$income ~ counties$college)

plot(lm(counties$democrat ~ counties$black))
abline(lm(counties$democrat ~ counties$white))

f <- ols(democrat ~ logpob + pop.change + logincome + farm + white + turnout + state, data=counties)
f$stats
lm(f)
latex(f)
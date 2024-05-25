install.packages("lmtest")

install.packages(zoo)

install.packages("xtable")

install.packages("stargazer")


install.packages(psych)

install.packages("ggplot2")

install.packages("carData")

install.packages("psych")
library(psych)
install.packages("stats")

install.packages("GGally")

install.packages("car")

install.packages("moments")

install.packages("gvlma")
install.packages("car")
install.packages("systemfit")


install.packages(corrplot)

install.packages("normtest")

install.packages(moments)

install.packages("nortest")
install.packages("tseries")



library(lmtest)
library(zoo)
library(xtable)
library(stargazer)
library(psych)
library(ggplot2)
library(carData)
library(stats)
library(GGally)
library(car)
library(moments)
library(gvlma)
library(corrplot)
library(nortest)
library(moments)
library(tseries)
library(systemfit)




datos=na.omit(datos_organizados_2_0)

linguacomplex=datos$`cl`
pib=datos$ppp
cs=datos$`Indice Capacidad social`
gi=datos$gi
#Logar?tmos
lcl=datos$lcl
lcs=log(cs)
lgi=log(gi)
lpib=log(pib)


cor(linguacomplex,pib)

cor(gi,linguacomplex)
cor(cs,linguacomplex)

cor(cs,pib)

cor(x=lcl, y=cs, method='spearman')
#capacidad social= complejidad + gasto en investigaci?n es el modelo

m1<-lm(cs~linguacomplex+gi, data = datos)
summary(m1)
m2<-lm(lcs~linguacomplex+gi, data = datos)
summary(m2)
m3<-lm(cs~lcl+lgi, data = datos)
summary(m3)
m4<-lm(lcs~lcl+lgi, data = datos)
summary(m4)

AIC(m1,m2,m3,m4)
BIC(m1,m2,m3,m4)
stargazer(m1,m2,m3,m4, type = "text")

hist(linguacomplex)
ui<-residuals(m1)
summary(ui)
cor(linguacomplex,ui)
cor(gi,ui)

m2<-lm(pisa~linguacomplex, data = datos)
summary(m2)

vif(m1)
#IMPORTANCIA RELATIVA DE LAS VARIABLES EXPLICATIVAS EN EL MODELO (DE VITAL IMPORTANCIA)

relweights <- function(regresion,...){   
  R <- cor(regresion$model)    
  nvar <- ncol(R)  
  rxx <- R[2:nvar, 2:nvar]  
  rxy <- R[2:nvar, 1]     
  svd <- eigen(rxx)       
  evec <- svd$vectors     
  ev <- svd$values        
  delta <- diag(sqrt(ev)) 
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2  
  beta <- solve(lambda) %*% rxy  
  rsquare <- colSums(beta ^ 2)   
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100 
  lbls <- names(regresion$model[2:nvar])   
  rownames(import) <- lbls 
  colnames(import) <- "Weights" 
  barplot(t(import),names.arg=lbls,
          ylab="% of R-Square",    
          xlab="Predictor Variables",
          main="Relative Importance of
          Predictor Variables",   
          sub=paste("R-Square=",round(rsquare, digits=3)),  
          ...) 
  return(import) 
}
relweights(m1)

# 2SLS 

#la primera etapa es m1, se sacan las predcciones de ese modelo:
predcs=fitted(m1)
lpredcs=log(predcs)

# predcs es nuestra variable independiente para la segunda etapa 

n1<-lm(pib~predcs, data = datos)
summary(n1)
n2<-lm(lpib~predcs, data = datos)
n3<-lm(pib~lpredcs, data = datos)
n4<-lm(lpib~lpredcs, data = datos)
summary(n4)
AIC(n1,n2,n3,n4)
BIC(n1,n2,n3,n4)
stargazer(n1,n2,n3,n4, type = "text")

#se escoge n4

ui<-residuals(n4)

#Exogeneidad extricta
cor(ui,lpredcs)

#Durbin Watson para la endogeneidad
test_durbin_watson <- durbinWatsonTest(n4)

# Imprime los resultados del test
print(test_durbin_watson)

#Homocedasticidad (h0 es que existe homocedasticidad)
ncvTest(n4)
bptest(n4)
#No multicolinealidad (ya se cumple)
corrplot(cor(dplyr::select((m4), lunem, , , ltur)),
         method = "number", tl.col = "black") 
#Prueba formal
vif(m4)
sqrt(vif(m4)) > 2 
#Normalidad #distribuci?n normal

qqPlot(n4, labels=row.names(datos),
       id.method="identify",  
       simulate=TRUE, 
       main="Q-Q Plot")
residplot <- function(n4, nbreaks=10) {       
  z <- rstudent(n4)            
  hist(z, breaks=nbreaks, freq=FALSE, xlab="Studentized Residual",
       main="Distribution of Errors")            
  rug(jitter(z), col="brown")            
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)     
  lines(density(z)$x, density(z)$y,      
        col="red", lwd=2, lty=2)         
  legend("topright",                     
         legend = c( "Normal Curve", "Kernel Density Curve"),         
         lty=1:2, col=c("blue","red"), cex=.7)        }
residplot(n4)
#Formalmente
skewness(ui)
kurtosis(ui)
shapiro.test(ui)
pearson.test(ui)
jarque.bera.test(ui)
jb.norm.test(ui)



library(normtest)
gvmincer4 <- gvlma(n4) 
summary(gvmincer4)
#h0: correcta especificacion resettest

resettest(n4, power=2:3,type="regressor", data=datos)
?resettest
?htest
install.packages(fRegression)
vcov(modelo)
confint(n4)

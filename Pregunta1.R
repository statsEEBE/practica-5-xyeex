#poblacion
mu<-95.3
sigma<-5.7
curve(dnorm(x, mean<-mu,sd=sigma),
      xlim=c(80,120))

rnorm(1, mu, sigma)
pnorm(98,mu, sigma)#la probabilitat de que sigue como maximo 98
pnorm(97,mu, sigma)

set.seed(123)
Y <- function(i)(sum(rnorm(4, mu, sigma)))#suma de la muestra o suma de la resistencia de la muestra
Y(1) 
#valor esperado
Y10<- sapply(1:10,Y)
Y10
mean(Y10)
###
Y100000<- sapply(1:100000,Y)
hist(Y100000)
mean(Y100000)
####EN TEORIA la media de la suma muestral
####de tamaño n=4
4*mu
###VARIANZA de la suma,muestral
4*sigma^2
var(Y100000)
###
hist(Y100000, freq=FALSE)

curve(dnorm(x, mean<-4*mu, sd=sqrt(4)*sigma), 
            add=TRUE)
###
Y <- function(i)(sum(rnorm(100, mu, sigma)))#suma de la muestra o suma de la resistencia de la muestra
Y100000<-sapply(1:10000,Y)
var(Y100000)
###PNORM MEANS LA PROBABILITAT DE QUE SEA MENOR QUE EL NUMERO Q INDUIMOS
P <- 1-pnorm(103,mu, sigma) # LA PROBABILITAT DE QUE LA RESISTENCIA SIGUIN MAYOR IGUAL QUE 103
P


Y <- function(i)(sum(rnorm(1, mu, sigma)))
Y100000<-sapply(1:100000,Y)
hist(Y100000)
Y10000 > 103
####
xbar <- function(i)(mean(rnorm(4, mu, sigma)))
xbar100000<-sapply(1:100000,xbar)
hist(xbar100000)
mean(xbar100000 < 98)
pnorm(98,mu,sigma/sqrt(4))### sigma/sqrt(n) es el error estandard de promedio commo dice el ejercicio la probabilitat de mijana
####32
Ssq<- function(i)(var(rnorm(100, mu, sigma)))
Ssq100000<-sapply(1:100000,Ssq)
hist(Ssq100000)
mean(Ssq100000 > 32)
Z <- 1- pchisq((100-1)*32/sigma^2, 100-1)# (N-1)*32/SIGMA^2
Z
hist(Ssq100000*(100-1)/sigma^2, prob=TRUE)
curve(dchisq(x, 100-1), add= TRUE, col="red")
(100-1)*32/sigma^2


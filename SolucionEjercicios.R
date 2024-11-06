# Solucion Ejercicio1

x1 <- seq(0,1,0.001)   # Posibles resultados
f <- function(x) {4/(pi*(1+x^2))}
f1 <- f(x1) # Probabilidad de ocurrencia de cada resultado
curve(f,0,1,col="red",lwd=5, main="Función de densidad", xlab="X",
      ylab="f(x)")


mean.X <- 0.4412712
var.X <- 0.07851927



N <- 50 # Número de muestras
n <- 4 # tamaño de la muestra
set.seed(321)
samples <- sample(x1,n*N,replace=TRUE,prob=f1) # Simulación de N * n eventos
samples.X = as.data.frame(matrix(samples, ncol=n)) # Organización en un data.frame



sum.samples.X = apply(samples.X,1,sum)



mean(sum.samples.X)



var(sum.samples.X)


med.samples.X = apply(samples.X,1,mean)


mean(med.samples.X)



var(med.samples.X)


var.samples.X = apply(samples.X,1,var)


mean(var.samples.X)


n2 <- 50
pnorm(24, mean=n2*mean.X, sd=sqrt(n2*var.X))-pnorm(17, mean=n2*mean.X, sd=sqrt(n2*var.X))

pnorm(0.41, mean=mean.X, sd=sqrt(var.X/n2))-pnorm(0.37, mean=mean.X, sd=sqrt(var.X/n2))


# Solucion Ejercicio2


mean.Y <- 80
var.Y <- 15^2
n <- 9
pnorm(800, mean=n*mean.Y, sd=sqrt(n*var.Y))-pnorm(700, mean=n*mean.Y, sd=sqrt(n*var.Y))


pnorm(80, mean=mean.Y, sd=sqrt(var.Y/n))-pnorm(78, mean=mean.Y, sd=sqrt(var.Y/n))


pchisq(250*(n-1)/var.Y,df=n-1)-pchisq(200*(n-1)/var.Y,df=n-1)


N <- 100 # Número de muestras
n <- 9 # tamaño de la muestra
set.seed(321)
samples <- rnorm(N*n, mean=mean.Y, sd=sqrt(var.Y)) # Simulación de N * n eventos
samples.Y = as.data.frame(matrix(samples, ncol=n)) # Organización en un data.frame



sum.samples.Y = apply(samples.Y,1,sum)
hist(sum.samples.Y,prob=T)



hist(sum.samples.Y,prob=T)
curve(dnorm(x,mean=n*mean.Y,sd=sqrt(n*var.Y)), add=T, lwd=2, col="red")


med.samples.Y = apply(samples.Y,1,mean)
hist(med.samples.Y,prob=T) 


hist(med.samples.Y,prob=T) 
curve(dnorm(x,mean=mean.Y,sd=sqrt(var.Y/n)), add=T, lwd=2, col="red")


var.samples.Y = apply(samples.Y,1,var)
hist(var.samples.Y,prob=T) 


hist(var.samples.Y,prob=T) 
curve(dchisq(x,df=n-1), add=T, lwd=2, col="red")

hist(var.samples.Y*(n-1)/var.Y,prob=T) 

hist(var.samples.Y*(n-1)/var.Y,prob=T) 
curve(dchisq(x,df=n-1), add=T, lwd=2, col="red")




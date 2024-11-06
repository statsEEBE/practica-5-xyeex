# Practica5

## Material Practica 5 

Distribuciones de Muestreo: 

[diapos](https://github.com/alejandro-isglobal/alejandro-isglobal.github.io/blob/master/slides/Practica6.pdf), [capture.txt](https://github.com/alejandro-isglobal/alejandro-isglobal.github.io/blob/master/data/capture.txt) 

- PDFs (antigua Práctica 6): [diapos](https://github.com/alejandro-isglobal/alejandro-isglobal.github.io/blob/master/slides/Practica6.pdf)

- datos: [capture.txt](https://github.com/alejandro-isglobal/alejandro-isglobal.github.io/blob/master/data/capture.txt) 

- Videos: [Video 1](https://youtu.be/Ia1WOXowoYY) , [Video 2](https://youtu.be/igqlRlxjVrs), [Video 3](https://youtu.be/Iwj4eg1uZhs)


## Trabajo en clase

comandos:


- <code>sample()</code>
- <code>curve()</code>
- <code>dnorm()</code>
- <code>pnorm()</code>
- <code>qnorm()</code>
- <code>rnorm()</code>
- <code>dchisq()</code>



[Pregunta 1](Pregunta1.png)


# Ejercicio 1

Consideramos la variable aleatoria $X$
que tiene la siguiente función de densidad:

$$f(x)= 
\begin{cases}
\frac{4}{\pi(1+x^2)}, &0 <x  < 1 \\
0,& resto \\
\end{cases}
$$

- Su valor esperado ($\mu$) es 0.4412712 y su varianza ($\sigma^2$)
) es 0.07851927. Representa gráficamente dicha función de densidad.


- Simula 50 muestras de tamaño n
 igual a 4 (utiliza la semilla 321).

- Calcula la suma de las observaciones de cada muestra

- ¿Cuál es el valor de la media de la suma muestral?

- ¿Qué relación tiene (aproximada) con la media de la población?
  - son iguales
  - es $n$ veces mayor
  - es $n$ veces menor
  - es $\sqrt{n}$ veces mayor
  - es $\sqrt{n}$ veces menor
  
- ¿La distribución de la suma muestral es exactamente a una $N(n\mu,n\sigma^2)$
?
  - Sí
  - No
  
- Calcula la media de cada muestra

- ¿Cuál es el valor de la media de la media muestral?

- ¿Qué relación tiene (aproximada) con la media de la población?
  - son iguales
  - es $n$ veces mayor
  - es $n$ veces menor
  - es $\sqrt{n}$ veces mayor
  - es $\sqrt{n}$ veces menor

- ¿Cuál es el valor de la varianza de la media muestral?
- ¿Qué relación tiene con la varianza de la población?
  - son iguales
  - es $n$ veces mayor
  - es $n$ veces menor
  - es $\sqrt{n}$ veces mayor
  - es $\sqrt{n}$ veces menor

- ¿La distribución de la suma muestral es exactamente a una $N\mu,\sigma^2/n)$
  - Sí
  - No
  
- Calcula la varianza de de cada muestra
- ¿Cuál es el valor de la media de la varianza muestral?

- ¿Qué relación tiene (aproximada) con la varianza de la población?
?
  - son iguales
  - es $n$ veces mayor
  - es $n$ veces menor
  - es $\sqrt{n}$ veces mayor
  - es $\sqrt{n}$ veces menor
  
- ¿La distribución de la varianza muestral es exactamente una $\chi^2_{n-1}$
?  
  - Sí
  - No
  
- Si el tamaño de la muestra es 50, ¿Cuál de las siguientes afirmaciones es incorrecta?
  - $\mu_\bar{x}=\mu_x$:  La media de la media muestral es igual a la media de población
  - $X\rightarrow N(\mu,\sigma^2/n)$ La distribucion de la suma es una normal
  - $T\rightarrow N(\mu μ,n σ^2/n)$
  - $(n−1)\frac{S^2}{\sigma^2} \rightarrow \chi^2_{n−1}$

- Calcula la probabilidad de que:
  - La suma de la muestra esté entre 17 y 24
  - La media de la muestra esté entre 0.39 y 0.41
  - La varianza de la muestra esté entre 0.05 y 0.15
  
# Ejercicio 2

Los pesos de los hombres adultos de una determinada población se distribuyen normalmente, con una media de 80 kg y una desviación estándar de 15 kg.

- Calcula la probabilidad de que:
  - La suma del peso de 9 hombres esté entre 700 y 800kg.
  - La media del peso de 9 hombres esté entre 78 y 80kg
  - La varianza del peso de 9 hombres esté entre 200 y 250
  - Simula 100 muestras de tamaño $n$ igual a 9 (utiliza la semila 321)
  - Haz el histograma de la suma muestral
  
¿La distribución de la suma muestral se puede aproximar a una $N(n\mu,n\sigma^2)$
? Compare el histograma y la distribución aproximada.


- Haz el histograma de la media muestral

¿La distribución de la media muestral se puede aproximar a una $N(\mu,\sigma^2/n)$
? Compare el histograma y la distribución aproximada.


- Haz el histograma de la varianza muestral

¿La distribución de la varianza muestral se puede aproximar a una $\chi^2_{n−1}$
? Compare el histograma y la distribución aproximada.


- Haz el histograma del estadístico $(n−1)\frac{S^2}{\sigma^2}$

- ¿La distribución del estadístico $(n−1)\frac{S^2}{\sigma^2}$
 se puede aproximar a una $\chi^2_{n−1}$
? Compare el histograma y la distribución aproximada.

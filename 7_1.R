#Tema 1
#En baseball se suele reclutar mas a jugadores que roban bases que a jugadores que
#consiguen bases por bola. Vamos a tratar de ver si esto es eficiente o no.
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

#Primero grafiquemos la relacion entre home runs y carreras, que claramente sera positiva
#porque a cada hr corresponde al menos una carrera.
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#Ahora veamos la relacion entre bases robadas y carreras.
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#Y finalmente la relacion entre bases por bola y carreras.
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
#Vemos que esta ultima es muy parecida a la primera, pero esto puede deberse a un factor
#externo que afecte a ambas o a que los home runs tambien producen bases por bola. Para
#corroborar si este es el caso usamos regresiones.
?Teams

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(win_rate = W / G, E_per_game = E / G) %>%
  ggplot(aes(win_rate, E_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(doubles_per_game = X2B / G, triples_per_game = X3B / G) %>%
  ggplot(aes(doubles_per_game, triples_per_game)) + 
  geom_point(alpha = 0.5)

#Tema 2
#Las regresiones nacieron por un analisis genetico realizado por Galton. La pregunta que
#Galton estaba tratando de contesatar es si puede predecir la altura de un hijo por la
#altura de sus papas. El creo el concepto de regresion y el de correlacion. Usaremos sus
#datos originales:
library(HistData)
data("GaltonFamilies")
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

#Podemos ver algunos datos relevantes de la informacion de Galton, por ejemplo el promedio
#y la desviacion estandar de las alturas en cada generacion.
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

#Sin embargo, estos datos dejan de lado un detalle muy importante: el hecho de que a mayor
#altura del padre, es mas alto el hijo. Es decir, hay correlacion.
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

#La formula de la correlacion es la sumatoria de: ((xi-mx)/sdx)-((yi-my)/sdy) donde m es
#el promedio y sd la desviacion estandar. Hay correlacion positiva cuando el valor tiende
#a 1, inversa cuando tiende a -1 y no hay correlacion cuando tiende a cero. En nuestro
#caso es de .501
galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

#Cuando en lugar de una poblacion completa tenemos una muestra, la correlacion en realidad
#es una variable aleatoria. Esto es asi porque pudimos haber tenido una muestra diferente.
#Imaginemos que Galton solo hubiera tenido acceso a 25 parejas de padre e hijo:
set.seed(0)
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son))
R

#Podemos repetir esto muchas veces y crear 1000 Rs con su distribucion. Veremos que la
#media de esta distribucion es cercana a 0.501, aunque tendra una sd mucho mas grande.
B <- 1000
N <- 50
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

mean(R)
sd(R)

#La pregunta entonces seria si con la N que tenemos es suficiente para adecuarnos a una
#distribucion normal. En este caso, no lo es.
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  summarize(r = cor(R_per_game, AB_per_game)) %>% pull(r)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W/G, E_per_game = E/G) %>%
  summarize(r = cor(W_per_game, E_per_game)) %>% pull(r)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X2B_per_game = X2B/G, X3B_per_game = X3B/G) %>%
  summarize(r = cor(X3B_per_game, X2B_per_game)) %>% pull(r)

#Tema 3
#La correlacion no siempre es un resumen adecuado de los datos. Es un estimado, pero a
#veces puede resultar util para predecir.
#Ver: https://en.wikipedia.org/wiki/Anscombe%27s_quartet
#Para entender cuando una correlacion puede ser significativa tomemos el ejemplo de
#tratar de predecir la altura de un hijo usando la del padre.
#De saque sabemos que la altura promedio son 70.5 in. Pero que pasa si el papa mide 72?
#Vamos a buscar el promedio de altura de los hijos CONDICIONADO a que el papa mide 72 in

#El problema es encontrar padres que midan exactamente 72. Hay pocos:
sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)

#Sacamos el promedio condicional:
conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>%
  pull(avg)
conditional_avg
#Y predecimos que el hijo medira 71.84

#Podemos estratificar el promedio de altura de los hijos para cada in que mida el padre
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

#El promedio parece aumentar, hay una linea con pendiente de aprox. 0.5, que es igual a
#la correlacion entre la altura del padre y del hijo (0.501)
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

#Podemos agregar la linea de regresion a la grafica:
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x
b <- mu_y - m*mu_x
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

#Si recordamos que y=b+mx, donde b es el intercepto y m es la pendiente, al momento de
#sustituir con los valores de arriba la pendiente es rho (la correlacion, r) multiplicada
#por la desviacion de y entre la desviacion de x y el intercepto es el promedio de y
#menos la pendiente por el promedio de x
#Podemos ver que esto funciona tambien si estandarizamos todo y lo ponemos a escala.
galton_heights %>%
  ggplot(aes(scale(father), scale(son))) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r)

#La diferencia entre una linea de regresion y un promedio condicional es que el segundo
#utiliza una cantidad limitada de datos, lo que lo vuelve poco estable. Con la regresion
#usamos todos los datos para predecir la pendiente y el intercepto y con estos valores
#podemos predecir, ya que solo tenemos que sustituir en x la altura del padre y la
#formula nos permitira sacar el valor para el hijo.
b+m*72

#Cuando dos variables se pueden aproximar con una distribucion bivariada normal, su
#grafica parece en forma ovalada. Mientras menos dispersos estan los puntos en el ovalo
#mayor es la correlacion.
#La pregunta es si nuestra variable predictiva (altura del padre) es distribuida de forma
#normal. Aqui podemos ver que si (la grafica esta hecha con desviaciones estandar).
galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)
#Esto implica que podemos usar esta variable "normal" x, para predecir el resultado de
#una variable "normal" y. En este caso, la altura del hijo.

#No es lo mismo predecir X dado Y que Y dado X. Son dos lineas de regresion diferentes
#porque los supuestos iniciales son diferentes:
#Originalmente teniamos esto para predecir la altura del hijo dada la del padre.
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x
#RESULTADO = 35.7 + .5X
#Pero tambien podemos sacar esto para predecir la altura del padre dada la del hijo.
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y
#RESULTADO = 34 + .5Y

#Podemos hacer lo mismo con datos de madres e hijas.
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mu_xf <- mean(female_heights$mother)
mu_yf <- mean(female_heights$daughter)
s_xf <- sd(female_heights$mother)
s_yf <- sd(female_heights$daughter)
rf <- cor(female_heights$mother, female_heights$daughter)
mf_1 <-  rf * s_yf / s_xf
bf_1 <- mu_yf - mf_1*mu_xf

bf_1+(mf_1*60)


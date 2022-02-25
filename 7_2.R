#TEMA 1:
library(tidyverse)
library(Lahman)

#La pendiente entre bases por bola y carreras es mas alta que la de singles y carreras
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef  %>%
  .[2]
singles_slope

#Esto parece indicar que BB generan HR. Sin embargo correlacion no implica causalidad.
#Cuando un jugador anota muchos HR, los pitchers prefieren darle una BB que arriesgarse a
#que les metan un HR, por lo que los HR causan que aumenten las BB, no al reves.
#Podemos ver la correlaicon entre BB y HR, Singles y HR, BB y Singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))

#La mas alta es la primera, pero porque los HR causan las BB. Se dice que:
#bases on balls are confounded with homeruns. (correlacionadas?)
#Aun asi, puede que las BB sigan siendo utiles para generar carreras. Para separar el
#efecto de los HR sobre las BB, podemos mantener el numero de HR fijo estratificando como
#lo hicimos con las alturas de padres e hijos.
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)
#Asi se ve la grafica de carreras por juego y BB por juego para cada posible estrato de
#HR entre 0.4 y 1.2.
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)

#Antes la pendiente entre BB y carreras era de .735, ahora podemos ver que para cada caso
#es  menor, y mas cercano a los valores de la pendiente de singles (.449):
dat %>%  
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))

#Tambien podemos hacer lo contrario, para checar si las BB estratificadas generan HR.
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game)) 

#Para no tener que hacer este analisis para cada estrato en cada variable podemos unirlas
#en un solo modelo que tome en cuenta el efecto que tiene cada variable sobre la otra y
#tambien sobre la variable que estamos tratando de predecir. Esto es una regresion lineal
#multivariada.

#Linnear regression has become one of the most widely used tools in data science.
#One reason for this has to do with the fact that regression permits us to find
#relationships between two variables while adjusting for others.

#TEMA 2:
#Ahora entraremos a minimos cuadrados ordinarios con la base de datos de Galton.
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

#El primer paso es calcular la suma residual de los cuadrados (rss) que nos permitira
#encontrar los valores de minimos cuadrados ordinarios y que se puede hacer con esta
#funcion:
rss <- function(beta0, beta1){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

#Los minimos cuadrados son aquellos dos valores que minimizan los valores de la rss. Si
#fijamos beta0 en 25, podemos buscar que valor de beta1 minimiza la rss en esta grafica.
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))
#En este caso, el minimo parece encontrarse con una beta1 en 0.65, pero solo si asumimos
#que beta0 es 25. Para no tener que asumir esto podemos derivar e igualar a cero, pero R
#lo hace con una funcion: lm(). 
#Si nuestro modelo es Yi = B0 + B1(x1) + ei, en la funcion se pondria como
#modelo <- lm(Yi ~ x1, data = ____)
fit <- lm(son ~ father, data = galton_heights)
fit
#El intercepto se agrega de manera automatica. Podemos ver mas informacion de la
#regresion con esta funcion:
summary(fit)

#Hay que recordar que los datos que tenemos pudieron haber sido otros. La aleatoreidad en
#la seleccion de datos implica que los estimados pudieron haber sido diferentes. Veamos
#como con una simulacion montecarlo.
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

#Aqui vemos como salio beta0 y beta1 en cada una de nuestras simulaciones.
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)
#Sigue siendo una normal, pero son datos muy diferentes.

sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>%
  .$coef
#Los estimados son parecidos en ambos casos.
lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

#Las regresiones que estimamos nos permiten crear intervalos de confianza. Una forma es
#con una grafica que los incluya:
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

#Tambien poemos predecir Y directamente, con una grafica lineal.
fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  ggplot(aes(father, Y_hat))+
  geom_line()

#TEMA 3:
#Cuando quisimos checar la pendiente de carreras por BB para diferentes grupos de HR
#hicimos esto:
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)
dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))
#Y nos dio un valor para todos los grupos desde .4 hasta 1.2
#El tema es que con lm no podemos hacer eso porque lm no reconoce los grupos. Los grupos
#son parte de tidyverse, y lm no lo es.
dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef
#Cuando agrupas algo con group_by o con summarize se convierte en una tibble (tbl). Son
#dataframes mas modernos con cuatro diferencias principales.
dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class()

#Diferencia 1: Es mucho mas facil de leer una tibble que un dataframe:
Teams #Dataframe
as_tibble(Teams) #Tibble
#Diferencia 2: Si sacas un subset de un dataframe deja de ser un dataframe y se convierte
#en otra cosa. Una tibble, incluso subseteada, sigue siendo una tibble.
class(Teams[,20]) #df
class(as_tibble(Teams[,20])) #tbl
#Si quiero ver la clase de un vector en tibble, puedo usar el signo de dolar:
class(as_tibble(Teams)$HR)
#En la misma linea de ideas, si trato de acceder a una columna que no existe en un df,
#simplemente me aparece NULL. En una tibble, me da una advertencia especifica.
Teams$hr
as_tibble(Teams)$hr
#Diferencia 3: Una tibble puede tener objetos complejos, como funciones.
tibble(id = c(1, 2, 3), func = c(mean, median, sd))
#Diferencia 4: Las tibbles se pueden agrupar.

#La funcion do() es un puente entre las funciones de R y las de tidyverse. do() siempre
#nos dara un dataframe como output. En este ejemplo, tenemos un dataframe con el estrato
#de HR en una columna y una regresion lm para cada estrato en la otra.
dat %>%  
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .))
#Tenemos que ponerle nombre a la columna (en este caso "fit") porque de otro modo, el
#resultado de do() serian muchas lm's, pero no un data frame, y eso nos daria un error.
dat %>%
  group_by(HR) %>%
  do(lm(R ~ BB, data = .))

#Lo que podemos hacer es crear una funcion que nos devuelva, para cada caso un df con el
#coeficiente y el error estandar:
get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2], 
             se = summary(fit)$coefficient[2,2])
}

#Asi, cuando lo pongamos en do(), el resultado final ya sera un df:
dat %>%  
  group_by(HR) %>%
  do(get_slope(.))

#En este caso, ya no hay que definir la columna porque los datos que tenemos ya se
#acomodan como df. Si definimos la columna, seria una columna con un df en cada fila:
dat %>%  
  group_by(HR) %>%
  do(slope = get_slope(.))

#Un ultimo tema es que si tenemos df con mas de una entrada por grupo, se van a 
#concatenar de manera adecuada, acomodando el orden correcto:
get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             estimate = fit$coefficients, 
             se = summary(fit)$coefficient[,2])
}
dat %>%  
  group_by(HR) %>%
  do(get_lse(.))
#En este caso, en lugar de ser solo la slope, es el intercepto tambien.
#Todo esto se puede hacer mas sencillo con el paquete broom. Broom tiene tres funciones
#principales que toman la informacion producida por un objeto lm y lo convierten a datos
#que se pueden utilizar en tidyverse: tidy(), glance() y augment().
#tidy() devuelve una tibble con los coeficientes, el error est., t-stat y p-value.
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)
#Le podemos pedir tambien intervalos de confianza.
tidy(fit, conf.int = TRUE)

dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE))
#Con esto ya podemos sacar una tibble con coeficientes e intervalos de confianza para
#el efecto de las BB sobre las R para cada estrato de HR.
dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>% #Filtramos para que no nos incluya el intercepto.
  select(HR, estimate, conf.low, conf.high) #Quitamos variables que no queremos ver.

#Y eso lo podemos ggplotear:
dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

#Para ver estadisticas de toda la regresion podemos usar glance(). Veremos augment() mas
#adelante.
glance(fit)

#Ejercicio: Pares por genero.

library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

galton_pairs <- galton %>%
  group_by(pair) %>%
  summarize(n = n())

galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight))


galton %>%  
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight") %>%
  select(pair, estimate, conf.low, conf.high) %>%
  ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

#TEMA 4:
#Ya podemos hacer una regresion multivariada:
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

#E incluso incluir singles, dobles y triples:
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

#Para estas regresiones usamos datos previos a 2002. Podemos ver que tan buena es la
#regresion tratando de predecir las carreras por equipo en 2002. La grafica queda asi:
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()
#En vertical las observadas y en horizontal las predicciones.
#Para traducir estas estadisticas a valores de jugador, y no valores de equipo, tenemos
#que convertirlas segun cuantas veces aparecio cada jugador:
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

#Queremos seleccionar buenos jugadores para 2002, asi que tomaremos los datos de 1999 y
#2001, los agrupamos por jugador y creamos las estadisticas para cada uno. Filtramos
#para tener a jugadores que aparecieron al menos 300 veces y agregamos una columna con
#valores predictivos.
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

#La prediccion indica cuantas carreras anotaria un equipo si sus nueve bateadores fueran
#exactamente iguales al bateador i. Asi se ve el histograma de los jugadores.
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))

#Tenemos que agregar los salarios para construir nuestro equipo porque tenemos un 
#presupuesto limitado. Nuestro presupuesto es de 40 millones de dolares. Hay una base de
#datos de salarios en la fuente de Lahman. La tomamos y la unimos a la que ya teniamos.
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

#Tambien tenemos que saber en que posicion juegan porque tenemos que cubrir todas las
#posiciones. Los jugadores pueden cambiar de posicion cada ano, por lo que mas bien vamos
#a tomar la posicion que mas han jugado en su carrera. Tambien quitamos las posiciones de
#outfilder y pitcher. Los pitchers no batean y los OF son una combinacion de otras tres
#posiciones ya existentes.
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

#Finalmente, cambiamos la clave que tienen por nombres y apellidos.
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

#Veamos a los 10 mejores:
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

#Podemos ver que si hay relacion entre salario y carreras, pero aun hay algunos jugadores
#con muchas carreras y bajos salarios.
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

#El problema es que estos suelen ser novatos que todavia no se sientan a negociar un
#salario, sino que tienen el de base. Para corregir esto, eliminamos a los que entraron
#antes de 1998.
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()
#De aqui, facilmente podemos escoger jugadores que por el mismo salario, corran mas.

#ESTO NO LO  VIMOS EN EL CURSO. ES UN CODIGO PARA GENERAR UN ALGORITMO QUE ESCOJA LOS
#NUEVE JUGADORES
library(reshape2)
library(lpSolve)

players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 

our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))
#HASTA AQUI LLEGA ESA PAUSA.

#Hoy en dia hay una nueva forma de medir que le da mayor peso a los HR que a los triples
#que a los dobles y que a los singles. Se llama OPS.

#Existe lo que se llama sophomore slump. Como lo que le paso a Tyler Herro. La pregunta
#es si eso se puede comprobar con datos para el baseball. Podemos modificar la base de
#datos de Lehman para ver si si. Usaremos los datos de bateo del segundo ano de los ROY.
#Empecemos por crear una tabla con ID de los jugadores, nombre y posicion mas jugada.
library(Lahman)
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

#Ahora, creemos una tabla para solo los rookies of the year y enfoquemosla en bateo,
#dejando fuera a los pitchers.
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")

#Ahora vamos a dejar solo las primeras dos temporadas, la de novato y la sophomore
ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

#Ahora usamos spread para separar el bateo de la temporad rookie y la sophomore:
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY
#Proporcion de jugadores que batearon peor en su segundo ano:
mean(ROY$sophomore - ROY$rookie <= 0)

#Para entender porque sucede esto veamos datos de todos los jugadores que batearon mas de
#130 veces (el minimo necesario para ganar ROY) en 2013 y 2014.
two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`, `2014`)
two_years
#Los mejores bateadores de 2013 tambien bajan de nivel en 2014, aunque no sean novatos o
#ROY. Por otro lado si vemos a los peores bateadores de 2013...
arrange(two_years, `2013`)
#... Vemos que en 2014 suben de nivel. Lo que pasa es que la correlacion entre resultados
#de dos anos seguidos es alta pero no perfecta. Es de .460
qplot(`2013`, `2014`, data = two_years)
summarize(two_years, cor(`2013`,`2014`))
#Cuando los ROY tienen una muy buena temporada, en el siguiente ano les ira un poco peor
#porque estan regresando al promedio en el que deberian de estar y en el que todos los
#demas estan. No es un sophomore jinx, es un regreso a la media.

#Los modelos tambien se pueden utilizar para tratar de reducir los errores de medicion
#que se presentan a la hora de reunir datos. Por ejemplo, imaginemos que estos son los
#datos recopilados de una pelota cayendo de una torre, donde tenemos el tiempo que lleva
#callendo y la distancia que ha caido.
library(dslabs)
falling_object <- rfalling_object()
falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")
#Vemos que la mayoria de los datos estan en una parabola, pero algunos parecen fuera de
#lugar. Esto se puede deber a un error en la medicion. Para resolverlo, podemos correr
#un modelo en el que los errores son aleatorios, independientes y con media 0, por lo que
#el modelo se deshace de ellos.
fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)
tidy(fit)
#Vemos que las predicciones del modelo si coinciden con la parabola.
augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")
#El ejemplo se tomo de una pelota cayendo de la torre de Pisa. Los datos de la altura,
#velocidad y aceleracion de la pelota en la vida real coinciden con los intervalos de
#confianza de la regresion
tidy(fit, conf.int = TRUE)


#USADO EN PREGUNTAS
fit <- Teams %>% 
  filter(yearID %in% 1971) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)


res <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  ungroup() 
res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")

res <- res %>%
  filter(term == "BB")
res  
fit<-lm(estimate ~ yearID, data = res)
tidy(fit)

#CORRELACION NO ES CAUSALIDAD. Confundirlas puede suceder por una variedad de razones.
#1. Cherry picking:
#A veces un par de variables no estan relacionadas, a menos que corras el experimento las
#suficientes veces para encontrar que si. En este ejemplo, nosotros construimos X y Y de
#manera independiente y tomamos muestras aleatorias. Checamos la correlacion de cada
#simulacion y decidimos presentar las correlaciones mas altas. Obviamente, X y Y no
#tienen nada que ver en este caso.
N <- 25
g <- 100000
sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N * g))
res <- sim_data %>% 
  group_by(group) %>% 
  summarize(r = cor(x, y)) %>% 
  arrange(desc(r))
res
sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(x, y)) +
  geom_point() + 
  geom_smooth(method = "lm")
res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")
library(broom)
sim_data %>% 
  filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(y ~ x, data = .)))

#2. Outlyers:
#Datos demasiado grandes o pequenos que nos pueden mover toda la correlacion. Spearman
#resuelve esto estimando usando el rango en que se encuentra una variable en lugar de la
#variable en si misma.
set.seed(1985)
x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])
qplot(x, y, alpha = 0.5)
cor(x,y)
cor(x[-23], y[-23])
qplot(rank(x), rank(y))
cor(rank(x), rank(y))
cor(x, y, method = "spearman")

#3. Causalidad Inversa:
#La altura de los padres causa la de los hijos, pero si hacemos el modelo al reves,
#pareciera como si la altura de los hijos causara la de los padres.
library(HistData)
data("GaltonFamilies")
GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight) %>% 
  do(tidy(lm(father ~ son, data = .)))

#4. Confounders (tercera variable):
#Aqui el caso aceptacion por genero. La tercera variable es el nivel de exigencia de las
#carreras a las que aplica cada genero.
# UC-Berkeley admission data
library(dslabs)
data(admissions)
admissions

# percent men and women accepted
admissions %>% group_by(gender) %>% 
  summarize(percentage = 
              round(sum(admitted*applicants)/sum(applicants),1))

# test whether gender and admission are independent
admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  do(tidy(chisq.test(.)))

# percent admissions by major
admissions %>% select(major, gender, admitted) %>%
  spread(gender, admitted) %>%
  mutate(women_minus_men = women - men)

# plot total percent admitted to major versus percent women applicants
admissions %>% 
  group_by(major) %>% 
  summarize(major_selectivity = sum(admitted * applicants) / sum(applicants),
            percent_women_applicants = sum(applicants * (gender=="women")) /
              sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()

# plot number of applicants admitted and not
admissions %>%
  mutate(yes = round(admitted/100*applicants), no = applicants - yes) %>%
  select(-applicants, -admitted) %>%
  gather(admission, number_of_students, -c("major", "gender")) %>%
  ggplot(aes(gender, number_of_students, fill = admission)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(. ~ major)

admissions %>% 
  mutate(percent_admitted = admitted * applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")

# condition on major and then look at differences
admissions %>%
  ggplot(aes(major, admitted, col = gender, size = applicants)) + geom_point()

# average difference by major
admissions %>%  group_by(gender) %>% summarize(average = mean(admitted))
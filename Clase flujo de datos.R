gapminder<- read.csv("data/gapminder-FiveYearData.csv")
gapminder
any(gapminder$year == 2002)
if (any(gapminder$year == 2002)){print("Hay elementos del año 2002")} else print("Algo")
any(gapminder$year == 2012)
if (any(gapminder$year == 2012)){print("Hay elementos del año 2012")} else print("Algo")
vector_salida <- c()
vector_salida
for(i in 1:5){
  for(j in c("a", "b", "c", "d", "e")){
    vector_salida <- c(vector_salida, paste(i, j))
  }
}
vector_salida
matrix_salida <- matrix(nrow = 5, ncol=5)
vector_j <- c("a", "b", "c", "d", "e")
for(i in 1:5){
  for(j in 1:5){
    valor_j_temp <- vector_j[j]
    salida_temp <- paste(i, valor_j_temp)
    matrix_salida[i, j] <- salida_temp
  }
  }
matrix_salida
vector_salida2 <- as.vector(matrix_salida)
vector_salida2
z <- 1
while (z > 0.1){
  z <- runif(1)
  print(z)
}
vector_salida <- vector(mode = "character", 25)
for(i in 1:5){
  for(j in c("a", "b", "c", "d", "e")){
    vector_salida[] <- c(vector_salida, paste(i, j))
  }
}
vector_salida <- c()
for (i in 1:5){
  for (j in c('a', 'b', 'c', 'd', 'e')){
    salida_temporal <- paste(i, j)
    vector_salida <- c(vector_salida, salida_temporal)
  }
}
vector_salida
vector_salida2
vector_salida == vector_salida2
matrix_salida <- matrix(nrow = 5, ncol=5)
vector_j <- c("a", "b", "c", "d", "e")
for(i in 1:5){
  for(j in 1:5){
    valor_j_temp <- vector_j[j]
    salida_temp <- paste(i, valor_j_temp)
    matrix_salida[j, i] <- salida_temp
  }
}
matrix_salida
vector_salida == vector_salida2
vector_salida %in% vector_salida2
matrix_salida <- matrix(nrow = 5, ncol=5)
vector_j <- c("a", "b", "c", "d", "e")
for(i in 1:5){
  for(j in 1:5){
    valor_j_temp <- vector_j[j]
    salida_temp <- paste(i, valor_j_temp)
    matrix_salida[j, i] <- salida_temp
  }
}
#primera forma, cambiando el orden en la matrix, es decir, primero j que i
matrix_salida
vector_salida2 <- as.vector(matrix_salida)
vector_salida2
vector_salida == vector_salida2
#segunda forma, transponiendo la matrix
matrix_salida <- matrix(nrow = 5, ncol=5)
vector_j <- c("a", "b", "c", "d", "e")
for(i in 1:5){
  for(j in 1:5){
    valor_j_temp <- vector_j[j]
    salida_temp <- paste(i, valor_j_temp)
    matrix_salida[i, j] <- salida_temp
  }
}
t(matrix_salida)
vector_salida2 <- as.vector(t(matrix_salida))
vector_salida2
vector_salida == vector_salida2
gapminder
gapminder[gapminder$continent == "Asia", "lifeExp"]
mean(gapminder[gapminder$continent == "Asia", "lifeExp"])
#saqué la media
media <-mean(gapminder[gapminder$continent == "Asia", "lifeExp"])
#lo puse como un vector, así es más fácil de manejar los datos 
if(media < 50){
  print("La media de vida es menor a 50")
  } else if (media > 50){
   print("La media de vida es mayor a 50")
  }
}
continentes <- levels(gapminder$continent)
for(continente in continentes){
  media <- mean(gapminder$lifeExp[gapminder$continent])
}
#cuando hago el bucle debe tener el nombre de salida in el vector (pais in paises)
paises <- levels(gapminder$country)
for(pais in paises){
  media <- mean(gapminder$lifeExp[gapminder$country == pais])
  if(media < 50){
    print(paste("En", pais, "la media de vida es menor que 50"))
  } else if (media > 50 | media <70) {
    print(paste("En", pais, "La media de vida es entre 50 y 70"))
  }else if (media > 70){
    print(paste("En", pais, "La media de vida es mayor que 70"))
  }
}
#después de cada if debe haber un else y si hay varias opciones, else if
#colocar | como separador, por ej: media>50 | media<70, asi me da los valores entre ambos

#clase 6

gapminder <- read.csv("data/gapminder-FiveYearData.csv")
gapminder
library(ggplot2)
ggplot(gapminder, aes(x = gdpPercap, y= lifeExp)) + geom_point()
# aes: le dices como quieres mapear tus datos. Para geom-point no se coloca nada porque toma las opciones de arriba
ggplot(gapminder, aes(x = year, y= lifeExp, color = continent)) + geom_point()
# head (gapminder) me permite ver los encabezados del archivo
# para agregar color u otra caracteristica del gráfico, lo coloco dentro del aes
ggplot(gapminder, aes(x = year, y= lifeExp, by = country, color = continent)) + geom_line()
# by es el mismo argumento que group
ggplot(gapminder, aes(x = year, y= lifeExp, by= country)) + geom_point() + geom_line(aes(color = continent)) 
# cambie el orden para que los puntos queden negros y las líneas de colores, por eso las coloco dentro de un aes
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(alpha = 0.5) + scale_x_log10()
# alpha agrega una transparencia, hay unos puntos más oscuros que otros, pues se superponían
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(alpha = 0.5, color = "red") + scale_x_log10()
#como no hay un vector que se llame red, hay que coocarlo entre comillas  dentro del alpha
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + scale_x_log10() + geom_smooth(method = "lm")

# lm: con esto hacemos una linea de regresión lineal
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + scale_x_log10() + geom_smooth() + geom_smooth(method = "lm", color = "red")
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(alpha = 0.1, color = "green") + geom_smooth() + geom_smooth(method = "lm", color = "red") + coord_trans(x = "log10")
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point(alpha = 0.1, color = "green") + geom_smooth() + geom_smooth(method = "lm", color = "red") + coord_trans(x = "log10")
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point(alpha = 0.1, color = "green") + geom_smooth() + geom_smooth(method = "lm", color = "red")
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(alpha = 0.1,) + geom_smooth() + geom_smooth(method = "lm", color = "red") + coord_trans(x = "log10")
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + scale_x_log10() + geom_smooth(method = "lm", size = 1.5)
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() + scale_x_log10() + geom_smooth(method = "lm", size = 1.5)
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(color = "green", size = 1) + scale_x_log10() + geom_smooth(method = "lm")
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent, shape = continent)) +
  geom_point() + scale_x_log10() + geom_smooth(method = "lm", size = 1.5)

#hace un string desde 1 y se detiene en 1, el vector si empieza con a o con z (az) %in%: solo los que empiezan con estas letras
starts.with <- substr(gapminder$country, start = 1, stop = 1)
az.countries <- gapminder[starts.with %in% c("A", "Z"), ]
ggplot(data = az.countries, 
       aes(x = year, y = lifeExp, color = continent)) +
  geom_line() + facet_wrap( ~ country)
#facet_wrap: envolver facetas en paneles, en este caso, hace un panel por cada país. Siempre lleva este chirimbolo ~, cualquier característica que facetee
#facet_grid (gdpfactor ~ continent), me crea variables del factor
ggplot(data = az.countries, 
       aes(x = year, y = lifeExp, color = continent)) +
  geom_line() + 
  facet_wrap( ~ country) +
  xlab("Year") +
  ylab("Life expectancy") + 
  ggtitle("Figura 1") +
  scale_colour_discrete( name = "Continent") +
  theme(strip.text = element_text(size = 13))
#con theme cambiamos cosas particulares, si le ponemos guion bajo, modifica todo el tema. Strip.text etc, modifica la fuente
ggplot(data = az.countries, 
       aes(x = year, y = lifeExp, color = continent)) +
  geom_line() + 
  facet_wrap( ~ country) +
  xlab("Year") +
  ylab("Life expectancy") + 
  ggtitle("Figura 1") +
  scale_colour_discrete( name = "Continent") +
  theme(axis.text.x=element_text(angle = 90, hjust = 1))

ggplot(data = az.countries, 
       aes(x = year, y = lifeExp, color = continent)) +
  geom_line() + 
  facet_wrap( ~ country) +
  xlab("Year") +
  ylab("Life expectancy") + 
  ggtitle("Figura 1") +
  scale_colour_manual( name = "Continent", values = c(Africa = "red", Americas = "blue", Asia = "yellow", Oceania = "orange", Europe = "purple")) +
  theme(axis.text.x=element_text(angle = 90, hjust = 1))

#gganimate: animación, tweenr: para anidar (si se mueven mas rapido o lento los puntos al principio o al final), dplyr: pipe=tubo

library(gganimate)
library(tweenr)
library(dplyr)

gapminder_2 <- gapminder %>% 
  transform(ease = rep("cubic-in-out", nrow(gapminder)),
            continent = as.numeric(continent))

gapminder_tw <- tween_elements(gapminder_2,
                               time = "year", 
                               group = "country", 
                               ease = "ease",
                               nframes = 120)
gapminder_tw <- transform(gapminder_tw, 
                          continent = factor(continent,
                                             labels = levels(gapminder$continent))
)
year <- gapminder_tw[!duplicated(gapminder_tw$year), c("year", ".frame")] %>% transform(year = round(year))

p <- ggplot(gapminder_tw, 
            aes(gdpPercap, lifeExp, frame = .frame)) +
  geom_point(aes(size = pop,
                 color = continent)) +
  geom_text(data = year, aes(x = mean(gapminder$gdpPercap), 
                             y = max(gapminder$lifeExp) + 1, label = year), size = 12) +
  scale_x_log10() +
  xlab("PBI per capita") +
  scale_y_continuous("Expectativa de vida") +
  scale_color_discrete("Continente") +
  scale_size_continuous("Población") +
  theme_light(20)

gg_animate(p, title_frame = FALSE)

install.packages("gganimate") 
library(tweener)
install.packages("tweenr")
install.packages("dplyr")
rpm -Uvh ImageMagick-7.0.3-0.x86_64.rpm
cd $ HOME
if(!require(gganimate) {
  if(!require(devtools)) install.packages("devtools")  
  devtools::install_github("dgrtwo/gganimate")
}
library(gganimate)
if(!require(tweenr) {
  if(!require(devtools)) install.packages("devtools")  
  devtools::install_github("thomasp85/tweenr")
}
library(tweenr)
library(dplyr)

POP <- (gapminder$pop)
POP 
head(gapminder)
gapminder$pop
POP/1000000
gapminder <- cbind(gapminder, POP)
POP
head(gapminder)

ggplot(data = gapminder, aes(x = year, y = POP, by = country, color = continent)) +
  geom_point()
m <- matrix(1:12, nrow = 3, ncol = 4)
m
m ^ -1
m * c(1, 0, -1)
m > c(0, 20)
m * c(1, 0, -1, 2)

n = 100
n
seq(1:100)
n <- (1:100)
n
n2 <- (1:10000)
n2
x <- (1)
x
sum(x/(x ^ 2))

y= 1:100
y ^ -2
sum(y ^ -2)
z= 1:10000


z ^ -2
sum(z ^-2)

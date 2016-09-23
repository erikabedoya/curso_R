library (plyr)
gapminder<- read.csv("data/gapminder-FiveYearData.csv")

#volvimos a cargar la función de la clase lecciones

calcPBI <- function(dat, year=NULL, country=NULL) {
  if(!is.null(year)) {
    dat <- dat[dat$year %in% year, ]
  }
  if (!is.null(country)) {
    dat <- dat[dat$country %in% country,]
  }
  
  gdp <- dat$pop * dat$gdpPercap
  new <- cbind(dat, gdp = gdp)
  return(new)
}

conPBI <- calcPBI(gapminder)
ddply(.data = conPBI,
      .variables = "continent",
      .fun = function(dat) mean(dat$gdp))
#esta función es anónim, es solo para utilizarla acá, no para guardarla. Mean trabaja sobre el gdp

dlply(.data = conPBI,
      .variables = "continent",
      .fun = head)
#dlply me la saca como lista, mean actua sobre los 5 dataframes de continente
#solo pueden ser factores numéricos. con el head me saca los datos de los encabezados

daply(.data = conPBI,
      .variables = "continent",
      .fun = function(dat) mean(dat$gdp))
#daply: array, es una matrix

daply(.data = conPBI,
      .variables = c("continent", "year"),
      .fun = function(dat) mean(dat$gdp))
#dividimos por más variables y nos saca una matrix, hace la media por paises por año, los agrupa

d_ply(
  .data = gapminder,
  .variables = "continent",
  .fun = function(x) {
    meanGDPperCap <- mean(x$gdpPercap)
    print(paste(
      "The mean GDP per capita for", unique(x$continent),
      "is", format(meanGDPperCap, big.mark = ",")
    ))
  }
)
# d_ply: no quiero que me devuelva datos particulares del dataframe, solo imprime lo que le pedí con el paste

format(1.003, decimal.mark = ",")
#cambia el punto por coma

concoma<-function(x){
  format(x, decimal.mark = ",")
}
# cree la función concoma para que sea directo
concoma(1.003)

#...: elipsis, es un atajo para cambiar todo lo que necesito sin utilizar todas las funciones que me ofrece, 
# es un argumento especial que me agrupa todos los que no he nombrado
concoma<-function(x, ...){
  format(x, decimal.mark = ",", ...)
}

d_ply(
  .data = gapminder,
  .variables = "continent",
  .fun = function(x) {
    meanGDPperCap <- mean(x$gdpPercap)
    print(paste(
      "The mean GDP per capita for", unique(x$continent),
      "is", concoma(meanGDPperCap, big.mark = ".")
    ))
  }
)

daply(
  .data = gapminder,
  .variables = "continent",
  .fun = function(x) {
    meanlifeExp <- mean(x$lifeExp)
    print(paste(
      "The mean lifeExp for", unique(x$continent),
      "is", concoma(meanlifeExp, big.mark = ".")
    ))
  }
)

ddply(
  .data = gapminder,
      .variables = "continent",
      .fun = function(x) {
        meanlifeExp <- mean(x$lifeExp)
      }
)

ddply(gapminder, . (continent), function(x) mean(x$lifeExp))
lifeExp_continent <- ddply(gapminder,.(continent), function(x) mean(x$lifeExp))
lifeExp_continent[c(which.max(lifeExp_continent$V1), which.min(lifeExp_continent$V1)),]

lifeExp_continent_year <- ddply(gapminder, .(continent, year), function(x) mean(x$lifeExp))
lifeExp_continent_2007 <-
lifeExp_continent_year[lifeExp_continent_year$year == 2007,]
lifeExp_continent_2007
lifeExp_continent_2007[c(which.max(lifeExp_continent$V1), which.min(lifeExp_continent_2007$V1)),]
lifeExp_continent_year[lifeExp_continent_year$year %in% c(2007, 1952) , ]
lifeExp_continent_1952 <-
lifeExp_continent_year[lifeExp_continent_year$year == 1952,]  
lifeExp_continent_2007
lifeExp_continent_1952_2007 <- cbind(lifeExp_continent_1952, anio_2007 = lifeExp_continent_2007$V1)
lifeExp_continent_1952_2007
names(lifeExp_continent_1952_2007)[3] <- "anio_1952"
lifeExp_continent_1952_2007$diferencia <- with (lifeExp_continent_1952_2007, anio_2007 - anio_1952)
lifeExp_continent_1952_2007

ddply(lifeExp_continent_year, .(continent), 
      function(x) x[x$year == 2007, "V1"]-
        x[x$year == 1952, "V1"])

ddply(gapminder, .(continent), summarise,
      media_lifeExp = mean(lifeExp),
      sd_lifeExp = sd(lifeExp))
# sd: desvío standard

library(dplyr)

year_country_gdp <- select(gapminder,year,country,gdpPercap)
year_country_gdp
year_country_gdp <- gapminder %>% select(year,country,gdpPercap)
year_country_gdp

# los pipes son %% rodeando un signo >, evita hacer el anidamiento de funciones manual. 

year_country_gdp_euro <- gapminder %>%
  filter(continent == "Europe") %>%
  select(year, country, gdpPercap)

year_country_gdp_euro

year_country_gdp_afri <- gapminder %>%
  filter(continent == "Africa") %>%
  select(lifeExp, country, year)

year_country_gdp_afri

year_country_gdp_euro <- gapminder %>%
  filter(continent == "Europe") %>%
  select(year, country, gdpPercap)
# importa mucho el orden en que coloquemos los pipes, primero hay que filtrar por continente y luego que te muestre las selecciones

str(gapminder %>% group_by(continent, year))

gdp_bycontinents <- gapminder %>%
  group_by(continent) %>%
  summarise(mean_gdpPercap = mean(gdpPercap))
gdp_bycontinents

#atajo para escribir el pipe: ctrl+shift+m
lifeExp_country <- gapminder%>% 
  group_by(country) %>% 
  summarise(mean_lifeExp = mean(lifeExp))

lifeExp_country

print(lifeExp_country, )

lifeExp_country[which.max(lifeExp_country$mean_lifeExp), ]
lifeExp_country %>% filter(mean_lifeExp == max(mean_lifeExp))
lifeExp_country %>% filter(mean_lifeExp %in% range(mean_lifeExp))
#para colocar el máximo y el mínimo de una vez, coloco %in% en vez de == y range

#mutate:
gdp_pop_bycontinents_byyear <- gapminder %>%
  mutate(gdp_billion = gdpPercap*pop/10^9) %>%
  group_by(continent,year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap),
            sd_gdpPercap = sd(gdpPercap),
            mean_pop = mean(pop),
            sd_pop = sd(pop),
            mean_gdp_billion = mean(gdp_billion),
            sd_gdp_billion = sd(gdp_billion))
gdp_pop_bycontinents_byyear

# ejercicio avanzado
gapminder %>%
  filter(year == 2002) %>%
  group_by(continent) %>% 
  sample_n(2) %>% 
  summarise(mean_lifeExp=mean(lifeExp)) %>% 
  arrange(desc(mean_lifeExp))

# Paquete tidyr (penúltima clase)

install.packages("tidyr")
install.packages("dplyr")
library("tidyr")
library("dplyr")



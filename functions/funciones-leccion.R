# function (argumentos){}

dir.create("functions")
mi_suma <- function(a,b) {
  suma <- a + b 
  return(suma)
}
mi_suma(5,4)

kelvin_a_fahr <- function(temp) {
  fahr <- (temp - 273.15) * (9/5) + 32
  return(fahr)
}
kelvin_a_fahr(273.15)
kelvin_a_fahr(373.15)
# T° a la que hierve el agua, 212 grados

celsius_a_kelvin <- function(temp) {
  kelvin <- (temp + 273.15)
  kelvin
}
celsius_a_kelvin(25)

celsius_a_fahr <- function(temp) {
  fahr <- (temp - 273.15) * (9/5) + 32
  return(fahr)
}
celsius_a_fahr(25)

celsius_a_fahr <- function(temp) {
  kelvin <- celsius_a_kelvin(temp)
  fahr <- kelvin_a_fahr(temp)
  return(fahr)
}
celsius_a_fahr(32)

gapminder<- read.csv("data/gapminder-FiveYearData.csv")
gapminder
calcPBI <- function(dat) {
  pbi <- dat$pop * dat$gdpPercap
  pbi
}
calcPBI(head(gapminder))

# el signo de admiración niega la condición, si el argumento es falso lo vuelve verdadero
# con %in% puedo elegir más datos que con ==
# las comas son para que me seleccionen las filas, no las columnas

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

calcPBI(head(gapminder))
calcPBI(gapminder, year = 2007)
calcPBI(gapminder, year = 2002, country = "Argentina")

calcPBI(gapminder, year = c(1987, 1952), country = "New Zealand")

# collapse colapsa el vector en un solo, nos saca todo en una sola línea
mejores_practicas <- c("Escribe", "programas", "para", "personas", 
                       "no", "para", "computadoras")
paste(mejores_practicas, collapse = " _")

vallar(texto = mejores_practicas, envoltura = "***")

paste(mejores_practicas) 
paste0("***", paste(mejores_practicas, collapse = " "), "***")

vallar <- function (texto, envoltura) {
  paste0(envoltura, paste(texto, collapse = " "), envoltura )
} 

vallar(texto = mejores_practicas, envoltura = "***")  


pdf("Life_Exp_vs_time.pdf", width=12, height=4)
ggplot(data=gapminder, aes(x=year, y=lifeExp, colour=country)) +
  geom_line()

# ¡Acordarse de apagar el dispositivo!

dev.off()

pdf("Life_Exp_vs_time.pdf", width=12, height=12, onefile = TRUE)
ggplot(data=gapminder, aes(x=year, y=lifeExp, colour=country)) +
  geom_line() +
  theme(legend.position="bottom") + 
  facet_grid(continent ~ .)

dev.off()

library (ggplot2)
pdf (" Mi_grafico.pdf ", width = 12, height = 8, onefile = TRUE) 
for(continent in levels(gapminder$continent)) { 
  print (ggplot(data = gapminder[gapminder$continent == continent, ], 
                aes (x = year, y = lifeExp, color = country)) + 
           geom_line ())
  }
dev.off () 

pdf ("Life_Exp_vs_time.pdf", width = 12, height = 12, onefile = TRUE) 
ggplot(data = gapminder, aes (x = year, y = lifeExp, color = country)) + 
  geom_line () + facet_grid(continent ~ .) + 
  theme (legend.position = "bottom") 
  
dev.off () 


aust_subset <- gapminder[gapminder$country == "Australia",]
dir.create("cleaned data")
write.table(aust_subset,
            file="cleaned data/gapminder-aus.csv",
            sep=","
)

aust_subset <- gapminder[gapminder$country == "Australia",]

write.table(aust_subset,
            file = "cleaned data/gapminder-aus.csv",
            sep = ",", quote = FALSE, row.names = FALSE
)

subset_1990 <- gapminder[gapminder$year > 1990, ]
write.table(subset_1990,
            file = "cleaned data/gapminder-aus.csv",
            sep = ","
)


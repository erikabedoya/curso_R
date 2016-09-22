x <- c(5.4, 6.2, 7.1, 4.8, 7.5)
names(x) <- c('a', 'b', 'c', 'd', 'e')
x

x[1]
x[c(1,3)]
x[1:4]
x[-1]
x <- c(5.4, 6.2, 7.1, 4.8, 7.5)
names(x) <- c('a', 'b', 'c', 'd', 'e')
print(x)
x[b,c,d]
x[c(-1,-5)]
x[-1,-5]
x[-c(1,5)]
x[c(2,3,4)]
x[c(-1,-5)]
x[2:4]
x
x["a"]
x[c("a", "b")]
names(x) <- c("a", "b", "c", "d", "a")
x["a"]
names(x) == "a"
which(names(x) == "a")
x[-which(names(x) == "a")]
x[-which(names(x) %in% c("a", "c"))]
x <- c(5.4, 6.2, 7.1, 4.8, 7.5)
names(x) <- c('a', 'b', 'c', 'd', 'e')
print(x)
x[-which(names(x) == "g")]
names(x) == "g"
names(x) <- rep("a", 5)
x
x["a"]
?"%in%"
help("%in%")
?" <- "names(x) == c("a", "c")
?" <- "
x[c(TRUE, TRUE, FALSE, FALSE)]
x <- x[-4]
names(x) ==c("a", "c")
&
|
!TRUE
!FALSE
all(x>4)
any(x>4)
x <- c(5.4, 6.2, 7.1, 4.8, 7.5)
names(x) <- c('a', 'b', 'c', 'd', 'e')
print(x)
x > 4 & x < 7
x[x > 4 & x < 7] 
is.na(x)
na.omit(x)
x <- c(x,NA)
x
is.na(x)
x[!is.na(x)]
na.omit(x)
c(x,4)
f <- factor(c("a", "a", "b", "c", "c", "d"))
f 
f[f == "a"]
f[f%in% c("b","c")]
f[1:3]
f
factor(f[-3])
set.seed(1)
m <- matrix(rnorm( 6 * 4), ncol = 4, nrow = 6)
m[3:4, c(3, 1)]
m[]
m[5,2]
m[, c(3,4)]
m[3,]
m[3, , drop = FALSE]
m[5]
m <- matrix(1:18, nrow = 3, ncol = 6)
print(m)
m[2,c(4,5)]
xlist <- list(a = "UNTDF", b = 1:10, data = head(iris))
xlist[1]
xlist[1]
class(xlist[1])
xlist[2]
xlist[3]
typeof(xlist[1])
xlist[1:2]
xlist[c("a", "data")]
typeof(xlist[[1]])
xlist$a
xlist$b
xlist$da
xlist[["data"]]["species"]
xlist[["data"]]["Petal.Length"]
xlist[["data"]]["Petal.Length"] == 1,5
xlist <- list(a = "UNTDF", b = 1:10, data = head(iris))
xlist[1]
xlist[[2]]
xlist[[2]][2]
mod <- aov(pop ~ lifeExp, data=gapminder)
str(mod)
attributes(mod)
head(gapminder)
mod <- aov(población ~ expect, data=gapminder)
mod
mod
str(mod)
atrributes(mod)
attributes(mod)
names(mod)
head(gapminder[3],5)
head(gapminder[["Length.petal"]
gapminder[gapminder$país=="Argentina",]
gapminder
mod <- aov(población ~ expect, data=gapminder)
gapminder[gapminder$año == 1957,]
gapminder[,-1:4]
gapminder[,-1:4]
gapminder(1:4)]
gapminder[,-c(1:4)]

gapminder-FiveYearData <- read.csv("data/gapminder-FiveYearData.csv")
gapminder-FiveYearData

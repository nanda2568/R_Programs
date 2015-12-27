library(datasets)
data(iris)
?iris
s <- split(iris,iris$Species)
str(s)
#what is the mean of 'Sepal.Length' for the species virginica?
lapply(s, function(x) {
    colMeans(x[, c("Sepal.Length", "Sepal.Width")])
})

#what R code returns a vector of the means of the variables 'Sepal.Length', 'Sepal.Width', 'Petal.Length', and 'Petal.Width'?
apply(iris[, 1:4], 2, mean)



library(datasets)
data(mtcars)
?mtcars

#How can one calculate the average miles per gallon (mpg) by number of cylinders in the car (cyl)?
sapply(split(mtcars$mpg, mtcars$cyl), mean)

#what is the absolute difference between the average horsepower of 4-cylinder cars and the average horsepower of 8-cylinder cars?
hpAvg <- sapply(split(mtcars$hp, mtcars$cyl), mean)
hpAvg[3] - hpAvg[1]
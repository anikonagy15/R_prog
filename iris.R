library(datasets)
data(iris)
str(split)
S<-split(iris, iris$Species)
s
k<-sapply(s, function(x) colMeans(x[,c("virginica")]))
print(k)

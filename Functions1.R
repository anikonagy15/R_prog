above10<- function(x){
  use<- x>10
}
above <- function(x,n) {
    use<-x>n
    x[use]
}
x<-1:10
if(x>5) {
  x<-0
}
x<-1:10
if(x>5) {
  pollutantmean <- function(x, y, id = 1:332,removeNA=TRUE){
             nr<- ncol(x)
               for(i in 1:nr){
                 means[i]<-mean(y[ ,i],na.rm=removeNA)
                 }
                means     
            }
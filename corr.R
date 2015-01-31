corr <- function(directory, threshold = 0) {
        x<-list.files(directory)
        f<-numeric()
        for(i in 1:length(x)) {
                k<-paste(directory, x[i], sep="/")
                y<-read.csv(k)
                s<-length(which(complete.cases(y)))
                if(s>threshold){
                        a<-read.csv(k)$nitrate
                        b<-read.csv(k)$sulfate
                        c<-round(cor(a, b, use="complete"), 6)
                        f<-c(f,c)              
                }

        }

        return(f)
}

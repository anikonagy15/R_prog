pollutantmean<-function(directory, pollutant, id = 1:332) {
        x<-list.files(directory)
        f<-NA
        for(i in 1:length(x)) {
                k<-paste(directory, x[i], sep="/")
                y<-read.csv(k)
                d<-y$ID
                if (d[1] %in% id) {
                        f<-rbind(f, y) 
                }
        }

        round(mean(f[,pollutant], na.rm=TRUE), 3)
}

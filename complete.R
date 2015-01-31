complete <- function(directory, id = 1:332) {
        x<-list.files(directory)
        nobs<-NA
        for (j in 1:length(id)) {
                for(i in 1:length(x)) {
                        k<-paste(directory, x[i], sep="/")
                        y<-read.csv(k)
                        d<-y$ID
                        if (d[1] == id[j]) {
                                s<-length(which(complete.cases(read.csv(k))))
                                nobs<-c(nobs, s)
                                print(s)
                        }
                }
        }

        nobs<-nobs[!is.na(nobs)]
        df = data.frame(cbind(id, nobs))
        print(df)
}
        
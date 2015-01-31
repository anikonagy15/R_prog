rankhospital <- function(state, outcome, num = "best") {
        r <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        col11 <- "heart attack"
        col17 <- "heart failure"
        col23 <- "pneumonia"
        x<- r[, c(2, 7, 11, 17, 23)]
        if ((state  %in% x$State) == FALSE) {
                stop("invalid state")
        }
        else if (outcome=="heart attack") {
                h <- subset(r, State==state)
                w <- h[order(h$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"), ]
                Rate <- w[, 11]
                Rate<-as.numeric(Rate)
                Hospital.Name <- w[, 2]
                q <- data.frame(Hospital.Name, Rate)
                q<-q[complete.cases(q),]
                i<-sapply(q, is.factor)
                q[i]<-lapply(q[i], as.character)
                d <- q[order(q[, 2], q[, 1]), ]
                if(num=="best"){
                        num <- 1
                }
                else if (num =="worst"){
                        num <- nrow(d)
                }
                else if (num >nrow(d)){
                        return(NA)
                }
                print(d[num,1])
                
        }
        else if (outcome=="heart failure") {
                h <- subset(r, State==state)
                w <- h[order(h$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"), ]
                Rate <- w[, 17]
                Rate<-as.numeric(Rate)
                Hospital.Name <- w[, 2]
                q <- data.frame(Hospital.Name, Rate)
                q<-q[complete.cases(q),]
                i<-sapply(q, is.factor)
                q[i]<-lapply(q[i], as.character)
                d <- q[order(q[, 2], q[, 1]), ]
                if(num=="best"){
                        num <- 1
                }
                else if (num =="worst"){
                        num <- nrow(d)
                }
                else if (num >nrow(d)){
                        return(NA)
                }
                print(d[num,1])
                
        }
        else if (outcome=="pneumonia") {
                h <- subset(r, State==state)
                w <- h[order(h$"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"), ]
                Rate <- w[, 23]
                Rate<-as.numeric(Rate)
                Hospital.Name <- w[, 2]
                q <- data.frame(Hospital.Name, Rate)
                q<-q[complete.cases(q),]
                i<-sapply(q, is.factor)
                q[i]<-lapply(q[i], as.character)
                d <- q[order(q[, 2], q[, 1]), ]
                if(num=="best"){
                        num <- 1
                }
                else if (num =="worst"){
                        num <- nrow(d)
                }
                else if (num >nrow(d)){
                        return(NA)
                }
                print(d[num,1])
                
        }
        else {
                stop("invalid outcome")
                
        }
}
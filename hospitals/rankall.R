rankall <- function(outcome, num = "best") {
        r <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#        x<- r[, c(2, 7, 11, 17, 23)]
        if (num == "best") {
                num <- 1
        }

        if (outcome=="heart attack") {
                if (num == "worst") {
                        w <- r[order(as.numeric(r$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"), r$"Hospital.Name", decreasing = TRUE), ]
                }
                else {
                        w <- r[order(as.numeric(r$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"), r$"Hospital.Name"), ]
                }
        }
        else if (outcome=="heart failure") {
                if (num == "worst") {
                        w <- r[order(as.numeric(r$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"), r$"Hospital.Name", decreasing = TRUE), ]
                }
                else {
                        w <- r[order(as.numeric(r$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"), r$"Hospital.Name"), ]
                }
        }
        else if (outcome=="pneumonia") {
                if (num == "worst") {
                        w <- r[order(as.numeric(r$"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"), r$"Hospital.Name", decreasing = TRUE), ]
                }
                else {
                        w <- r[order(as.numeric(r$"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"), r$"Hospital.Name"), ]
                }
        }
        else {
                stop("invalid outcome")
        }

        w <- w[complete.cases(w),]
        k <- split(w, w$State)
        df <- numeric()
        for (i in 1:length(k)) {
                if (num == "worst") {
                        hospital <- k[[i]][1,2]
                }
                else {
                        hospital <- k[[i]][num,2]
                }

                state <- k[[i]][1,7]
                df<-rbind(df, data.frame(hospital, state))
        }

        print(df)                
}

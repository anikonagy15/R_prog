best <- function(state, outcome) {
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
                m <- min(as.numeric(h[, 11]), na.rm=TRUE)
                z <- format(round(m, 2), nsmall = 1)
                s <- h[which(h[, 11] == z), ]
                p <- with(s, s[order(s[, 2]),])
                p[1,2]
        }
        else if (outcome=="heart failure") {
                h <- subset(r, State==state)
                m <- min(as.numeric(h[, 17]), na.rm=TRUE)
                z <- format(round(m, 2), nsmall = 1)
                s <- h[which(h[, 17] == z), ]
                p <- with(s, s[order(s[, 2]),])
                p[1,2]
        }
        else if (outcome=="pneumonia") {
                h <- subset(r, State==state)
                m <- min(as.numeric(h[, 23]), na.rm=TRUE)
                z <- format(round(m, 2), nsmall = 1)
                s <- h[which(h[, 23] == z), ]
                p <- with(s, s[order(s[, 2]),])
                p[1,2]
        }
        else {
                stop("invalid outcome")
                
        }
        
        
        
}
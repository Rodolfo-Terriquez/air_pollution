#Question 1

pollutantmean <- function(directory, pollutant, id = 1:332){
        all_files <- list.files(directory, full.names = TRUE)
        x <- data.frame()
        for(i in id) {
                data <- read.csv(all_files[i])
                good <- !is.na(data[,pollutant])
                y <- (data[good,])
                x <- rbind(x, y)
        }
        return(mean(x[, pollutant], na.rm = TRUE))
}

#Question 2

complete <- function(directory, id = 1:332){
        all_files <- list.files(directory, full.names = TRUE)
        x <- data.frame()
        y <- 1
        for(i in id) {
                data <- read.csv(all_files[i])
                comp <- complete.cases(data)
                s <- sum(comp)
                x[y, 1] <- i
                x[y, 2] <- s
                y <- y + 1
        }
        colnames(x) <- c("id", "nobs")
        print(x)
}

#Question 3

corr <- function(directory, threshold = 0){
        all_files <- list.files(directory, full.names = TRUE)
        x <- vector()
        id <- 1:332
        for(i in id) {
                data <- read.csv(all_files[i])
                comp <- complete.cases(data)
                t1 <- data[comp,]
                if(nrow(t1) < threshold){
                        
                } else {
                        a <- cor(t1[,2], t1[,3])
                        x <- append(x, a)
                }
        }
        print(x)
}
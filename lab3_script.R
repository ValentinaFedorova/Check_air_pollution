library(dplyr)
getMean <- function(folder, type, id=1:332){
  data_files <- list.files(folder, full.names=TRUE)
  dat <- data.frame()
  for (i in id) {                                
    dat <- rbind(dat, read.csv(data_files[i]))
  }
  mean(dat[, type], na.rm=TRUE)
} 

#getMean("data", "nitrate", 23)

getCompleteObservation <- function(folder, id = 1:332) {
  data_files <- list.files(folder, full.names=TRUE)
  dat <- data.frame()
  for (i in id) {                                
    dat <- rbind(dat, read.csv(data_files[i]))
  }
  not_null_data <- dat[complete.cases(dat),]
  not_null_data %>%
    group_by(ID) %>%
    summarise(count = length(Date)
  )
}

#getCompleteObservation("data", c(2, 4, 8, 10, 12))

getCorrelation <- function(folder, limen = 0) {
  data_files <- list.files(folder, full.names=TRUE)
  dat <- data.frame()
  for (i in 1:(length(data_files))) {                                
    dat <- rbind(dat, read.csv(data_files[i]))
  }
  limen_data <- data.frame()
  for (i in 1:(length(data_files))) {                                
    dat_by_monitor <- dat[which(dat[, "ID"] == i),]
    needed_data = dat_by_monitor[c("sulfate","nitrate")]
    full_data <- needed_data[complete.cases(needed_data),]
    if (nrow(full_data) > limen) {
      limen_data <- rbind(limen_data, cor(full_data["sulfate"],full_data["nitrate"]))
    }
  }
  limen_data
}

res <- getCorrelation("data", 400)
head(res)
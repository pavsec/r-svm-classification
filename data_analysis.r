rm(list=ls())

setwd("C:/Users/mbano/Documents/Faks/Usluge zasnovane na lokaciji/r-svm-classification")

car <- read.csv("datasets/car_clean1.csv", header=TRUE)
walk <- read.csv("datasets/walk_clean1.csv", header=TRUE)

car <- subset(car, x < 2000)
car <- subset(car, z > -200)

par(mar=c(6,2,6,2))
par(mfrow=c(1,3))

rows <- max(nrow(car), nrow(walk))
car[rows+1,] <- NA
walk[rows+1,] <- NA
accel_x <- data.frame(car=car[, 'x'], walk=walk[,'x'])
accel_y <- data.frame(car=car[, 'y'], walk=walk[,'y'])
accel_z <- data.frame(car=car[, 'z'], walk=walk[,'z'])
boxplot(accel_x, horizontal=TRUE, main="Accel.x")
boxplot(accel_y, horizontal=TRUE, main="Accel.y")
boxplot(accel_z, horizontal=TRUE, main="Accel.z")

plot(density(na.omit(append(accel_x$car, accel_x$walk))), main="Accel.x")
plot(density(na.omit(append(accel_y$car, accel_y$walk))), main="Accel.y")
plot(density(na.omit(append(accel_z$car, accel_z$walk))), main="Accel.z")

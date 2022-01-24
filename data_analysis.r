rm(list=ls())

setwd("C:/Users/mbano/Documents/Faks/Usluge zasnovane na lokaciji/r-svm-classification")

library(DataExplorer)
library(ggplot2)

car <- read.csv("datasets/clean datasets/car.csv", header=TRUE)
car2 <- read.csv("datasets/clean datasets/car2.csv", header=TRUE)
car3 <- read.csv("datasets/clean datasets/car3.csv", header=TRUE)
walk <- read.csv("datasets/clean datasets/walk.csv", header=TRUE)
walk2 <- read.csv("datasets/clean datasets/walk2.csv", header=TRUE)
bus <- read.csv("datasets/clean datasets/bus.csv", header=TRUE)
bus2 <- read.csv("datasets/clean datasets/bus2.csv", header=TRUE)

car <- rbind(car, car2)
car <- rbind(car, car3)

car = subset(car, x < 2000)
car = subset(car, z > -200)
car = subset(car, y > -1000)

walk <- rbind(walk, walk2)

walk = subset(walk, x < 2000)
walk = subset(walk, z > -200)
walk = subset(walk, y > -1000)

bus <- rbind(bus, bus2)

bus = subset(bus, x < 2000)
bus = subset(bus, z > -200)
bus = subset(bus, y > -1000)

total <- rbind(car, walk)
total <- rbind(total, bus)
total <- na.omit(total)
total <- total[, c("x","y", "z", "class")]

par(mar=c(6,2,5,2))
par(mfrow=c(1,3))

rows <- max(nrow(car), nrow(walk), nrow(bus))
car[rows+1,] <- NA
walk[rows+1,] <- NA
bus[rows+1,] <- NA
accel_x <- data.frame(car=car[, 'x'], walk=walk[,'x'], bus=bus[,'x'])
accel_y <- data.frame(car=car[, 'y'], walk=walk[,'y'], bus=bus[,'y'])
accel_z <- data.frame(car=car[, 'z'], walk=walk[,'z'], bus=bus[,'z'])
boxplot(accel_x, horizontal=TRUE, main="Accel.x")
boxplot(accel_y, horizontal=TRUE, main="Accel.y")
boxplot(accel_z, horizontal=TRUE, main="Accel.z")
ggplot(total, aes(x=class, y=x, group=class)) + geom_boxplot() + coord_flip()+ scale_y_continuous(trans = scales::pseudo_log_trans())
ggplot(total, aes(x=class, y=y, group=class)) + geom_boxplot() + coord_flip()+ scale_y_continuous(trans = scales::pseudo_log_trans())
ggplot(total, aes(x=class, y=z, group=class)) + geom_boxplot() + coord_flip()+ scale_y_continuous(trans = scales::pseudo_log_trans())

x <- append(append(accel_x$car, accel_x$walk), accel_x$bus)
plot(density(na.omit(x)), main="Accel.x")
y <- append(append(accel_y$car, accel_y$walk), accel_y$bus)
plot(density(na.omit(y)), main="Accel.y")
z <- append(append(accel_z$car, accel_z$walk), accel_z$bus)
plot(density(na.omit(z)), main="Accel.z")

par(mfrow=c(1,1))
# total <- total[, c("x","y", "z")]
plot_correlation(total)

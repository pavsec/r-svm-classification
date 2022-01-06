rm(list=ls())
require(ggplot2)
require(BBmisc)

setwd('D:/fax/Collage/9/uznl/projekt/r-svm-classification')
data = read.csv(paste('./datasets/walk_clean1.csv', sep = ''), head = FALSE, sep = ',', colClasses = 'character', skip = 1)
x = data[c('V2', 'V5')]
x$V2 = as.numeric(x$V2)
x$V5 = as.POSIXct(x$V5)

x$V2 = normalize(x$V2, method = "range", range = c(0, 1), margin = 1L, on.constant = "quiet")
print(head(x, 5))
ggplot(x, aes(V5, V2, group = 1)) +
         geom_point() +
         geom_line() +
         scale_x_datetime() + xlab('Time') + ylab('x acceleration')



car = read.csv(paste('./datasets/car_clean1.csv', sep = ''), head = FALSE, sep = ',', colClasses = 'character', skip = 1)
x = car[c('V2', 'V5')]
x$V2 = as.numeric(x$V2)
x$V5 = as.POSIXct(x$V5)

x$V2 = normalize(x$V2, method = "range", range = c(0, 1), margin = 1L, on.constant = "quiet")
print(head(x, 5))
ggplot(x, aes(V5, V2, group = 1)) +
         geom_point() +
         geom_line() +
         scale_x_datetime() + xlab('Time') + ylab('x acceleration')
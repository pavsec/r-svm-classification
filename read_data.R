rm(list=ls())
require(ggplot2)
require(hash)

# setwd('D:/fax/Collage/9/uznl/projekt/r-svm-classification')
# set class
dataset = 'walk'
class = hash()
class[['car']] = 0
class[['bus']] = 1
class[['walk']] = 2
print(class[[dataset]])
cl = class[[dataset]]

# read data
data = read.csv(paste('./datasets/', dataset, '.csv', sep = ''), head = FALSE, sep = ';', colClasses = 'character', skip = 2)

# clean data - each new dataset will have data in span of 15min
print(head(data, 5))
data = data[c('V7', 'V8', 'V9', rev(names(data))[1])]
print(head(data, 5))

data$timestamp = rev(data)[1]
colnames(data) = c('x', 'y', 'z', 'timestamp')
print(head(data, 5))
data$timestamp = as.POSIXct(data$timestamp)

# data1 = data[data$timestamp <= (data$timestamp[1]) + minutes(15),]
data1 = data[data$timestamp > (data$timestamp[1]) + minutes(15),]
print(head(data1, 5))


# ne mogu zapisat milisekunde pa sam ovako rijesila problem
# for each second, take mean of recorded values and set that as acceleration in new, clean dataset
tmp_t = data1$timestamp[1]
tmp_x = 0
tmp_y = 0
tmp_z = 0
df = data.frame(matrix(ncol = 5, nrow = 0))
i = 1

for (r in 1:nrow(data1))
{
  if (tmp_t != data1[r, 'timestamp'])
  {
    df = rbind(df, c(as.numeric(mean(tmp_x)),
                     as.numeric(mean(tmp_y)),
                     as.numeric(mean(tmp_z)),
                     as.character(tmp_t),
                     cl ))
    tmp_t = data1[r, 'timestamp']
    tmp_x = 0
    tmp_y = 0
    tmp_z = 0
  }

  tmp_x = tmp_x + as.numeric(data1[r, 'x'])
  tmp_y = tmp_y + as.numeric(data1[r, 'y'])
  tmp_z = tmp_z + as.numeric(data1[r, 'z'])
}

print(head(df, 5))

colnames(df) = c('x', 'y', 'z', 't', 'class')
df$t = as.POSIXct(df$t)
df$x = as.numeric(df$x)
df$y = as.numeric(df$y)
df$z = as.numeric(df$z)
df$class = as.numeric(df$class)

print(head(df, 5))


# plot all axes
ggplot(df) + geom_line(aes(t, x, color='x')) +
             geom_line(aes(t, y, color='y')) +
             geom_line(aes(t, z, color='z')) +
             scale_x_datetime() + xlab('Time') + ylab('acceleration')

ggsave(paste('./images/', dataset, '2.png', sep = ''))

# save file
write.csv(df ,paste('./datasets/', dataset, '_clean2.csv', sep = ''), row.names = TRUE)

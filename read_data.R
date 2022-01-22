rm(list=ls())
require(ggplot2)
require(hash)
library(lubridate)

setwd('D:/fax/Collage/9/uznl/projekt/r-svm-classification')
# set class
dataset = 'car'
class = hash()
class[['car']] = 0
class[['bus']] = 1
class[['walk']] = 2
class_list = c('car', 'bus', 'walk')
print(class[[dataset]])
cl = class[[dataset]]

filenames <- list.files('./datasets/', pattern="*.csv")

for (f in filenames)
{
  f_clean = gsub('[[:digit:]]+', '', f)
  f_clean = substr(f_clean,1,nchar(f_clean)-4)
  print(f_clean)
  if (!is.null(class[[f_clean]]))
  {
    cl = class[[f_clean]]
    L <- readLines(paste('./datasets/', f, sep = ''), n = 1)
    numfields <- count.fields(textConnection(L), sep = ";")
    if (numfields == 1)
    {
      data = read.csv(paste('./datasets/', f, sep = ''), head = FALSE, sep = ',', colClasses = 'character', skip = 2)
    }
    else
    {
      data = read.csv2(paste('./datasets/', f, sep = ''), head = FALSE, sep = ';', colClasses = 'character', skip = 2)
    }
    # data = read.csv(paste('./datasets/', f, sep = ''), head = FALSE, sep = ',', colClasses = 'character', skip = 2)
    # read data
    # clean data - each new dataset will have data in span of 15min
    print(head(data, 2))
    data = data[c('V7', 'V8', 'V9', rev(names(data))[1])]
    # print(head(data, 5))

    data$timestamp = rev(data)[1]
    colnames(data) = c('x', 'y', 'z', 'timestamp')
    print(head(data, 5))
    data$timestamp = as.POSIXct(data$timestamp)

    # data1 = data[data$timestamp <= (data$timestamp[1]) + minutes(15),]
    # data1 = data[data$timestamp > (data$timestamp[1]) + minutes(15),]
    # print(head(data1, 5))


    # ne mogu zapisat milisekunde pa sam ovako rijesila problem
    # for each second, take mean of recorded values and set that as acceleration in new, clean dataset
    tmp_t = data$timestamp[1]
    tmp_x = 0
    tmp_y = 0
    tmp_z = 0
    df = data.frame(matrix(ncol = 5, nrow = 0))
    i = 1

    for (r in 1:nrow(data))
    {
      if (tmp_t != data[r, 'timestamp'])
      {
        df = rbind(df, c(as.numeric(mean(tmp_x)),
                         as.numeric(mean(tmp_y)),
                         as.numeric(mean(tmp_z)),
                         as.character(tmp_t),
                         cl ))
        tmp_t = data[r, 'timestamp']
        tmp_x = 0
        tmp_y = 0
        tmp_z = 0
      }

      tmp_x = tmp_x + as.numeric(data[r, 'x'])
      tmp_y = tmp_y + as.numeric(data[r, 'y'])
      tmp_z = tmp_z + as.numeric(data[r, 'z'])
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

    # ggsave(paste('./images/', f_clean, '.png', sep = ''))

    # save file
    # write.csv(df ,paste('./datasets/', dataset, '_clean1.csv', sep = ''), row.names = FALSE)

    file_output_name = './datasets/clean datasets/svm_data2.csv'
    if (file.exists(file_output_name) == TRUE)
    {
      write.table(df, file_output_name, row.names = FALSE, append = TRUE, col.names = FALSE, sep = ',')
    }
    else
    {
      write.csv(df, file_output_name, row.names = FALSE)
    }

  }

  # print(head(data, 1))
}


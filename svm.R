require(e1071)
library(caTools)
require(ggplot2)
library(caret)


setwd('D:/fax/Collage/9/uznl/projekt/r-svm-classification')
data = read.csv(paste('./datasets/clean datasets/svm_data.csv', sep = ''), head = TRUE, sep = ',', colClasses = 'character', skip = 0)

print(head(data, 5))
plot_data = data
data = plot_data[c('x', 'y', 'z', 'class')]
print(head(data, 5))
data$x = as.numeric(data$x)
data$y = as.numeric(data$y)
data$z = as.numeric(data$z)
# data$t = as.numeric(as.POSIXct(data$t))
data$class = factor(data$class, levels = c(0, 1, 2))
set.seed(123)
split = sample.split(data$class, SplitRatio = 0.75)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

classifier = svm(formula = class ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear',
                 cross = 10)

print(head(test_set[-4]), 5)
y_pred = predict(classifier, newdata = test_set[-4])
y_train_pred = predict(classifier, newdata = training_set[-4])

cm = table(test_set[, 4], y_pred)
cm2 = table(training_set[, 4], y_train_pred )

print(cm)
print(cm2)

#plot only if there are two columns for classification
# plot(classifier, training_set)

#plot two by two variables
plot(classifier, training_set, x ~ y)
plot(classifier, training_set, y ~ z)
plot(classifier, training_set, x ~ z)

cv_accuracies <- classifier$accuracies
all.equal(mean(cv_accuracies), classifier$tot.accuracy)
print(mean(cv_accuracies))
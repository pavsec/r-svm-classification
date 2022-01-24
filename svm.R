require(e1071)
library(caTools)
require(ggplot2)
library(caret)
library(yardstick)


setwd('D:/fax/Collage/9/uznl/projekt/r-svm-classification')
data = read.csv(paste('./datasets/clean datasets/svm_data.csv', sep = ''), head = TRUE, sep = ',', colClasses = 'character', skip = 0)

print(head(data, 5))
plot_data = data
data = plot_data[c('x', 'y', 'z', 'class')]

data$x = as.numeric(data$x)
data$y = as.numeric(data$y)
data$z = as.numeric(data$z)

#izbaci outliere
data = subset(data, x < 2000)
data = subset(data, z > -200)
data = subset(data, y > -1000)

#standardizacija - oduzimanjem srednje vrijednosti od svih znacajki
data$x = scale(data$x, scale = FALSE)
data$y = scale(data$y, scale = FALSE)
data$z = scale(data$z, scale = FALSE)
print(head(data, 5))

data$class = factor(data$class, levels = c(0, 1, 2))
set.seed(123)
split = sample.split(data$class, SplitRatio = 0.8)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

classifier = svm(formula = class ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial',
                 cost = 0.1,
                 gamma = 100,
                 cross = 10)

print('--------------------------')
print(classifier)

y_pred = predict(classifier, newdata = test_set[-4])
y_train_pred = predict(classifier, newdata = training_set[-4])

print('train confusion matrix')
print(table(test_set[, 4]))
train_results = confusionMatrix(data = y_train_pred, reference = training_set[, 4])
print(train_results)
set.seed(123)
truth_predicted = data.frame(
  obs = training_set[, 4],
  pred = y_train_pred
)
truth_predicted$obs = as.factor(truth_predicted$obs)
truth_predicted$pred = as.factor(truth_predicted$pred)

cm = conf_mat(truth_predicted, obs, pred)

autoplot(cm, type = 'heatmap') +
  scale_fill_gradient(low = 'pink', high = 'cyan')

print('')
print('test confusion matrix')
print(table(test_set[, 4]))
test_results = confusionMatrix(data = y_pred, reference = test_set[, 4])
print(test_results)
truth_predicted = data.frame(
  obs = test_set[, 4],
  pred = y_pred
)
truth_predicted$obs = as.factor(truth_predicted$obs)
truth_predicted$pred = as.factor(truth_predicted$pred)

cm = conf_mat(truth_predicted, obs, pred)

autoplot(cm, type = 'heatmap') +
  scale_fill_gradient(low = 'pink', high = 'cyan')

#plot two by two variables
# plot(classifier, training_set, x ~ y)
# plot(classifier, training_set, y ~ z)
# plot(classifier, training_set, x ~ z)

print('--------------------------')

#parameter tuning
#tune model
# set.seed (1)
# tune.out=tune(svm ,class~.,data=training_set ,kernel ='linear',
#               ranges =list(cost=10^(-3:2), gamma=10^(-3:2), coef0=10^(-1:1)))
# tune.out=tune(svm ,class~.,data=training_set,kernel ='radial',
#               ranges =list(cost=10^(-3:2), gamma=10^(-3:2)))
# tune.out=tune(svm ,class~.,data=training_set,kernel ='radial', cost=0.1,
#               ranges =list(coef0=10^(-1:1), gamma=10^(2:4)))
# tune.out=tune(svm ,class~.,data=training_set,kernel ='sigmoid',
#               ranges =list(cost=10^(-3:2), gamma=10^(-3:3), coef0=10^(-1:1)))
# summary(tune.out)


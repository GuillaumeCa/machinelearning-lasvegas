lasvegas = read.table("LasVegas.csv", sep=";", header = T)
summary(lasvegas)
attach(lasvegas)


# supervised learning

# classification

# Logistic regression (glm)
set.seed(1)

# transform data
Score.high = rep("HIGH", length(lasvegas$Score))
Score.high[lasvegas$Score >= 4] = "LOW"
lasvegas$Score = as.factor(Score.high)


# train model on full lasvegas dataset
log.fit = glm(Score ~ Helpful.votes + Hotel.stars + Free.internet + Traveler.type + Spa + Nr..rooms + Nr..reviews + Pool + Period.of.stay + Member.years, family=binomial, data=lasvegas)

# show model info
summary(log.fit)

# predict score based on the dataset
log.prob = predict(log.fit, type = "response")

# Gives value of HIGH and LOW for score (HIGH = 0, LOW = 1) 
contrasts(lasvegas$Score)

# transform score predictions to match the dataset format
log.pred = rep("HIGH", nrow(lasvegas))
log.pred[log.prob > 0.5] = "LOW"

library(pROC)

# Confusion matrix between actual score and predicted
table(Score, log.pred)

# Accuracy percentage
mean(log.pred == Score)
# Error rate
mean(log.pred != Score)

# compute roc
ROC.log = roc(lasvegas$Score, log.prob, levels = c("LOW", "HIGH"))

# plot ROC curve
plot.roc(ROC.log, print.auc = T, xlab = "1-Specificity", col = "red", axes = T)


# With test set

# split the dataset in two
train = (Member.years < 1)

# split Data
lasvegas.train = lasvegas[train,]
lasvegas.test = lasvegas[!train,]

# train model with train data
log.fit = glm(Score ~ Helpful.votes + Hotel.stars + Free.internet + Traveler.type + Spa + Nr..rooms + Nr..reviews + Pool + Period.of.stay + Member.years, family=binomial, data=lasvegas.train)

# get model info
summary(log.fit)

# predict score for the test set
log.prob = predict(log.fit, newdata = lasvegas.test, type = "response")

# Gives value of HIGH and LOW for score (HIGH = 0, LOW = 1)
contrasts(lasvegas.test$Score)

# transform the predictions vector to match the score in the test set
log.pred = rep("HIGH", nrow(lasvegas.test))
log.pred[log.prob > 0.5] = "LOW"

# display confusion matrix between the predictions and the dataset
table(log.pred, lasvegas.test$Score)

# Accuracy percentage
mean(log.pred == lasvegas.test$Score)
# Error rate
mean(log.pred != lasvegas.test$Score)

# compute roc
ROC.log.test = roc(lasvegas.test$Score, log.prob, levels = c("LOW", "HIGH"))

# plot ROC curve
plot.roc(ROC.log.test, print.auc = T, xlab = "1-Specificity", col = "red", axes = T)


# LDA
library(MASS)

lda.fit = lda(Score ~ Helpful.votes + Hotel.stars + Free.internet + Traveler.type + Spa + Nr..rooms + Nr..reviews + Pool + Period.of.stay + Member.years + User.continent, data = lasvegas, subset = train)
lda.pred = predict(lda.fit, lasvegas.test)

# Accuracy
mean(lda.pred$class == lasvegas.test$Score)

# Error rate
mean(lda.pred$class != lasvegas.test$Score)

# Confusion matrix
table(lda.pred$class, lasvegas.test$Score)


# 
lda.probs = lda.pred$posterior[,1]

ROC.lda = roc(lasvegas.test$Score, lda.probs, levels = c("LOW", "HIGH"))
plot.roc(ROC.lda, print.auc = T, xlab = "1-Specificity", col = "red", axes = T)

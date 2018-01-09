lasvegas = read.table("LasVegas.csv", sep=";", header = T)
summary(lasvegas)
attach(lasvegas)


# supervised learning

# classification
set.seed(1)

# transform data
Score.high = rep("HIGH", length(lasvegas$Score))
Score.high[lasvegas$Score >= 4] = "LOW"
lasvegas$Score = as.factor(Score.high)


# train model on full lasvegas dataset
log.fit = glm(Score ~ Helpful.votes + Hotel.stars + Free.internet + Traveler.type + Spa + Nr..rooms + Nr..reviews + Pool + Period.of.stay + Member.years, family=binomial, data=lasvegas)

# show model info
summary(log.fit)

# predict score based on the dataset (HIGH = 0, LOW = 1)
log.prob = predict(log.fit, type = "response")

# transform score predictions to match the dataset format
log.pred = rep("HIGH", nrow(lasvegas))
log.pred[log.prob > 0.5] = "LOW"

# Confusion matrix between actual score and predicted
table(Score, log.pred)

# Accuracy percentage
mean(log.pred == Score)
# Error rate
mean(log.pred != Score)


# With test set

# split the dataset in two
train = (Member.years < 2)

# split Data
lasvegas.train = lasvegas[train,]
lasvegas.test = lasvegas[!train,]

# train model with train data
log.fit = glm(Score ~ Helpful.votes + Hotel.stars + Free.internet + Traveler.type + Spa + Nr..rooms + Nr..reviews + Pool + Period.of.stay + Member.years, family=binomial, data=lasvegas.train)

# get model info
summary(log.fit)

# predict score for the test set (HIGH = 0, LOW = 1)
log.prob = predict(log.fit, newdata = lasvegas.test, type = "response")

# transform the predictions vector to match the score in the test set
log.pred = rep("HIGH", nrow(lasvegas.test))
log.pred[log.prob > 0.5] = "LOW"

# display confusion matrix between the predictions and the dataset
table(log.pred, lasvegas.test$Score)

# Accuracy percentage
mean(log.pred == lasvegas.test$Score)
# Error rate
mean(log.pred != lasvegas.test$Score)

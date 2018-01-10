lasvegas = read.table("LasVegas.csv", sep=";", header = T)
attach(lasvegas)

summary(lasvegas)

# Hierarchical clustering

library(tree)
set.seed(1)

# Create training sample
train = sample(1:nrow(lasvegas), nrow(lasvegas) / 2)

# Create tree without User.country
tree.lasvegas = tree(Score ~ ., lasvegas[,-1], subset = train)

# plot tree
plot(tree.lasvegas)
text(tree.lasvegas)

# Cross validation
cv.lasvegas = cv.tree(tree.lasvegas, K=10)

# plot
plot(cv.lasvegas$size, cv.lasvegas$dev, type = 'b')

# prune to 10 leaves
prune.lasvegas = prune.tree(tree.lasvegas, best = 10)

# display pruned tree
plot(prune.lasvegas)
text(prune.lasvegas)


#
yhat = predict(tree.lasvegas, newdata = lasvegas[-train,])
lasvegas.test = lasvegas[-train, "Score"]
mean((yhat - lasvegas.test)^2)


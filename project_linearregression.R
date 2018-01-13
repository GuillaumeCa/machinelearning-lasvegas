#----------------------Linear regression
# Load data set

lasvegas = read.table(file = 'LasVegas.csv',header=T,sep=";")

attach(lasvegas)

# With test set

train = (Member.years < 5)

# split Data
lasvegas.train = lasvegas[train,]
lasvegas.test = lasvegas[!train,]


#model_multiplereg=lm(Score~Hotel.stars+Hotel.name+Spa+Review.month+Review.weekday,data = lasvegas.train)
model_multiplereg=lm(Score~Hotel.stars+Pool+Traveler.type+Tennis.court+Member.years,data = lasvegas.train)
reg.prob = predict(model_multiplereg, newdata=lasvegas.test,type="response")
mean((lasvegas.test$Score-reg.prob)^2)


x=c(1:225)
plot(x, reg.prob,col="red")
lines(x, lasvegas.test$Score,col="green")







p = 1:7
x = df[,p]
y = df[[8]]


ggpairs(df, columns=c(1,2,3,6))

attach(df)

linearMod <- lm(expenses ~ ., data = df)
summary(linearMod)

require(ggiraph)
require(ggiraphExtra)
require(plyr)

plot(linearMod)


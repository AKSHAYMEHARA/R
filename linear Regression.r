library(tidyverse)
library(caTools)
library(Metrics)
axisbank <-read.csv(file("D:/axisbank.csv"), header = T, sep = ",")
axisbank <- data.frame(axisbank[,-1], row.names = axisbank[,1])
df <- axisbank%>%select(Open,High,Low,Close,VWAP,Volume)

# splitting data into train and test
set.seed(2)
split <- sample.split(df, SplitRatio = 0.7)
train <- subset(df, split = "TRUE")
test <- subset(df, split = "FALSE")

# creating a model
Model <- lm(Close~.,data = train)
summary(Model)

# Prediction
pred <- predict(Model, test)

# Accuracy Measures
mse <- mse(df$Close, pred)
print(mse)

rmse <- rmse(df$Close, pred)
print(rmse)

mae <- mae(df$Close, pred)
print(mae)

df2 <- data.frame("Actual" = df$Close, "Predicted" = pred)

ggplot(df2,aes(x = index(df2),y = df2[,1])) + 
  geom_line(aes(color="Actual")) +
  geom_line(y = df[,2],aes(color="Predicted")) +
  labs(color="Legend") + xlab("Date") + ylab("Price") +
  scale_colour_manual("", breaks = c("Actual", "Predicted"), values = c("blue", "red")) +
  ggtitle("Closing Stock Prices: Actual & Predicted") + 
  theme(plot.title = element_text(lineheight=.7, face="bold"))





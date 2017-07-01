setwd("~/Desktop/IMS/NOVO/Decision_Tree")
library(dplyr)
rawdata = read.csv("decision tree rawdata-v5-0607.csv")
rawdata = rawdata[,c(-1)]

# delete missing data
library(caret)
row.has.na <- apply(rawdata, 1, function(x){any(is.na(x))})
sum(row.has.na) # 759 rows with NA values
data.filtered <- rawdata[!row.has.na, ] # 2841 rows


###### READ ME --------
### rpart tree
library(rpart)
library(rpart.plot)

# look at the class y2 variable
table(data.filtered$y2)
data.filtered$y2 <- as.factor(data.filtered$y2)

data.filtered$z16 <- as.factor(data.filtered$z16)
data.filtered$z17 <- as.factor(data.filtered$z17)
data.filtered$z18 <- as.factor(data.filtered$z18)
data.filtered$z19 <- as.factor(data.filtered$z19)
data.filtered$z21 <- as.factor(data.filtered$z21)
data.filtered$z22 <- as.factor(data.filtered$z22)
data.filtered$z23 <- as.factor(data.filtered$z23)
data.filtered$x9 <- as.factor(data.filtered$x9)
data.filtered$x0 <- as.factor(data.filtered$x0)
data.filtered$x10 <- as.factor(data.filtered$x10)

# x11 as response variable
data.filtered$x11[data.filtered$x11 < 0.58 ] = "[0.34, 0.58)"
data.filtered$x11[data.filtered$x11 >= 0.58 & data.filtered$x11 < 0.68]= "[0.58, 0.68)"
data.filtered$x11[data.filtered$x11 >= 0.68 & data.filtered$x11 <= 0.75]= "[0.68, 0.75)"
data.filtered$x11[data.filtered$x11 >= 0.75 ] = "[0.75, 0.95]"
data.filtered$x11 <- as.factor(data.filtered$x11)

# try this model 20 times to get averaged model accuracy
accuracy_trial = 0 
for (i in 1: 20) {
  data_rand <- data.filtered[order(runif(2841)), ]
  data_train <- data_rand[1:1500, ]
  data_test  <- data_rand[1501:2841, ]
  
  fit <- rpart(x11 ~ x0 + x3 + x9 + x10 + z1 + z5 + z9 + z10 + z11 + z16 + z17
               + z18 + z19 + z21 + z22 + z23 + z25 + z26 + z27 + x6 + x7 + z3
               + x1 + x2 + z2 + z4 + z6 + z7 + z8
               + y2 + x5 + x8,
               data = data_train,
               method = "class",
               control = rpart.control(minsplit = 30, cp = 0.001))
  
  prediction = predict(fit, data_test, type = "class")
  submit = data.frame(data_test$x11, x11_predicted = prediction, match = (data_test$x11 == prediction))
  sumMatch = 0
  sumMatch = sum(submit$match) # number of correct predictions
  accuracy_trial[i] = sumMatch/nrow(submit)
}
accuracy_trial
sum(accuracy_trial)/20
# you can try different trial times ("i" in this function) and average the model accuracy
# also try different "minsplit" and "cp" values to improve accuracy and tree size


### specific example
data_train <- data.filtered[sample(1:nrow(data_train), replace=T),]

fit <- rpart(x11 ~ x0 + x3 + x9 + x10 + z1 + z5 + z9 + z10 + z11 + z16 + z17
             + z18 + z19 + z21 + z22 + z23 + z25 + z26 + z27 + x6 + x7 + z3
             + x1 + x2 + z2 + z4 + z6 + z7 + z8
             + y2 + x5 + x8,
             data = data_train,
             method = "class",
             control = rpart.control(minsplit = 10, cp = 0.001))
fit
summary(fit)
rpart.plot(fit)
printcp(fit)

prediction = predict(fit, data_test, type = "class")
library(gmodels)
CrossTable(data_test$x11, prediction,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual results', 'predicted results'))


### specific example

data_rand <- data.filtered[order(runif(2841)), ]
data_train <- data_rand[1:1500, ]
data_test  <- data_rand[1501:2841, ]

fit <- rpart(x11 ~ x0 + x3 + x9 + x10 + z1 + z5 + z9 + z10 + z11 + z16 + z17
             + z18 + z19 + z21 + z22 + z23 + z25 + z26 + z27 + x6 + x7 + z3
             + x1 + x2 + z2 + z4 + z6 + z7 + z8
             + y2 + x5 + x8,
             data = data_train,
             method = "class",
             control = rpart.control(minsplit = 30, cp = 0.001))
fit
summary(fit)
rpart.plot(fit)
printcp(fit)

prediction = predict(fit, data_test, type = "class")

CrossTable(data_test$x11, prediction,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual results', 'predicted results'))


# get model accuracy table: region

dff <- data.frame(region =data_test$x0,actual = data_test$x11,prediction)
dff$match <- (data_test$x11 == prediction)

df.region <- data.frame(region = c('a','c','d','e','f','g','h','i'), 
                        accuracy = 0)
sumTrue <- sum(dff[dff$region =='a' & dff$match,]$match)
sumRegion <- sum(dff$region == 'a')
df.region[1,'accuracy'] = sumTrue/sumRegion

sumTrue <- sum(dff[dff$region =='c' & dff$match,]$match); sumTrue
sumRegion <- sum(dff$region == 'c'); sumRegion
df.region[2,'accuracy'] = sumTrue/sumRegion

sumTrue <- sum(dff[dff$region =='d' & dff$match,]$match); sumTrue
sumRegion <- sum(dff$region == 'd'); sumRegion
df.region[3,'accuracy'] = sumTrue/sumRegion

sumTrue <- sum(dff[dff$region =='e' & dff$match,]$match); sumTrue
sumRegion <- sum(dff$region == 'e'); sumRegion
df.region[4,'accuracy'] = sumTrue/sumRegion

sumTrue <- sum(dff[dff$region =='f' & dff$match,]$match); sumTrue
sumRegion <- sum(dff$region == 'f'); sumRegion
df.region[5,'accuracy'] = sumTrue/sumRegion

sumTrue <- sum(dff[dff$region =='g' & dff$match,]$match); sumTrue
sumRegion <- sum(dff$region == 'g'); sumRegion
df.region[6,'accuracy'] = sumTrue/sumRegion

sumTrue <- sum(dff[dff$region =='h' & dff$match,]$match); sumTrue
sumRegion <- sum(dff$region == 'h'); sumRegion
df.region[7,'accuracy'] = sumTrue/sumRegion


sumTrue <- sum(dff[dff$region =='i' & dff$match,]$match); sumTrue
sumRegion <- sum(dff$region == 'i'); sumRegion
df.region[8,'accuracy'] = sumTrue/sumRegion

# get model accuracy table: title

dfff <- data.frame(title =data_test$x1,actual = data_test$x11,prediction)
dfff$match <- (data_test$x11 == prediction)

df.title <- data.frame(title = c(1,2,3), 
                        accuracy = 0)
sumTrue <- sum(dfff[dfff$title == 1 & dfff$match,]$match); sumTrue
sumTitle <- sum(dfff$title == 1)
df.title[1,'accuracy'] = sumTrue/sumTitle

sumTrue <- sum(dfff[dfff$title == 2 & dfff$match,]$match); sumTrue
sumTitle <- sum(dfff$title == 2)
df.title[2,'accuracy'] = sumTrue/sumTitle

sumTrue <- sum(dfff[dfff$title == 3 & dfff$match,]$match); sumTrue
sumTitle <- sum(dfff$title == 3)
df.title[3,'accuracy'] = sumTrue/sumTitle




## three specifi prediction example 
data.filtered[data.filtered$x9 == 0 & data.filtered$x7 >= 1.5 & data.filtered$x0== "d" & data.filtered$x8 < 2.5 
              & data.filtered$z26 < 8.6 & data.filtered$z10 >=12 & data.filtered$z10 < 15,]

data.filtered[data.filtered$x9 == 1 & (data.filtered$x0 == "b" | data.filtered$x0 == "h" | data.filtered$x0 == "f")  
              & data.filtered$x7 >=1.5 & data.filtered$z25 >=5.9 & data.filtered$z27 < 7.7 & data.filtered$x3 < 0.5 
              & data.filtered$z26 <9.9 & data.filtered$x7 <3.5,]

data.filtered[data.filtered$x9 == 1 & (data.filtered$x0 == "b" | data.filtered$x0 == "h" | data.filtered$x0 == "f") 
              & data.filtered$x7 >=1.5  & data.filtered$z25 >= 5.9 & data.filtered$z27 < 7.7 & data.filtered$x3 <0.5 
              & data.filtered$z26 >=9.9,]


## Exploratory data analysis
# x0: regions a - i
# check Region distribution against Perception Score
df = data.filtered[data.filtered$x11 == "[0.34, 0.58)",]
table(df$x0)
df2 = data.filtered[data.filtered$x11 == "[0.58, 0.68)",]
table(df2$x0)
df3 = data.filtered[data.filtered$x11 == "[0.68, 0.75)",]
table(df3$x0)
df4 = data.filtered[data.filtered$x11 == "[0.75, 0.95]",]
table(df4$x0)

# x1: physician title 1 - 3
# check Region distribution against Perception Score
table(df$x1)
table(df2$x1)
table(df3$x1)
table(df4$x1)

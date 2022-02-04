library(MASS)
library(nnet)
library(stats)
library(e1071)
library(randomForest)
library(rpart)



# --- read crises
data = read.csv("https://raw.githubusercontent.com/sylbarth/data/main/ews.csv")
head(data)

# --- create row names
nams = paste0(data[,1], data[,2])
row.names(data) = nams


# --- prepare the dataset
numdata = data[,3:ncol(data)]
numdata = na.omit(numdata)
head(numdata)


# --- PCA
#install.packages("FactoMineR")
library(FactoMineR)
pca = PCA(numdata, scale.unit=TRUE)

plot(pca)
plot(data.frame(pca$ind$coord), pch=19)

colour = ifelse(numdata$PCRISIS1==1, "red", "green")
plot(data.frame(pca$ind$coord), pch=19, col=colour)
plot(data.frame(pca$ind$coord), pch=19, col=ifelse(numdata$PCRISIS1==1, "red", "green"))

res.hcpc = HCPC( pca, nb.clust=-1 )



# --- analyse lin?aire discriminante
m <- lda(PCRISIS1 ~ ., data=numdata)
p <- predict(m, numdata)
table(p$class, numdata$PCRISIS1)
plot(m)


# --- logit
m <- glm(PCRISIS1~ ., data=numdata, family=binomial)
p <- predict(m, numdata, type="response") 
table(p>0.5, numdata$PCRISIS1)


# --- rpart
library(rpart)
library(rpart.plot)

m <- rpart(factor(PCRISIS1)~., data=numdata)
p <- predict(m, numdata, type="class")
table(p, numdata$PCRISIS1)

prp(m, extra=8, type=5)


# --- RF
m <- randomForest(factor(PCRISIS1)~., data=numdata)
p <- predict(m, numdata, type="class")
table(p, numdata$PCRISIS1)
varImpPlot(m)


# --- svm
m <- svm(PCRISIS1 ~ ., data=numdata)
p <- predict(m, numdata)
table(p, numdata$PCRISIS1)


# --- NNET
#install.packages("nnet")
library(nnet)
set.seed(123)

m <- nnet(factor(PCRISIS1)~., size=2, data=numdata)
p <- predict(m, numdata)
table(p>0.5, numdata$PCRISIS1)


m <- nnet(factor(PCRISIS1)~., size=50, data=numdata)
p <- predict(m, numdata)
table(p>0.5, numdata$PCRISIS1)


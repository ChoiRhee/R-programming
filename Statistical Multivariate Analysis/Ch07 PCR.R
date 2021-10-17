### 주성분 회귀 분석###
# 1. Standardize the independent & dependent variables
# 2. Get principal components scores from sample correlation matrix
# 3. Regress the standardized dependent variable on some principal components scores
# 4. Back transform to origin variables

setwd("C:/Users/choir/R-Scripts/R-programming/Statistical Multivariate Analysis/")
data = read.table("slump_test.data", sep = ",", header = T)[,-1]
# https://archive.ics.uci.edu/ml/datasets/Concrete+Slump+Test
data = data.frame(X1 = data[,1], X2 = data[,2], X3 = data[,3], X4 = data[, 4], X5 = data[,5], X6 = data[,6], X7 = data[,7], Y = data[,10])
head(data)

# 1. Standardize the independent & dependent variables
y.mean = apply(data[8], 2, mean)
y.std = apply(data[8], 2, sd)
scale.data = as.data.frame(scale(data))
head(scale.data)

# 2. Get principal components scores from sample correlation matrix
par(mfrow = c(1, 1))
data.pca = prcomp(scale.data[1:7])
plot(data.pca, type = "l") # pc1 ~pc4
head(round(data.pca$x, 3)) # pca scores

# 3. Regress the standardized dependent variable on some principal components scores
# pca score, scale y를 새로 만들어
y = scale(data[,8])
pca.score = data.pca$x
data = cbind(as.data.frame(pca.score), y)

data.pcr = glm(y~PC1 + PC2 + PC3 + PC4, data = data) # pc1~pc4로 회귀분석
summary(data.pcr)

one.vec = rep(1, dim(pca.score)[1])
x.mat = cbind(one.vec, pca.score[, (1:4)])
y.hat = x.mat %*% as.vector(data.pcr$coefficients)
head(y.hat, 3)

origin.y.hat = (y.hat * y.std) + y.mean
head(origin.y.hat, 3)

# R-square, adjust R-square 계산
r = sum((origin.y.hat - y.mean)^2) / sum((data[,8] + y.mean)^2)
ad.r = 1 - ((dim(x.mat)[1] - 1)*(1-r) / (dim(x.mat)[1] - dim(x.mat)[2]))

### 상관계수 행렬로 pcr (pls 패키지 사용) ###
library(pls)
# scale.data = as.data.frame(scale(data))
pcr = pcr(Y~ ., data = scale.data, ncomp = 4)
head(pcr$fitted.values, 3)
ftn = c(1: (103*3))
pcr.yhat = as.vector(pcr$fitted.values[-ftn]) # 4 comps
head(pcr.yhat, 3)

### 위의 2가지 방법 비교 ###
x.mat1 = cbind(one.vec, pcr$scores[, (1:4)])
original.pcr.y = (pcr.yhat * y.std) + y.mean
r2 = sum((original.pcr.y - y.mean)^2) / sum((data[,8] - y.mean)^2)
ad.r2 = 1 - ((dim(x.mat1)[1] - 1)*(1 - r2) / (dim(x.mat1)[1] - dim(x.mat1)[2]))
head(round(y.hat, 5) == round(pcr.yhat, 5), 5) 
round(r, 5) == round(r2, 5)
# 비교결과 두 방법의 결과가 같음

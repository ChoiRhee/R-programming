### 정규성 검정 ###
data = read.table("T1-2.DAT")
library(stats)
data = as.data.frame(data) # column 이름 넣으려고
names(data) = c("Density", 'Machine', 'Cross')
boxplot(data)
# 그림 그려보니까 스케일링 해야됨. (x-mean(x)) / sd(x)
data1 = as.data.frame(cbind(scale(data[,1]), scale(data[,2]), scale(data[,3])))
names(data1) = names(data)
boxplot(data1)
# Density 열에 있는 이상치 없애자
data[data$Density == max(data$Density),]
data2 = data1[-25, ] # 행 삭제
boxplot(data2)
# 히스토그램 그리기
p = dim(data)[2]
layout(matrix(1:p, ncol = p))
hist(data$Density)
hist(data$Machine)
hist(data$Cross)
layout(matrix(1))
# Machine 열은 정규분포처럼 보이지만 다른애들은 아님
# data2로 해보자
p = dim(data2)[2]
layout(matrix(1:p, ncol = p))
hist(data2$Density)
hist(data2$Machine)
hist(data2$Cross)
layout(matrix(1))
# 표준화하고 이상치를 삭제한 data2는 비교적 나아보인다.
# Q-Q plot을 그려보자
p = dim(data)[2]
layout(matrix(1:p, ncol = p))
qqnorm(data$Density)
qqnorm(data$Machine)
qqnorm(data$Cross)
layout(matrix(1))
# 히스토그램과 마찬가지로 Machine만 직선 형태를 보인다.
# data2로도 한번 해보자
p = dim(data)[2]
layout(matrix(1:p, ncol = p))
qqnorm(data2$Density)
qqnorm(data2$Machine)
qqnorm(data2$Cross)
layout(matrix(1))

# Shapiro-Wilk test (일변량)
shapiro.test(data$Density)
shapiro.test(data$Machine)
shapiro.test(data$Cross)
# Machine의 p값은 0.5741로, Machine만 정규분포를 따른다.
# 데이터를 표준화하고 이상치를 없앤 뒤 다시 도전
shapiro.test(data2$Density)
shapiro.test(data2$Machine)
shapiro.test(data2$Cross)
# α = 0.05 하에서 Density, Machine이 정규분포를 따른다.

# Box-Cox transformation (일변량)
data3 = data[-25, ]
library(car)
# 각 변수에 대해 람다 계산
lambda.den = powerTransform(data3$Density)
lambda.mac = powerTransform(data3$Machine)
lambda.cro = powerTransform(data3$Cross)
lambda.vec = c(as.vector(lambda.den$lambda), as.vector(lambda.mac$lambda), as.vector(lambda.cro$lambda))
# 람다를 이용한 powertransforming
trans.Density = (data3[,1]^lambda.vec[1] - 1)/lambda.vec[1]
trans.Machine = (data3[,2]^lambda.vec[2] - 1)/lambda.vec[2]
trans.Cross = (data3[,3]^lambda.vec[3] - 1)/lambda.vec[3]
trans.x.mat.uni = cbind(trans.Density, trans.Machine, trans.Cross)
# Q-Q plots
p = dim(data3)[2]
layout(matrix(1:p, ncol = p))
qqnorm(trans.x.mat.uni[, 1])
qqnorm(trans.x.mat.uni[, 2])
qqnorm(trans.x.mat.uni[, 3])
layout(matrix(1))

# Box-Cox transformation (다변량)
# 모든 변수의 람다 계산
data3 = data[-25,]
lambda.fit = powerTransform(data3)
lambda.vec = as.vector(lambda.fit$lambda)
# 모든 람다를 이용해 powertransforming
trans.Density = (data3[,1]^lambda.vec[1] - 1)/lambda.vec[1]
trans.Machine = (data3[,2]^lambda.vec[2] - 1)/lambda.vec[2]
trans.Cross = (data3[,3]^lambda.vec[3] - 1)/lambda.vec[3]
trans.x.mat.multi = cbind(trans.Density, trans.Machine, trans.Cross)
# Q-Q plots
p = dim(data3)[2]
layout(matrix(1:p, ncol = p))
qqnorm(trans.x.mat.multi[,1])
qqnorm(trans.x.mat.multi[,2])
qqnorm(trans.x.mat.multi[,3])
layout(matrix(1))
lambda.vec

# Shapiro-Wilk test (다변량)
data = as.matrix(data) # 원본 데이터
data1 = as.matrix(data1) # 표준화
data2 = as.matrix(data2) # 표준화 & 이상치 제거
data3 = as.matrix(data3) # 이상치 제거
trans.x.mat.uni # powertransformed (일변량)
trans.x.mat.multi # powertransformed (다변량)
library(mvnormtest)
mshapiro.test(t(data)) # 에러
mshapiro.test(t(data1)) # 에러
mshapiro.test(t(data2))
mshapiro.test(t(data3))
mshapiro.test(t(trans.x.mat.uni)) # 에러
mshapiro.test(t(trans.x.mat.multi))
# data, data1, uni는 다변량이 아닌 일변량이기 때문에 error 발생
# data2, data3의 p값은 똑같고, multi는 조금 더 낮다.

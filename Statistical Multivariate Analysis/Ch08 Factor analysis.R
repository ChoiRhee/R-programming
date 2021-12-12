setwd("C:/Users/choir/R-Scripts/R-programming/Statistical Multivariate Analysis/")

# 데이터를 먼저 읽어서 corr matrix 를 구한다.
# 임의의 다변량 방법을 시도하기 전에 data matrix의 corr이 identity matrix가 되냐 안되냐 따져야 함. I면 할필요 x
install.packages('psych')
library(psych)
data = read.table("T8-4.dat")
stock = as.data.frame(data)
names(stock) = c("JPMorgan", "Citibank", "WllsFargo", "RoyallDShell", "ExxonMobil")
len = dim(stock)[1]

# Correlation test
cors = cor(stock)
round(cors, 3) # 소수점 아래 3번쨰까지 반올림
cortest.bartlett(cors, n = len) # null hypo H_0: cor(data) = 1 > p-value 확인해서 기각할지 채택할 지 선택택
eigen(cors)$values # cor matrix의 고유값들 출력 

# Scree plot
pc = princomp(stock, cor = TRUE)
screeplot(pc, type = 'lines', main = "Scree plot")

# Factor analysis (디폴트는 ML 방법)
fact1 = factanal(stock, factors = 2, rotation = 'none', scores = 'regression') # rotation 없이
print(fact1$loadings, cutoff = 0)
fact2 = factanal(stock, factors = 2, rotation = 'varimax', scores = 'regression') # 주성분방법
print(fact2$loadings, cutoff = 0)

# Positioning of variables and factor scores pattern
par(mfrow = c(1, 2))

# positioning of variables
plot(fact2$loadings[, 1:2], type = 'n') # xlim = c(-0.3, 1.2)
text(fact2$loadings[, 1:2], names(stock))
abline(v = 0.45, h = 0.5)
grid()
title("Positioning of Variables")

# factor scores pattern
# fact2$scores > 랜덤하게 뿌려진 형태로 나오면 됨
rownames(stock) = c(seq(1:len))
plot(fact2$scores[, 1:2], type = 'n')
text(fact2$scores[, 1:2], rownames(stock))
abline(v = 0, h = 0)
grid()
title("Factor Score")

# FA using covariance matrix
covs = var(stock)
cors = cor(stock)
fact3 = factanal(covmat = covs, factors = 2, rotation = 'none')
fact3

fact4 = factanal(covmat = cors, factors = 2, rotation = 'none')
fact4
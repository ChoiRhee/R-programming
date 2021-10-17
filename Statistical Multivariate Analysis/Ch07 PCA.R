### 주성분분석 ###
setwd("C:/Users/choir/R-Scripts/R-programming/Statistical Multivariate Analysis/")
data = read.table("T5-1.DAT")
data = as.data.frame(data)
names(data) = c('Sweat', 'Sodium', 'Potassium')

# 공분산, 상관계수
s.cov = cov(data)
s.cor = cor(data)

# eigenvalues, eigenvectors
eigen.cov = eigen(s.cov)
eigen.cor = eigen(s.cor)

# 공분산 행렬로 pca
pca.cov = prcomp(data) # 디폴트가 공분산
pca.cov
summary(pca.cov)

# 상관계수 행렬로 pca
pca.cor = prcomp(data, center = T, scale = T)
pca.cor
summary(pca.cor)

# pc score (둘 다 같은 결과)
pca.cor$x[1,]
predict(pca.cor)[1,]

### data matrix 없이 cov, cor matrix만으로 PCA ###
s = matrix(c(1, 2, 0, 2, 5, 0, 0, 0, 2), 3) # cov mat
r = cov2cor(s) # cor mat
# cov mat으로 PCA
princomp(covmat = s)
# cor mat으로 PCA (2개 같음)
princomp(covmat = r)
princomp(cor = TRUE, covmat = s)
# eigen
eigen(s)
eigen(r)

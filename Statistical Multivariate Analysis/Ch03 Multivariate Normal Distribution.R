### Density & visualization ###
# µ = (0, 2)T & Σ = ((2, 1/√2)T,(1/√2, 1)T)
mu.vec = c(0, 2)
sigma.mat = matrix(c(2, 1/sqrt(2), 1/sqrt(2), 2), ncol = 2)
variable.num = 2
x.vec = c(2, 1) # given x
# 다변량정규분포, t분포 계산해주는 패키지
#install.packages('mvtnorm')
#library(mvtnorm) 
# 주어진 x = (2, 1)T, 평균 mu.vec, 분산 sigma.mat에 대한 분포 계산
density1 = dmvnorm(x = x.vec, mean = mu.vec, sigma = sigma.mat); density1
# 위의 2변량 정규분포를 시각화하기 위해 샘플을 뽑고 'MASS' 패키지로 kernel smoothing
install.packages("MASS"); library(MASS)
sample.vec = mvrnorm(1000, mu = mu.vec, Sigma = sigma.mat)
sample.kde = kde2d(sample.vec[,1], sample.vec[,2], n = 50) # n은 grid point개수
# 그래프 작성
layout(matrix(1:4, 2))
plot1 = contour(sample.kde)
plot2 = persp(sample.kde, phi = 45, theta = 30)
plot3 = image(sample.kde)
contour(sample.kde, add = T)
plot4 = persp(sample.kde, phi = 30, theta = 30)
layout(matrix(1))
# 또 다른 방법으로 2변량 정규분포 그래프 작성
mu1 = 0; s11 = 10 # x1의 평균, 분산
mu2 = 0; s22 = 10 # x2의 평균, 분산
s12 = 15; rho = 0.5 # x1, x2의 공분산,  상관계수
x1 = seq(-10, 10, length = 41)
x2 = x1

f = function(x1, x2){
  term1 = 1/(2*pi*sqrt(s11*s22*(1-rho^2)))
  term2 = -1/(2*(1-rho^2))
  term3 = (x1-mu1)^2/s11
  term4 = (x2-mu2)^2/s22
  term5 = 2*rho*((x1-mu1)*(x2-mu2))/(sqrt(s11)*sqrt(s22))
  term1 * exp(term2 * (term3 + term4 - term5))
}
# density의 값들 계산
z = outer(x1, x2, f)

# perspective plot
layout(matrix(1:2, 2))
persp(x1, x2, z, main = "Two dimensional Normal Distribution", theta = 30)
# Contour plot
contour(x1, x2, z, xlab = "x1", ylab = "x2", main = "Contour plot")
layout(matrix(1))

### 조건부 분포 ###
install.packages("condMVNorm"); library(condMVNorm)
result = condMVN(mean = mu.vec, sigma = sigma.mat, dependent.ind = 1, given.ind = 2, X.given = 1)
result
# -> (X1|X2 = 1) ~ N_1(µ_1|1 = -0.71, σ^2_1|2 = 1.5)

# (X2|X1 = x1, X3 = x3) 알고 싶다!
# µ = (-3, 1, 4)T
# µ1 = 1, µ2 = (-3, 4)T
# Σ = (c(1, -2, 0, -2, 5, 0, 0, 0, 2), 3)
# Σ11 = 5, Σ12 = (-2, 0), Σ22 = (c(1, 0, 0, 2), 2)
mu.vec = c(-3, 1, 4)
sigma.mat = matrix(c(1, -2, 0, -2, 5, 0, 0, 0, 2), 3)
given.x.vec = c(2, 3) # x1 = 2, x3 = 3
given.x.num = c(1, 3) # given X index
result = condMVN(mean = mu.vec, sigma = sigma.mat, dependent.ind = 2, given.ind = given.x.num, X.given = given.x.vec)
result
# -> (X2|X1 = 2, X3 = 3) ~ N_1(µ_2|1,3  = -9, σ^2_2|1,3 = 1)
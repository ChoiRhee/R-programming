### matrix ###
matrix(c(1, 2, 3, 4), 2, 2)
matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
rbind(c(1, 3), c(2, 4))
cbind(c(1, 2), c(3, 4))

### diagonal Matrix ###
I3 = diag(3); I3
I5 = diag(5); I5

### one matrix ###
J3 = rep(1, 3) %*% t(rep(1, 3)); J3
J5 = rep(1, 5) %*% t(rep(1, 5)); J5

### matrix product ###
I3 %*% J3

### Centering Matrix Cn = In - 1/n Jn ###
C3 = I3 - (1/3) * J3; C3

### Linear Dependence ###
# if span(v1, v2) = 0 then linearly dependent
# if span(v2, v2) != 0 then linearly independent
lin_dep.fun = function(r1, r2){r1*v1 + r2*v2}
v1 = c(1, 2); v2 = c(2, 4)
lin_dep.fun(0, 0)
lin_dep.fun(1, 3)
lin_dep.fun(-2, 0)

### rank of matrix ###
install.packages("matrixcalc")
library(matrixcalc)
matrix.rank(A)
matrix.rank(A+B)
matrix.rank(A%*%B)

### Trace of a Matrix ###
install.packages("matrixcalc")
matrix.trace(A%*%B) == matrix.trace(B%*%A)

### Eigenvaluse & Eigenvectors ###
# 합을 1로 normalized 한 값 출력
# values, vectors 같이 출력
eigen(A)
eigen(A)$values
eigen(A)$vectors # eigenvector는 scalar product하면 손으로 구한거랑 같아짐
# 원래 eigen value 값을 구하려면 vector 원소에 제곱
eigen(A)$vectors^2
# A의 trace와 A의 eigen values의 합은 같음
matrix.trace(A) == sum(eigen(A)$values)

### determinent ###
det(A)
prod(eigen(A)$values)

### Gram-Schmidt process of orthgonalization ###
x1 = c(2, 0, 0); x2 = c(2, 2, 0); x3 = c(2, 0, 3)
X = cbind(x1, x2, x3)
install.packages("pracma")
library(pracma)
gramSchmidt(X)

### Quadratic forms and definite matricies ###
# matrix A의 모든 고유값이 0보다 크면 A는 p.d matrix이다.
A = matrix(c(3, 5, 1, 5, 13, 0, 1, 0, 1), 3)
all(eigen(A)$values > 0)
eigen(A)$values

### orthogonal matrix ###
P = matrix(c(-1, 0, 0, 0, -1, 0, 0, 0, 1), 3)
sum(t(P)- solve(P)) # PT = P^-1
det(P) # det(P) = +-1
all(abs(diag(P)) <= 1) # -1 <= p_ii <= 1

### Idempotent matrix ###
# AA = A
A = matrix(c(2, -1, 1, -2, 3, -2, -4, 4, -3), 3)
sum(A%*%A - A)

### Decomposition of a matrix ###
A = matrix(c(3, 5, 1, 5, 13, 0, 1, 0, 1), 3)
# diag(λ_ii)
D = diag(eigen(A)$values); D
# eigen vectors
p1 = eigen(A)$vectors[,1]
p2 = eigen(A)$vectors[,2]
p3 = eigen(A)$vectors[,3]
P = cbind(p1, p2, p3); P
# A = PΛt(P)
A
P%*%D%*%t(P)
# A^-1 = PΛ^-1P^T
solve(A)
P%*%solve(D)%*%t(P)

### Kronecker products of matrices ###
A = matrix(c(1, 2, 3, 4), 2)
B = matrix(c(2, 3, 4, 5), 2)
C = matrix(c(4, 5, 6, 7), 2)
D = matrix(c(4, 5, 6, 7), 2)
# A⊗B
kronecker(A, B)
# (A⊗B)(C⊗D) = AC⊗BD
sum(kronecker(A, B) %*% kronecker(C, D) - kronecker(A%*%C, B%*%D)) 
# abT = a⊗bT = bT⊗a
a = 1:3; b = 4:6
all.equal(a%*%t(b), kronecker(a, t(b)), kronecker(t(b), a)) 

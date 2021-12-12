# Classification 
library(WMDB)
library(MASS)

# data read
irises = read.table("T11-5.dat")
irises = data.frame(irises)
names(irises) = c('SL', 'SW', 'PL', 'PW', 'cla')

irises$cla = as.numeric(irises$cla)
head(irises)

# Simple histogram by the grouping variable
ldahist(data = irises$SL, g = irises$cla, type = 'both') # histogram, density

# Mahalanobis Distance
G = as.factor(irises[, 5])
clasts = wmd(irises[, -5], G)

# ------------------------------------------------------------------------------
# Linear Discriminant Analysis (LDA)
ld_iri = lda(cla ~ SL+SW+PL+PW, data = irises)
ld_iri

predc = predict(ld_iri, irises)$class
predc = as.numeric(predc)
predc

ta_class = table(original = irises$cla, classify = predc)
ta_class

correct.rate = sum(diag(ta_class)) / sum(ta_class)
error.rate = 1 - correct.rate
correct.rate
error.rate

# with cv : leave-one-out cross validation
ldc_iri = lda(cla~SL+SW+PL+PW, data = irises, CV = TRUE)
results = data.frame(irises$cla, ldc_iri$class)
head(results)

class.table = table(irises$cla, ldc_iri$class)
class.table

correct.rate = sum(diag(class.table)) / sum(class.table)
errorr.rate = 1 - correct.rate
correct.rate
error.rate

# ------------------------------------------------------------------------------
# Quadratic Discriminant Analysis
qd_iri = qda(cla~SL+SW+PL+PW, data = irises)
qd_iri

predc.qd = predict(qd_iri, irises)$class
predc.qd = as.numeric(predc.qd)
predc.qd

ta_class.qd = table(original = irises$cla, classify = predc.qd)
ta_class.qd

correct.rate = sum(diag(ta_class.qd)) / sum(ta_class.qd)
error.rate = 1 - correct.rate
correct.rate
error.rate

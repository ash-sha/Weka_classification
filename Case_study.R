library(NeuralNetTools)
library(neuralnet)
library(nnet)

XOR <- c(0,1,1,0)
xor.data <- data.frame(expand.grid(c(0,1), c(0,1)), XOR)
print(net.xor <- neuralnet( XOR~Var1+Var2, xor.data, hidden=2, rep=5))
plot(net.xor, rep="best")

#md

data(neuraldat)
set.seed(123)
mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5)
garson(mod)


library(RSNNS)
data(neuraldat)
x <- neuraldat[, c('X1', 'X2', 'X3')]
y <- neuraldat[, 'Y1']
mod <- mlp(x, y, size = 5)
garson(mod, 'Y1')

data(neuraldat)
set.seed(123)
mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5)
olden(mod)

set.seed(123)
mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5)
lekprofile(mod)

mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5, linout = TRUE,skip = TRUE)
neuralskips(mod)

library(nnet)
mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5, linout = TRUE)
neuralweights(mod)

set.seed(123)
mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5)
mat_in <- neuraldat[, c('X1', 'X2', 'X3')]
grps <- apply(mat_in, 2, quantile, seq(0, 1, by = 0.2))
pred_sens(mat_in, mod, 'X1', 100, grps, 'Y1')


x <- neuraldat[, c('X1', 'X2', 'X3')]
grps <- kmeans(x, 6)$center
lekgrps(grps)

Var1 <- runif(5, 0, 10)
sqrt.data <- data.frame(Var1, Sqrt=sqrt(Var1))
print(net.sqrt <- neuralnet(Sqrt~Var1, sqrt.data, hidden=3,threshold=0.01))
compute(net.sqrt, (1:3)^2)$net.result


data(infert, package="datasets")
print(net.infert <- neuralnet(case~parity+induced+spontaneous,
infert, err.fct="ce", linear.output=FALSE))
confidence.interval(net.infert)


data(infert, package="datasets")
print(net.infert <- neuralnet(case~parity+induced+spontaneous, infert,
err.fct="ce", linear.output=FALSE, likelihood=TRUE))
gwplot(net.infert, selected.covariate="parity")
gwplot(net.infert, selected.covariate="induced")
gwplot(net.infert, selected.covariate="spontaneous")




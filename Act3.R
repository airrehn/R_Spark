cancer <- tibble(case = factor(c(rep(1,3),rep(0,3)),
                               levels = c(0,1),
                               labels = c("control","case")),
                 x1 = c(1,2,3,4,2,3),
                 x2 = c(4,8,6,0,2,4))

matrix <- c(2,6)
dim(matrix) <- c(2,1)
u1 <- matrix

u2 <- c(3,2)
dim(u2) <- c(2,1)
u2


X <- matrix(c(-1,0,1,1,-1,0,-2,2,0,-2,0,2), nrow=6, byrow=FALSE)
X

summation <- 0.25 * t(X) %*% X
summation

inv_summ <- solve(summation)
inv_summ

A1 = inv_summ %*% u1
B1 = t(u1) %*% inv_summ %*% u1
A2 = inv_summ %*% u2
B2 = t(u2) %*% inv_summ %*% u2
B1

Adiff <- A1 - A2
Bdiff <- B1 - B2

Adiff_inv <- solve(Adiff)

# Crap de Quiz 1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
weighted.mean(x,w)


x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(I(y~ x))
lm( formula=I(y-mean(y)) ~ I(x - mean(x) -1 ))
lm(I(x~ y))
lm( formula=I(x-mean(x)) ~ I(y - mean(y) -1 ))

library(datasets)
data(mtcars)
lm( formula=I(mtcars$wt-mean(mtcars$wt)) ~ I(mtcars$mpg-mean(mtcars$mpg)), 
    data=mtcars)
lm( formula=I(mtcars$mpg-mean(mtcars$mpg)) ~ I(mtcars$wt-mean(mtcars$wt)), 
    data=mtcars)


corre = 0.5
sdx = 0.5
sdy = 1
slope=corre * (sdy/sdx)

0.4 * 1.5


x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
xn = (x-mean(x))/sd(x)

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(I(y~ x))


x2 <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
lm(I(x2~ x2))


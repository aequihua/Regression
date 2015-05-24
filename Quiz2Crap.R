
# Pregunta 1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

fit <- lm(y~x)


# Ejemplo de predict
newx <- c(0.16, 0.27, 0.34)
coef(fit)[1] + coef(fit)[2] * newx

predict(fit, newdata = data.frame(carat = newx))


# Ejemplo residuales
data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
e <- resid(fit)
yhat <- predict(fit)
max(abs(e -(y - y
            hat)))

# Residual sigma
summary(fit)$sigma

# Caso de mtcars- Preguntas 3 y 4
library(datasets)
data(mtcars)
y <- mtcars$mpg
x <- mtcars$wt
n <- length(y)
fit <- lm(y~I(x-mean(x))) # Saco la regresion lineal restando la media para que beta0 sea el valor esperado de Y en el X media
# Saco los coeficientes de la regresion y su intervalo de confianza de los betas
# El "intervalo de confianza en el promedio" es el intervalo de confianza de beta0
sumCoef <- summary(fit)$coefficients
sumCoef[1,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[1, 2] # Para beta0


# Pregunta 5 - Inferencia (prediccion)
newdata <- data.frame(x = 3.0)
p2 <- predict(fit, newdata, interval = ("prediction"))

# Pregunta 6 - Intervalo de confianza beta1
sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2] # Para beta1

# Pregunta 9 - Comparar la suma de los cuadrados de los errores en el modelo 
# con X e Y vs. el modelo con solo el intercept
y <- mtcars$mpg
x <- mtcars$wt
n <- length(y)
fitxy <- lm(y~I(x-mean(x)))
fitx <- lm(I(x~ x))


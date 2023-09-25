# https://cran.r-project.org/web/packages/GauPro/vignettes/GauPro.html

library(ggplot2)
library(GauPro)


# Gaussian Process 



# data from a function 
x <- seq(0,1,l=10)
y <- abs(sin(2*pi*x))^.8
plot(x, y)


# fit a linear model 
lm_mod <- lm(y ~ x)

# bad fit !!! 
plot(x, y)
abline(a=lm_mod$coef[1], b=lm_mod$coef[2], col='red')


plot(y, lm_mod$residiuals)
plot(fitted(lm_mod), resid(lm_mod))

resid(lm_mod)



# model with gaussian process 
gp <- GauPro(x, y, parallel=FALSE) # gaussian kernel by default

plot(x, y)
curve(gp$predict(x), add=T, col=2)
curve(gp$predict(x)+2*gp$predict(x, se=T)$se, add=T, col=4)
curve(gp$predict(x)-2*gp$predict(x, se=T)$se, add=T, col=4)

if (requireNamespace("MASS", quietly = TRUE)) {
  plot(gp)
}


# try new kernel 
kern <- Matern52$new(0)
gpk <- GauPro_kernel_model$new(matrix(x, ncol=1), y, kernel=kern, parallel=FALSE)

if (requireNamespace("MASS", quietly = TRUE)) {
  plot(gpk)
}
  

kern.exp <- Exponential$new(0)
gpk.exp <- GauPro_kernel_model$new(matrix(x, ncol=1), y, kernel=kern.exp, parallel=FALSE)
if (requireNamespace("MASS", quietly = TRUE)) {
  plot(gpk.exp)
}


kern.exp <- Exponential$new(0)
trend.0 <- trend_0$new()
gpk.exp <- GauPro_kernel_model$new(matrix(x, ncol=1), y, kernel=kern.exp, trend=trend.0, parallel=FALSE)
if (requireNamespace("MASS", quietly = TRUE)) {
  plot(gpk.exp)
}


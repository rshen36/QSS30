strikeOut = function (p) {
  return(choose(6,3)*p^3*(1-p)^3)
}

omitCubic = function (n) {
  # define variables
  x1 = rnorm(n)
  x2 = x1^2
  x3 = x1^3
  e = rnorm(n)
  y = 1 + 2*x1 + 3*x2 + 4*x3 + e
  
  # no variable omitted
  fit1 = lm(y ~ 1 + I(2*x1) + I(3*x2) + I(4*x3))
  print(summary(fit1))
  
  # cubic omitted
  fit2 = lm(y ~ 1 + I(2*x1) + I(3*x2))
  print(summary(fit2))
  
  plot(x1, fit2$residuals, 
       xlab = "x values", ylab = "Residuals from incorrect regression")
}
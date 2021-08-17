
#Yield and spawning biomass-per-recruit model with fixed fishing mortality
YPR.fun <- function(F_ = 0.2, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
                    to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM, 
                    approxim="i"){
                    
  Sa   = exp(-Ma) # supervivencia por mortalidad natural a la edad
  wa   = Winf*(1-exp(-k*(edad-to)))^b # peso a la edad
  la   = Linf*(1-exp(-k*(edad-to)))   # talla a la edad
  va   = 1/(1+exp(-log(3)*(la-aS)/bS))
  ma   = 1/(1+exp(aM-bM*la))
  fa   = wa*ma
  
  N    = rep(NA, length(edad))
  N[1] = 1
  n    = length(N)
  for(i in 1:(n-1)){
    N[i+1]<- N[i]*(1-va[i]*F_)*Sa[i]                 
  }
  N[n] <- N[n-1]*((1-va[n-1]*F_)*Sa[n-1]/(1-(1-va[n-1]*F_)*Sa[n-1]))# grupo plus
  

  BR <-sum(N*fa)        # Spawning biomass per recruit 
  YR  <- sum(N*va*F_*wa)  # Yield per recruit
  
  lst <- list(YR = YR, BR = BR, F = F, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
              to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM, 
              approxim = approxim[1])
  return(lst)
}


Fmax <- function (obj) 
{
  # if (class(obj) != "BH") {
  #   stop("Argument must be BH class")
  # }
  B.H <- function(F_, Ma, edad, a, b, Linf, k,
                  to, Winf, aS, bS, aM, bM, 
                  approxim) {
    YPR.fun(F_ = F_, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
            to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM,
            approxim = approxim)$YR
    
    # Beverton.Holt(F = F, Tr = Tr, Tc = Tc, Tmax = Tmax, 
    #               M = M, Wt = Wt, Winf = Winf, K = K, t0 = t0, b = b, 
    #               approxim = approxim)$YR
  }
  result <- optimize(f = B.H, interval = c(0, 1), maximum = TRUE, 
                     tol = 1e-04, Ma = obj$Ma, edad = obj$edad, a = obj$a, b = obj$b, Linf = obj$Linf, k= obj$k,
                     to  = obj$to, Winf = obj$Winf, aS = obj$aS, bS = obj$bS, aM = obj$aM, bM = obj$bM, 
                     approxim = obj$approxim)
  result$maximum
}

F0.1 <- function (obj) 
{
  dX <- .Machine$double.eps^0.25
  fdX <- YPR.fun(F_ = dX, Ma = obj$Ma, edad = obj$edad, a = obj$a, b = obj$b, Linf = obj$Linf, k= obj$k,
                 to  = obj$to, Winf = obj$Winf, aS = obj$aS, bS = obj$bS, aM = obj$aM, bM = obj$bM, 
                 approxim = obj$approxim)$YR
  DfX0 <- fdX/dX
  DfX0.1 <- 0.1 * DfX0
  funBH <- function(F_, Ma, edad, a, b, Linf, k,
                    to, Winf, aS, bS, aM, bM, approxim, Df) {
    
    dX <- .Machine$double.eps^0.25
    FdX <- F_ + dX
    fX   <- YPR.fun(F_ = F_, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
                   to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM, 
                  approxim = approxim)$YR
    fXdX <- YPR.fun(F_ = FdX, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
            to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM, 
            approxim = approxim)$YR
    DfX <- (fXdX - fX)/dX
    abs(Df - DfX)
  }
  result <- optimize(f = funBH, interval = c(0, Fmax(obj)), 
                     Ma = obj$Ma, edad = obj$edad, a = obj$a, b = obj$b, Linf = obj$Linf, k= obj$k,
                     to  = obj$to, Winf = obj$Winf, aS = obj$aS, bS = obj$bS, aM = obj$aM, bM = obj$bM, 
                     approxim = obj$approxim, Df = DfX0.1)
  result$minimum
}


F0.x <- function (obj, x = 1) 
{
  fx <- function(x, Ma, edad, a, b, Linf, k,
           to, Winf, aS, bS, aM, bM, approxim, Df) {
      YPR.fun(F_ = x, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
                    to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM, 
                    approxim = approxim)$YR
  }
  
  DfxOrigin <- num.deriv(fun = fx, x = 0, Ma = obj$Ma, edad = obj$edad, a = obj$a, b = obj$b, Linf = obj$Linf, k= obj$k,
                         to  = obj$to, Winf = obj$Winf, aS = obj$aS, bS = obj$bS, aM = obj$aM, bM = obj$bM, 
                         approxim = obj$approxim)$deriv
  
  DfxTarget <- (x/10) * DfxOrigin
  
  funMIN <- function(x, Ma, edad, a, b, Linf, k,
                     to, Winf, aS, bS, aM, bM, approxim, target) {
    
    Dfx <- num.deriv(fun = fx, x = x, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
                     to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM, 
                     approxim = approxim)$deriv
    Dfx - target
  }
  result <- uniroot(f = funMIN, interval = c(0, Fmax(obj)), 
                    tol = 1e-04, Ma = obj$Ma, edad = obj$edad, a = obj$a, b = obj$b, Linf = obj$Linf, k= obj$k,
                    to  = obj$to, Winf = obj$Winf, aS = obj$aS, bS = obj$bS, aM = obj$aM, bM = obj$bM, 
                    approxim = obj$approxim, target = DfxTarget)
  result$root
  
}


F40 <- function(obj, p = 40){
  x <- profile.YR(obj)
  PR_F40 <- x$Fs[which.min(abs(x$BR/x$BR[1]*100 - 40))]
  return(PR_F40)
}
  
  
  
num.deriv <- function (fun, x, h = 0.5, ...) 
{
  if (!is.function(fun)) {
    stop("fun should be a function")
  }
  if (h == 0) {
    stop("h must be nonzero")
  }
  TAB <- 10
  dStep <- 1.4
  dStep2 <- dStep * dStep
  a <- matrix(rep(NA, TAB * TAB), ncol = TAB, nrow = TAB)
  a[1, 1] <- (fun(x + h, ...) - fun(x - h, ...))/(2 * h)
  err <- 1e+30
  for (i in seq(2, TAB)) {
    h <- h/dStep
    a[1, i] <- (fun(x + h, ...) - fun(x - h, ...))/(2 * 
                                                      h)
    fac <- dStep2
    for (j in seq(2, i)) {
      a[j, i] <- (a[j - 1, i] * fac - a[j - 1, i - 1])/(fac - 
                                                          1)
      fac <- fac * dStep2
      errt <- max(c(abs(a[j, i] - a[j - 1, i]), abs(a[j, 
                                                      i] - a[j - 1, i - 1])))
      if (errt <= err) {
        err <- errt
        ans <- a[j, i]
      }
    }
    if (abs(a[i, i] - a[i - 1, i - 1]) >= 2 * err) 
      break
  }
  lst <- list(deriv = ans, error = err)
  return(lst)
}


profile.YR <- function(obj, Fs = seq(0,1, by = 0.01)){
  
  YR <- rep(NA, length(Fs))
  BR <- rep(NA, length(Fs))

  for(i in seq_along(Fs)){
    YR[i] <- YPR.fun(F_ = Fs[i], Ma = obj$Ma, edad = obj$edad, a = obj$a, b = obj$b, Linf = obj$Linf, k= obj$k,
                     to  = obj$to, Winf = obj$Winf, aS = obj$aS, bS = obj$bS, aM = obj$aM, bM = obj$bM, approxim = obj$approxim)$YR
    BR[i] <- YPR.fun(F_ = Fs[i], Ma = obj$Ma, edad = obj$edad, a = obj$a, b = obj$b, Linf = obj$Linf, k= obj$k,
                     to  = obj$to, Winf = obj$Winf, aS = obj$aS, bS = obj$bS, aM = obj$aM, bM = obj$bM, approxim = obj$approxim)$BR
  }
  lst <- list(Fs = Fs, YR = YR, BR = BR)
  return(lst)
}

#obj = obj_fav
getPR <- function (obj, Flab = "Tasa de explotación", YRlab = "YR", 
                     BRlab = "BR", mFmed = NULL) 
{
  x <- profile.YR(obj)
  decimal <- function(x) {
    d <- -9
    while (10^d < x) {
      d <- d + 1
    }
    10^d
  }
  # byYR <- decimal(max(x$YR) - min(x$YR))/10
  # byBR <- decimal(max(x$BR) - min(x$BR))/10
  # scaleYR <- seq(from = floor(min(x$YR)), to = ceiling(max(x$YR)), 
  #                by = byYR)
  # scaleBR <- seq(from = decimal(min(x$BR)), to = ceiling(max(x$BR)), 
  #                by = byBR)
  
  
  byYR <- (max(x$YR) - min(x$YR))/10
  byBR <- (max(x$BR) - min(x$BR))/10
  scaleYR <- round(seq(from = floor(min(x$YR)), to = ceiling(max(x$YR)), 
                 by = byYR),0)
  scaleBR <- round(seq(from = floor(min(x$BR)), to = ceiling(max(x$BR)), 
                 by = byBR),0)
  
  plot.new()
  par(mar = c(4, 4, 4, 4) + 0.25)
  plot.window(xlim = c(min(x$Fs), max(x$Fs)), ylim = c(0, 1))
  lines(x = x$Fs, y = x$YR/max(x$YR))
  lines(x = x$Fs, y = x$BR/max(x$BR))
  axis(3, at = seq(0,1, by = 0.2))
  axis(1, at = seq(0,1, by = 0.2), labels = round(-log(1-c(0,0.2,0.4,0.6,0.8,0.99)),1))
  axis(2, at = scaleYR/max(x$YR), labels = scaleYR)
  axis(4, at = scaleBR/max(x$BR), labels = scaleBR)
  box()
  #title(xlab = Flab, ylab = YRlab, cex = 1)
  mtext(BRlab, side = 4, line = 2.8, cex = 1)
  mtext(Flab, side = 1, line = 2.8, cex = 1)
  mtext(YRlab, side = 2, line = 2.8, cex =  1)

  P.Fmax <- Fmax(obj)
  P.F01  <- F0.x(obj, x = 1)
  P.F40  <- F40(obj)
  
  YRmax <- YPR.fun(F_ = P.Fmax, Ma = obj$Ma, edad = obj$edad, a = obj$a, b = obj$b, Linf = obj$Linf, k= obj$k,
                         to  = obj$to, Winf = obj$Winf, aS = obj$aS, bS = obj$bS, aM = obj$aM, bM = obj$bM, approxim = obj$approxim)$YR/max(x$YR)
  YR01 <- YPR.fun(F_ = P.F01, Ma = obj$Ma, edad = obj$edad, a = obj$a, b = obj$b, Linf = obj$Linf, k= obj$k,
                        to  = obj$to, Winf = obj$Winf, aS = obj$aS, bS = obj$bS, aM = obj$aM, bM = obj$bM, approxim = obj$approxim)$YR/max(x$YR)
  
  BS40 <- YPR.fun(F_ = P.F40, Ma = obj$Ma, edad = obj$edad, a = obj$a, b = obj$b, Linf = obj$Linf, k= obj$k,
                  to  = obj$to, Winf = obj$Winf, aS = obj$aS, bS = obj$bS, aM = obj$aM, bM = obj$bM, approxim = obj$approxim)$BR/max(x$BR)
  
  
  points(x = P.Fmax, y = YRmax, pch = "+", cex = 2, col = 2, lwd = 2)
  points(x = P.F01, y = YR01, pch = "+", cex = 2, col = 3, lwd = 2)
  points(x = P.F40, y = BS40, pch = "+", cex = 2, col = 4, lwd = 2)
  PFmed = NULL
  if(!is.null(mFmed)){
    PFmed = x$Fs[which.min(abs(x$BR - mFmed))]
    BSmed <- mFmed/max(x$BR)
  points(x = PFmed, y = BSmed, pch = "+", cex = 2, col = 6, lwd = 2) 
  lines(x = c(-1, PFmed, PFmed), y = c(BSmed, BSmed, -1), lty = "dashed", col = 6)
  }
  lines(x = c(-1, P.Fmax, P.Fmax), y = c(YRmax, YRmax, -1), 
        lty = "dashed", col = 2)
  lines(x = c(-1, P.F01, P.F01), y = c(YR01, YR01, -1), lty = "dashed", col = 3)
  lines(x = c(-1, P.F40, P.F40), y = c(BS40, BS40, -1), lty = "dashed", col = 4)
  
  
  
  if(P.Fmax > 0.99){
    P.Fmax = 0.99
  }
  PR.Fmax <- -log(1 - round(P.Fmax,2))
  PR.F01 <- -log(1 - round(P.F01,2))
  PR.F40 <- -log(1 - round(P.F40,2))
  PRFmed <- -log(1 - round(PFmed,2))
  
  legend("bottomleft", legend = c(paste0("Fmax=",round(PR.Fmax,2)), 
                               paste0("F01=",round(PR.F01,2)),
                               paste0("F40=",round(PR.F40,2)),
                               paste0("Fmed=",round(PRFmed,2))),
                               col = c(2,3,4,6), pch = "+", bty = "n", cex = 1.2)
  
  return(list(Fmax = PR.Fmax, F01 = PR.F01, F40 = PR.F40, Fmed = PRFmed))
}

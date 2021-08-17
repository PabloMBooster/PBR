options(max.print=12000,scipen=6) # mostrar decimales
# -------------------------------------------------------------------------
source("R/funciones.R")

## REGIMEN DE PRODUCTIVIDAD MEDIA

# -------------------------------------------------------------------------
# RELACION S-R ------------------------------------------------------------
# -------------------------------------------------------------------------

source("R/SR.Fun.R")
dataR <- read.csv(file = "data/output_SR_ricardoModel.csv")
names(dataR) <- c("year", "ssb", "rec" )

yr0 = 1972
yr1 = 1993
data = dataR[dataR$year > yr1 , ]# ultimo regimen
st=data$ssb*1000
rt=data$rec
yr=data$year

# PENDIENTE ><> PARA ENCONTRAR Fmed
modRicker <-  SR.Fun("Ricker")
text(data$ssb*1000, data$rec, labels = round(data$year),pos = 3, cex = 0.8)

mFmed = modRicker$mFmed 


## YPR
## parametros - [escenario favorable] 

a = 0.0036
b = 3.238

Linf = 20.63
k    = 1
to   = -0.18
Winf = a*Linf^b
aS = 11.57 # a selectividad
bS = 0.84  # b selectividad
aM = 21.01 # a madurez
bM = 1.88  # b madurez
edad = seq(1,4,by=1)
M_adultos     = 0.83
M_reclutas    = 0.91
M_prereclutas = 1.29
Ma = c(0.91, 0.83, 0.83) # mortalidad natural a la edad
#Ma = c(1.29, 1.29, 0.91, 0.83, 0.83, 0.83,0.83, 0.83, 0.83)/2
obj_fav <- YPR.fun(F_ = 0.2, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
               to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM, approxim="i")

PR_fav = getPR(obj_fav, Flab = "F", mFmed = mFmed)

par(mfrow=c(1,2))
getPR(obj_fav, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")


## parametros - [escenario neutro] 

a = 0.0036
b = 3.238

Linf = 20.59
k    = 0.84
to   = -0.21
Winf = a*Linf^b
aS = 11.57 # a selectividad
bS = 0.84  # b selectividad
aM = 21.01 # a madurez
bM = 1.88  # b madurez
edad = seq(1,4,by=1)
M_adultos     = 0.83
M_reclutas    = 0.91
M_prereclutas = 1.29
Ma = c(0.91, 0.83, 0.83)  # mortalidad natural a la edad

obj_neu <- YPR.fun(F_ = 0.2, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
               to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM, approxim="i")
pro_neu <- profile.YR(obj_neu)

par(mfrow=c(1,2))
getPR(obj_neu, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")
title(main = "1993-2017")
lines(n$x, n$y, col = 2)

## parametros - [escenario neutro desfavorable] 

a = 0.0036
b = 3.238

Linf = 20.37
k    = 0.73
to   = -0.24
Winf = a*Linf^b
aS = 11.57 # a selectividad
bS = 0.84  # b selectividad
aM = 21.01 # a madurez
bM = 1.88  # b madurez
edad = seq(1,4,by=1)
M_adultos     = 0.83
M_reclutas    = 0.91
M_prereclutas = 1.29
Ma = c(0.91, 0.83, 0.83)  # mortalidad natural a la edad

obj_neudes <- YPR.fun(F_ = 0.2, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
                   to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM, approxim="i")
pro_neudes <- profile.YR(obj_neudes)

par(mfrow=c(1,2))
getPR(obj_neudes, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")
title(main = "1993-2017")
lines(n$x, n$y, col = 2)

## parametros - [escenario desfavorable]

a = 0.0036
b = 3.238

Linf = 18.63
k    = 0.68
to   = -0.27
Winf = a*Linf^b
aS = 11.57 # a selectividad
bS = 0.84  # b selectividad
aM = 21.01 # a madurez
bM = 1.88  # b madurez
edad = seq(1,4,by=1)
M_adultos     = 0.99
M_reclutas    = 1.08
M_prereclutas = 1.29
Ma = c(1.08, 0.99, 0.99) # mortalidad natural a la edad

obj_des <- YPR.fun(F_ = 0.2, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
                   to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM, approxim="i")

pro_des <- profile.YR(obj_des)


par(mfrow=c(1,2))
getPR(obj_des, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")
title(main = "1993-2017")
lines(n$x, n$y, col = 2)

pdf("PBR_anchoveta_reg_media_productividad.pdf", width = 16, height = 8, pointsize = 20)
par(mfrow=c(1,2))
getPR(obj_fav, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")
legend("toprigh",legend = "1993-2017", bty = "n")
lines(n$x, n$y, col = 2)
title(main = "favorable", outer = T, line = -2)

par(mfrow=c(1,2))
getPR(obj_neu, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")
legend("toprigh",legend = "1993-2017", bty = "n")
lines(n$x, n$y, col = 2)
title(main = "neutro", outer = T, line = -2)

par(mfrow=c(1,2))
getPR(obj_neudes, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")
legend("toprigh",legend = "1993-2017", bty = "n")
lines(n$x, n$y, col = 2)
title(main = "neutro desfavorable", outer = T, line = -2)
dev.off()
# -------------------------------------------------------------------------

## REGIMEN DE PRODUCTIVIDAD BAJA

# -------------------------------------------------------------------------
# RELACION S-R ------------------------------------------------------------
# -------------------------------------------------------------------------

yr0 = 1972
yr1 = 1993
data = dataR[dataR$year > yr0 & dataR$year < yr1, ]# ultimo regimen
st=data$ssb*1000
rt=data$rec
yr=data$year

# PENDIENTE ><> PARA ENCONTRAR Fmed

modRicker <-  SR.Fun("Ricker")
text(data$ssb*1000, data$rec, labels = round(data$year),pos = 3, cex = 0.8)

mFmed = modRicker$mFmed 


## YPR
## parametros - [escenario favorable] 

a = 0.0036
b = 3.238

Linf = 20.63
k    = 1
to   = -0.18
Winf = a*Linf^b
aS = 11.57 # a selectividad
bS = 0.84  # b selectividad
aM = 21.01 # a madurez
bM = 1.88  # b madurez
edad = seq(1,4,by=1)
M_adultos     = 0.83
M_reclutas    = 0.91
M_prereclutas = 1.29
Ma = c(0.91, 0.83, 0.83) # mortalidad natural a la edad
#Ma = c(1.29, 1.29, 0.91, 0.83, 0.83, 0.83,0.83, 0.83, 0.83)/2
obj_fav <- YPR.fun(F_ = 0.2, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
                   to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM, approxim="i")

PR_fav = getPR(obj_fav, Flab = "F", mFmed = mFmed)

par(mfrow=c(1,2))
getPR(obj_fav, Flab = "F", mFmed = mFmed)

SR.Fun("Ricker")
title(main = "1993-2017")
lines(n$x, n$y, col = 2)

## parametros - [escenario neutro] 

a = 0.0036
b = 3.238

Linf = 20.59
k    = 0.84
to   = -0.21
Winf = a*Linf^b
aS = 11.57 # a selectividad
bS = 0.84  # b selectividad
aM = 21.01 # a madurez
bM = 1.88  # b madurez
edad = seq(1,4,by=1)
M_adultos     = 0.83
M_reclutas    = 0.91
M_prereclutas = 1.29
Ma = c(0.91, 0.83, 0.83)  # mortalidad natural a la edad

obj_neu <- YPR.fun(F_ = 0.2, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
                   to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM, approxim="i")
pro_neu <- profile.YR(obj_neu)

par(mfrow=c(1,2))
getPR(obj_neu, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")
title(main = "1993-2017")
lines(n$x, n$y, col = 2)

## parametros - [escenario neutro desfavorable] 

a = 0.0036
b = 3.238

Linf = 20.37
k    = 0.73
to   = -0.24
Winf = a*Linf^b
aS = 11.57 # a selectividad
bS = 0.84  # b selectividad
aM = 21.01 # a madurez
bM = 1.88  # b madurez
edad = seq(1,4,by=1)
M_adultos     = 0.83
M_reclutas    = 0.91
M_prereclutas = 1.29
Ma = c(0.91, 0.83, 0.83)  # mortalidad natural a la edad

obj_neudes <- YPR.fun(F_ = 0.2, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
                      to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM, approxim="i")
pro_neudes <- profile.YR(obj_neudes)

par(mfrow=c(1,2))
getPR(obj_neudes, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")
title(main = "1993-2017")
lines(n$x, n$y, col = 2)

## parametros - [escenario desfavorable]

a = 0.0036
b = 3.238

Linf = 18.63
k    = 0.68
to   = -0.27
Winf = a*Linf^b
aS = 11.57 # a selectividad
bS = 0.84  # b selectividad
aM = 21.01 # a madurez
bM = 1.88  # b madurez
edad = seq(1,4,by=1)
M_adultos     = 0.99
M_reclutas    = 1.08
M_prereclutas = 1.29
Ma = c(1.08, 0.99, 0.99) # mortalidad natural a la edad

obj_des <- YPR.fun(F_ = 0.2, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
                   to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM, approxim="i")

pro_des <- profile.YR(obj_des)

par(mfrow=c(1,2))
getPR(obj_des, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")
title(main = "1993-2017")
lines(n$x, n$y, col = 2)

pdf("PBR_anchoveta_reg_baja_productividad.pdf", width = 16, height = 8, pointsize = 20)
par(mfrow=c(1,2))
getPR(obj_fav, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")
legend("toprigh",legend = "1993-2017", bty = "n")
lines(n$x, n$y, col = 2)
title(main = "favorable", outer = T, line = -2)

par(mfrow=c(1,2))
getPR(obj_neu, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")
legend("toprigh",legend = "1993-2017", bty = "n")
lines(n$x, n$y, col = 2)
title(main = "neutro", outer = T, line = -2)

par(mfrow=c(1,2))
getPR(obj_neudes, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")
legend("toprigh",legend = "1993-2017", bty = "n")
lines(n$x, n$y, col = 2)
title(main = "neutro desfavorable", outer = T, line = -2)
dev.off()

# -------------------------------------------------------------------------

## REGIMEN DE PRODUCTIVIDAD ALTA

# -------------------------------------------------------------------------
# RELACION S-R ------------------------------------------------------------
# -------------------------------------------------------------------------

yr0 = 1972
yr1 = 1993
data = dataR[dataR$year  <= yr0, ]# ultimo regimen
st=data$ssb*1000
rt=data$rec
yr=data$year

# PENDIENTE ><> PARA ENCONTRAR Fmed
modRicker <-  SR.Fun("Ricker")
text(data$ssb*1000, data$rec, labels = round(data$year),pos = 3, cex = 0.8)

mFmed = modRicker$mFmed 


## YPR
## parametros - [escenario favorable] 

a = 0.0036
b = 3.238

Linf = 20.63
k    = 1
to   = -0.18
Winf = a*Linf^b
aS = 11.57 # a selectividad
bS = 0.84  # b selectividad
aM = 21.01 # a madurez
bM = 1.88  # b madurez
edad = seq(1,4,by=1)
M_adultos     = 0.83
M_reclutas    = 0.91
M_prereclutas = 1.29
Ma = c(0.91, 0.83, 0.83) # mortalidad natural a la edad
#Ma = c(1.29, 1.29, 0.91, 0.83, 0.83, 0.83,0.83, 0.83, 0.83)/2
obj_fav <- YPR.fun(F_ = 0.2, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
                   to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM, approxim="i")

PR_fav = getPR(obj_fav, Flab = "F", mFmed = mFmed)

par(mfrow=c(1,2))
getPR(obj_fav, Flab = "F", mFmed = mFmed)

SR.Fun("Ricker")
title(main = "1993-2017")
lines(n$x, n$y, col = 2)

## parametros - [escenario neutro] 

a = 0.0036
b = 3.238

Linf = 20.59
k    = 0.84
to   = -0.21
Winf = a*Linf^b
aS = 11.57 # a selectividad
bS = 0.84  # b selectividad
aM = 21.01 # a madurez
bM = 1.88  # b madurez
edad = seq(1,4,by=1)
M_adultos     = 0.83
M_reclutas    = 0.91
M_prereclutas = 1.29
Ma = c(0.91, 0.83, 0.83)  # mortalidad natural a la edad

obj_neu <- YPR.fun(F_ = 0.2, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
                   to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM, approxim="i")
pro_neu <- profile.YR(obj_neu)

par(mfrow=c(1,2))
getPR(obj_neu, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")
title(main = "1993-2017")
lines(n$x, n$y, col = 2)

## parametros - [escenario neutro desfavorable] 

a = 0.0036
b = 3.238

Linf = 20.37
k    = 0.73
to   = -0.24
Winf = a*Linf^b
aS = 11.57 # a selectividad
bS = 0.84  # b selectividad
aM = 21.01 # a madurez
bM = 1.88  # b madurez
edad = seq(1,4,by=1)
M_adultos     = 0.83
M_reclutas    = 0.91
M_prereclutas = 1.29
Ma = c(0.91, 0.83, 0.83)  # mortalidad natural a la edad

obj_neudes <- YPR.fun(F_ = 0.2, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
                      to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM, approxim="i")
pro_neudes <- profile.YR(obj_neudes)

par(mfrow=c(1,2))
getPR(obj_neudes, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")
title(main = "1993-2017")
lines(n$x, n$y, col = 2)

## parametros - [escenario desfavorable]

a = 0.0036
b = 3.238

Linf = 18.63
k    = 0.68
to   = -0.27
Winf = a*Linf^b
aS = 11.57 # a selectividad
bS = 0.84  # b selectividad
aM = 21.01 # a madurez
bM = 1.88  # b madurez
edad = seq(1,4,by=1)
M_adultos     = 0.99
M_reclutas    = 1.08
M_prereclutas = 1.29
Ma = c(1.08, 0.99, 0.99) # mortalidad natural a la edad

obj_des <- YPR.fun(F_ = 0.2, Ma = Ma, edad = edad, a = a, b = b, Linf = Linf, k= k,
                   to  = to, Winf = Winf, aS = aS, bS = bS, aM = aM, bM = bM, approxim="i")

pro_des <- profile.YR(obj_des)

par(mfrow=c(1,2))
getPR(obj_des, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")
title(main = "1993-2017")
lines(n$x, n$y, col = 2)

pdf("PBR_anchoveta_reg_alta_productividad.pdf", width = 16, height = 8, pointsize = 20)
par(mfrow=c(1,2))
getPR(obj_fav, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")
legend("toprigh",legend = "1993-2017", bty = "n")
lines(n$x, n$y, col = 2)
title(main = "favorable", outer = T, line = -2)

par(mfrow=c(1,2))
getPR(obj_neu, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")
legend("toprigh",legend = "1993-2017", bty = "n")
lines(n$x, n$y, col = 2)
title(main = "neutro", outer = T, line = -2)

par(mfrow=c(1,2))
getPR(obj_neudes, Flab = "F", mFmed = mFmed)
SR.Fun("Ricker")
legend("toprigh",legend = "1993-2017", bty = "n")
lines(n$x, n$y, col = 2)
title(main = "neutro desfavorable", outer = T, line = -2)
dev.off()






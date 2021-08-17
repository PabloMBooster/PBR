require(ggplot2)
data <- read.csv(file = "data/output_SR_ricardoModel.csv")
names(data) <- c("year", "ssb", "rec" )

p <- ggplot(aes(ssb, rec), data=data) +
  geom_point() + geom_smooth(method="loess")

## ultimo regimen
yr = 1993
dataR = data[data$year >= yr, ]

st=dataR$ssb
rt=dataR$rec
yr=dataR$year

SR.Fun("Ricker")
SR.Fun("BevertonHolt")

plot(data$ssb*1000, data$rec, xaxt="n", yaxt="n", xlab = "sbb (miles ton)", ylab = "reclutas (millones)",
     ylim = c(0,max(data$rec)), xlim = c(0,max(data$ssb*1000)))
abline(h = 0)
axis(1)
axis(2)
n = locator(2)
lines(n$x, n$y, col = 2)
n$x[2]/n$y[2]#/1000)

lm(n$y~n$x)


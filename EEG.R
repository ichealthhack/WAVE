setwd("C:/Users/Loukia Petrou/Desktop/OpenVibe")
df <- read.csv("recordtrial4.csv")


argmax <- function(x, y, w=1, ...) {
  require(zoo)
  n <- length(y)
  y.smooth <- loess(y ~ x, ...)$fitted
  y.max <- rollapply(zoo(y.smooth), 2*w+1, max, align="center")
  delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max <- which(delta <= 0) + w
  list(x=x[i.max], i=i.max, y.hat=y.smooth)
}

test <- function(w, span) {
  peaks <- argmax(x, y, w=w, span=span)
  plot(x, y, cex=0.75, col="Gray", main=paste("w = ", w, ", span = ", span, sep=""))
  lines(x, peaks$y.hat,  lwd=2) #$
  y.min <- min(y)
  sapply(peaks$i, function(i) lines(c(x[i],x[i]), c(y.min, peaks$y.hat[i]),
                                    col="Red", lty=2))
  points(x[peaks$i], peaks$y.hat[peaks$i], col="Red", pch=19, cex=1.25)
  raw <- which(diff(sign(diff(df[,2])))==-2)+1
  attention <- which(diff(sign(diff(df[,3])))==-5)+1
  Meditation <- which(diff(sign(diff(df[,4])))==-5)+1
  raw.output <- print(df[raw,1])
  attention.output<-print(df[attention,1])
  Meditation.output <- print(df[Meditation,1])
  output <- list(raw.output, attention.output, Meditation.output)
  print(output)
  #x.name <- c("Time", "Peak")
  #colnames(df)<-x.name
}

#to print meditation type in console plot(df[,4])

# for attention type in console plot(df[,3])

x <- df[,1]
y <- df[,2]
par(mfrow=c(3,1))
test(65, 0.05)
plot(df[,3])
plot(df[,4])

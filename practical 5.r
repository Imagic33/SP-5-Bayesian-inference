library(rjags)
library(ggplot2)

y <-c(1,2,0,2,2,0,4,4,1,9,14,20,22,27,40,46,66,63,105,103,149,159,204,263,326,353,360,437,498,576,645,647,
      700,778,743,727,813,900,792,740,779,718,699,646,686,639,610,571,524,566,486,502,451,438,386,380,345,341,
      325,313,306,268,251,259,252,266,256,215,203,196,166,183,162,180,172,167,138,160,144,153,150,122,128,116,
      133,139,122,124,117,92,83,94,109,111,83,86,83,80,73,69)
y1 <- c(rep(0,20), y)
N <- length(y1)

B <- matrix(0,N,N)

for (i in 1:N) {
  for (j in 1:i) {
    B[i,j] <- dlnorm(i-j, 3.235,0.4147)
  }
}

mod <- jags.model("basic.jags",data=list(y=y1,N=N, B=B))

#sam <- jags.samples(mod,c("tau"),n.iter=10000)
#str(sam)

sam.coda <- coda.samples(mod,c("m","n","tau"),n.iter=100000)
crosscorr(sam.coda)
effectiveSize(sam.coda)
pdf("plot.pdf")

sam.coda[[1]]
?varnames
plot(sam.coda )
dev.off()
plot(sam.coda[,241,drop = FALSE])
sam.coda[1,4,drop = FALSE]

#  95% interval 
quat <- apply(sam.coda[[1]],2,quantile,prob=(c(0.025,.975)))
means <- apply(sam.coda[[1]],2,mean)

n_025 <- quat[1,121:240]
n_975 <- quat[2,121:240]
n_mean <- means[121:240]
m_mean <- means[1:120]
tau_95 <- quat$tau


data1<- as.data.frame(y1)

data1 <- cbind(date = as.Date(c(1:120), origin = "2020-2-11") ,
               data1,
               n_mean,
               n_025,
               n_975,
               m_mean)
row.names(data1) <- c(1:120)          

# plot
ggplot()+
  geom_line(data = data1, aes(date, y1, color = "death")) +
  geom_line(data = data1[1:100,], aes(date, n_mean,color = "n_mean"), size = 1) +
  geom_line(data = data1[1:100,], aes(date, n_025, color = "2.5% interval")) +
  geom_line(data = data1[1:100,], aes(date, n_975, color = "97.5% interval")) +
  geom_line(data = data1, aes(date, m_mean, color = "m_mean")) +
  geom_vline(data = data1[42,], aes(xintercept = date, )) +
  labs(x = "date", y = "population") + 
  annotate(geom = "text", x = as.Date("2020-3-30"), y = 2000, 
           label = "UK lockdown(2020-3-24)", hjust = 0) +
  annotate(geom = "line", 
           x = c(as.Date("2020-3-30"), as.Date("2020-3-24")), 
           y = c(2000, 2000), 
           arrow = arrow(angle = 30,length = unit(2,"mm")))

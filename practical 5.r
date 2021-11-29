library(rjags)
getwd()
setwd("C:/Users/Imagic/Desktop/Stats Programming/Practical 5")

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
sum(B)
mod <- jags.model("basic.jags",data=list(y=y1,N=N, B=B))

sam <- jags.samples(mod,c("tau"),n.iter=10000)
str(sam)
sam.coda <- coda.samples(mod,c("tau"),n.iter=10000)
plot(sam.coda)



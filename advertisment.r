library(rethinking)
ad = read.csv("Advertising.csv")
plot(ad$Sales ~ ad$Newspaper)


ad$T = standardize(ad$TV)
ad$R = standardize(ad$Radio)
ad$N = standardize(ad$Newspaper)
ad$S = standardize(ad$Sales)

m5.1 <- quap(
  alist(
    S ~ dnorm( mu , sigma ) ,
    mu <- t + bT * T ,
    t ~ dnorm( 0 , 0.2 ) ,
    bT ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = ad )


m5.2 <- quap(
  alist(
    S ~ dnorm( mu , sigma ) ,
    mu <- r + bR * R ,
    r ~ dnorm( 0 , 0.2 ) ,
    bR ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = ad )
precis(m5.2)

m5.3 <- quap(
  alist(
    S ~ dnorm( mu , sigma ) ,
    mu <- n + bN * N ,
    n ~ dnorm( 0 , 0.2 ) ,
    bN ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = ad )
precis(m5.3)


m5.4 <- quap(
  alist(
    S ~ dnorm( mu , sigma ) ,
    mu <- a + bN * N + bR * R + bT * T,
    a ~ dnorm( 0 , 0.2 ) ,
    bN ~ dnorm( 0 , 0.5 ) ,
    bR ~ dnorm( 0 , 0.5 ) ,
    bT ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = ad )
precis(m5.4)

plot( coeftab(m5.1,m5.2,m5.3, m5.4), par=c("bT","bR", "bN") )

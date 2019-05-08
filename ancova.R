library(tidyverse)
library(rethinking)

df<-read.csv("mathematics.csv", sep=";")

m1<-lm(math.perf~math.anx, data=df)

df %>% 
  ggplot(aes(math.anx, math.perf))+
  geom_point()+
  geom_abline(slope = m1$coefficients[2], intercept = m1$coefficients[1])







m2<-lm(math.perf~math.anx+gender, data=df)

df %>% 
  ggplot(aes(math.anx,math.perf, col=gender))+
  geom_point()+
  geom_abline(slope=m2$coefficients[2], intercept = m2$coefficients[1], col="red")+
  geom_abline(slope=m2$coefficients[2], intercept = (m2$coefficients[1]+m2$coefficients[3]), col="blue")




m3<-lm(math.perf~math.anx*gender, data=df)


df %>% 
  ggplot(aes(math.anx,math.perf, col=gender))+
  geom_point()+
  geom_abline(slope=m3$coefficients[2], intercept = m2$coefficients[1], col="red")+
  geom_abline(slope=(m3$coefficients[2]+m3$coefficients[4]), intercept = (m3$coefficients[1]+m3$coefficients[3]), col="blue")


######################################################################
####################################################################

df<-read.csv("mathematics.csv", sep=";")

df2 %>% 
  ggplot(aes(math.anx, math.perf))+
  geom_point()

df2<-df %>% 
  filter(gender=="F")

####standardizzo le variabili########
#df2$math.perf<-scale(df2$math.perf)
df2$math.anx<-scale(df2$math.anx)


##model  math.perf~Normal(mu, s) - likelihood
##       mu<-a+b*(math.anx-math.anx_bar) - linear model
## prior##
a<-rnorm(1e4, 0, 0.2)
dens(a)
b<-rnorm(1e4, 0, 0.5)
dens(b)
s<-dexp(1e4, 1)


###prior predictive simulation###
N<-100
a<-rnorm(N, 0, 0.2)
#b<-rnorm(N, 0, 5)
b<-rnorm(N, 0, 0.5)


plot(NULL, xlim=range(df2$math.anx), ylim=c(-3,3),
     xlab="anxiety", ylab="math score")
abline(h=-2.5, lty=2)
abline(h=2.5, lty=2)
anx.bar<-mean(df2$math.anx)

for(i in 1:N) curve(a[i]+b[i]*(x), 
                    from=min(df2$math.anx),to=max(df2$math.anx),
                    col=col.alpha("navy", 0.2), add = TRUE
                    )
#####posterior###

m1s<-map(
  alist(
    math.perf~dnorm(mu, s),
    mu<-a+b*(math.anx),
    a~dnorm(0, 0.2),
    b~dnorm(0,0.5),
    s~dexp(1)
  ), data=df2
)

#####posterior senza standardizzazione variabili######
m1<-map(
  alist(
    math.perf~dnorm(mu, s),
    mu<-a+b*(math.anx),
    a~dnorm(25,5),
    b~dnorm(0,1),
    s~dunif(0,50)
  ), data=df2
)



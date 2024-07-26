### Some simulations to look at two linear distributions

##starting with y=mx+b, no error
m<-1.5
b<-0
x<-rnorm(1000)
y<-m*x+b
y2<-0*x+b
plot.new()
plot(density(y), ylim=c(0, 1.75), xlim=c(-7,7),  col="red")
lines(density(y2))
mean(y)
mean(y2)
lm<-lm(y~x)
lm(y2~x)
t.test(y)
plot(x, y, col="red")
(lines(x, y2))




##### Smote-Technik verdeutlichen

# SMOTE-Technik verdeutlicht:
x<-rnorm(n=1000, mean=0, sd=1)
y<-x+rnorm(n=1000, mean=0, sd=1)

a<-rnorm(n=30, mean=1.5, sd=0.5)
b<-a+rnorm(n=30, mean=1.5, sd=0.5)

plot(x,y, pch=1, col="green", xlim=c(-3, 5), ylim=c(-4,5), xlab="x-Variable", ylab="y-Variable")
par(new=TRUE)
plot(a,b, pch=2, col="red", xlim=c(-3, 5), ylim=c(-4,5), xlab="x-Variable", ylab="y-Variable")
par(new=TRUE)
rect(0, 1, 3,5)

x<-rnorm(n=1000, mean=0, sd=1)
y<-x+rnorm(n=1000, mean=0, sd=1)

a<-rnorm(n=300, mean=1.5, sd=0.5)
b<-a+rnorm(n=300, mean=1.5, sd=0.5)

plot(x,y, pch=1, col="green", xlim=c(-3, 5), ylim=c(-4,5), xlab="x-Variable", ylab="y-Variable")
par(new=TRUE)
plot(a,b, pch=2, col="red", xlim=c(-3, 5), ylim=c(-4,5), xlab="x-Variable", ylab="y-Variable")
par(new=TRUE)
rect(0, 1, 3,5)

setwd("~/Desktop/Evolution/Tasks/Task_11")
x <- rnorm(100, mean=5, sd =2)
head(x)
y <- (x*5) +2 +rnorm(100, 0:0.1)
y
x
plot(x,y)
abline(x,y, col="blue")

# the slope is around -0.3185 and the y intercept ot arounf .1967.  This is because of the random numbers, and adding more will make these conclusions to change a little bit. 
z <- c()
x <- rnorm(100, mean=5, sd=2)
for (i in 1:100) 
	 z[i] <- rnorm(100, mean=5, sd=2) 
	 y <- (x * z[i]) + 2 + (rnorm(100, 0:0.1)) 
	 x <- (z[1:100]) 

plot(z[1:100])
abline(lm(y~z[1:100]))
plot(c(z, -.32))

install.packages("meme")
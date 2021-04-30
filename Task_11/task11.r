setwd("~/Desktop/Evolution/Tasks/Task_11")
x <- rnorm(100, mean=5, sd =2)
head(x)
y <- (x*5) +2 +rnorm(100, 0:0.1)
y
x
plot(x,y)
abline(x,y, col="blue")
coef(lm(y~x))

# the slope is around 2.335 and the y intercept ot arounf 4.958.  This is because of the random numbers, and adding more will make these conclusions to change a little bit. 
z <- c()
x <- rnorm(100, mean=5, sd=2)
for (i in 1:100) {
	 z[i] <- runif(1)
	 y <- (x*z[i]) + 2 + (rnorm(100, 0:0.1)) 
	 l <- coef(lm(z[1:100]~y))
	 }
l
plot(z[1:100],y)
abline(lm(y~z[1:100]))
plot(c(z, -0.029))
abline(lm(y~z[1:100]))
plot(c(z, -0.029))
install.packages("meme")
library("meme")

K <-
'https://www.google.com/imgres?imgurl=https%3A%2F%2Fwww.sunset.com%2Fwp-content%2Fuploads%2FPLANT-MEME-BEAN-0120-1280x720.jpg&imgrefurl=https%3A%2F%2Fwww.sunset.com%2Fhome-garden%2Fplants%2Fplant-memes-instagram&tbnid=wJHde2f0jx70AM&vet=12ahUKEwjztYqflqXwAhVBbK0KHSzRCv4QMygKegUIARCkAQ..i&docid=Ed-N89qFDZdKPM&w=1280&h=720&q=memes&ved=2ahUKEwjztYqflqXwAhVBbK0KHSzRCv4QMygKegUIARCkAQ'\
Katiememe <- (K, lower="One doesn't grow without nutrients", color="blue", size="1.0")

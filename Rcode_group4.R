#####################################
#      Mathematical Statistics      #
#                                   #
#              GROUP 4              #
#        Margot POUPONNEAU          #
#         Yangjiawei XUE            #
#       Ilyass EL KANSOULI          #
#         Lucas MONTEIRO            #
#                                   #
#             R CODE                #
#####################################


install.packages("tidytext")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("readr")
install.packages("tidyr")
install.packages("fitdistrplus")
install.packages("distr")
install.packages("DescTools")
install.packages("EnvStats")
install.packages("e1071")
install.packages("HDInterval")

library("tidytext")
library("tidyverse")
library("dplyr")
library("readr")
library("tidyr")
library("fitdistrplus")
library("distr")
library("DescTools")
library("EnvStats")
library("e1071")
library("HDInterval")

blogtext <- read.csv("blogtext.csv", sep = ",", header = TRUE)
blogtext_age <- blogtext[ ,c("age","text") ]

# one document = between 20,000 and 40,000 words
bt1 <- tibble(blogtext_age[1:100, ] )     
bt2 <- tibble(blogtext_age[101:200, ] )
bt3 <- tibble(blogtext_age[201:300, ] )
bt4 <- tibble(blogtext_age[301:400, ] )
bt5 <- tibble(blogtext_age[401:500, ] )
bt6 <- tibble(blogtext_age[501:600, ] )
bt7 <- tibble(blogtext_age[601:700, ] )
bt8 <- tibble(blogtext_age[701:800, ] )
bt9 <- tibble(blogtext_age[801:900, ] )
bt10 <- tibble(blogtext_age[901:1000, ] )
bt11<- tibble(blogtext_age[1001:1100, ] )
bt12<- tibble(blogtext_age[1101:1200, ] )
bt13<- tibble(blogtext_age[1201:1300, ] )
bt14<- tibble(blogtext_age[1301:1400, ] )
bt15<- tibble(blogtext_age[1401:1800, ] )
bt16<- tibble(blogtext_age[1801:2100, ] )
bt17<- tibble(blogtext_age[2101:2500, ] )
bt18<- tibble(blogtext_age[2501:3100, ] )

#we break each document to have a new frame whose rows correspond to word in the document
bt1_unnest <- bt1 %>% unnest_tokens(word,text)  
bt2_unnest <- bt2 %>% unnest_tokens(word,text)  
bt3_unnest <- bt3 %>% unnest_tokens(word,text)
bt4_unnest <- bt4 %>% unnest_tokens(word,text)
bt5_unnest <- bt5 %>% unnest_tokens(word,text)
bt6_unnest <- bt6 %>% unnest_tokens(word,text)
bt7_unnest <- bt7 %>% unnest_tokens(word,text)
bt8_unnest <- bt8 %>% unnest_tokens(word,text)
bt9_unnest <- bt9 %>% unnest_tokens(word,text)
bt10_unnest <- bt10 %>% unnest_tokens(word,text)
bt11_unnest <- bt11 %>% unnest_tokens(word,text)
bt12_unnest <- bt12 %>% unnest_tokens(word,text)
bt13_unnest <- bt13 %>% unnest_tokens(word,text)
bt14_unnest <- bt14 %>% unnest_tokens(word,text)
bt15_unnest <- bt15 %>% unnest_tokens(word,text)
bt16_unnest <- bt16 %>% unnest_tokens(word,text)
bt17_unnest <- bt17 %>% unnest_tokens(word,text)
bt18_unnest <- bt18 %>% unnest_tokens(word,text)


#count() create a new dataframe that count the occurrence of each word in the document
bt1_count <- bt1_unnest %>%  count(word, sort = TRUE)
bt2_count <- bt2_unnest %>%  count(word, sort = TRUE)
bt3_count <- bt3_unnest %>%  count(word, sort = TRUE)
bt4_count <- bt4_unnest %>%  count(word, sort = TRUE)
bt5_count <- bt5_unnest %>%  count(word, sort = TRUE)
bt6_count <- bt6_unnest %>%  count(word, sort = TRUE)
bt7_count <- bt7_unnest %>%  count(word, sort = TRUE)
bt8_count <- bt8_unnest %>%  count(word, sort = TRUE)
bt9_count <- bt9_unnest %>%  count(word, sort = TRUE)
bt10_count <- bt10_unnest %>%  count(word, sort = TRUE)
bt11_count <- bt11_unnest %>%  count(word, sort = TRUE)
bt12_count <- bt12_unnest %>%  count(word, sort = TRUE)
bt13_count <- bt13_unnest %>%  count(word, sort = TRUE)
bt14_count <- bt14_unnest %>%  count(word, sort = TRUE)
bt15_count <- bt15_unnest %>%  count(word, sort = TRUE)
bt16_count <- bt16_unnest %>%  count(word, sort = TRUE)
bt17_count <- bt17_unnest %>%  count(word, sort = TRUE)
bt18_count <- bt18_unnest %>%  count(word, sort = TRUE)

QUESTION 1
#GRAPHS

#POISSON

par(mfrow = c(1,3))

w <- seq(0,30,1)
plot(x = w, dpois(w,1), col = 'blue',lwd = 2,type = 'h' ,ylab = "Poisson(k)", xlab = "", main = "", xlim = c(0,35))
legend(x = "topright",legend = c("lambda = 1"), col = c("blue"),lwd = 2)  

plot(x = w, dpois(w,5), col = 'darkgoldenrod1',lwd = 2,type = 'h' ,ylab = "Poisson(k)", xlab = "", main = "", xlim = c(0,35))
legend(x = "topright",legend = c("lambda = 5"), col = c("darkgoldenrod1"),lwd = 2)  

plot(x = w, dpois(w,15), col = 'purple', pch = 20,lwd = 2,type = 'h' ,ylab = "Poisson(k)", xlab = "", main = "", xlim = c(0,35))
legend(x = "topright",legend = c("lambda = 15"), col = c("purple"),lwd = 2)

#NEGATIVE BINOMIAL

#MOVE MU
par(mfrow = c(1,3))

w <- seq(0,35,1)
plot(x = w, dnbinom(w,size=10,mu=2), col = 'black',lwd = 2,type = 'h' ,ylab = "NB(k)", xlab = "", main = "size = 10", xlim = c(0,35))
legend(x = "topright",legend = c("mu = 2"), col = c("black"),lwd = 2)  

plot(x = w, dnbinom(w,size=10,mu=10), col = 'green',lwd = 2,type = 'h' ,ylab = "NB(k)", xlab = "", main = "size = 10", xlim = c(0,35))
legend(x = "topright",legend = c("mu = 10"), col = c("green"),lwd = 2)  

plot(x = w, dnbinom(w,size=10,mu=20), col = 'blue', pch = 20,lwd = 2,type = 'h' ,ylab = "NB(k)", xlab = "", main = "size = 10", xlim = c(0,35))
legend(x = "topright",legend = c("mu = 20"), col = c("blue"),lwd = 2)

#MOVE SIZE
par(mfrow = c(1,3))

w <- seq(0,35,1)
plot(x = w, dnbinom(w,size=5,mu=10), col = 'black',lwd = 2,type = 'h' ,ylab = "NB(k)", xlab = "", main = "mu = 10", xlim = c(0,35))
legend(x = "topright",legend = c("size = 5"), col = c("black"),lwd = 2)  

plot(x = w, dnbinom(w,size=10,mu=10), col = 'green',lwd = 2,type = 'h' ,ylab = "NB(k)", xlab = "", main = "mu = 10", xlim = c(0,35))
legend(x = "topright",legend = c("size = 10"), col = c("green"),lwd = 2)  

plot(x = w, dnbinom(w,size=20,mu=10), col = 'blue', pch = 20,lwd = 2,type = 'h' ,ylab = "NB(k)", xlab = "", main = "mu = 10", xlim = c(0,35))
legend(x = "topright",legend = c("size = 20"), col = c("blue"),lwd = 2)



#2-POISSON
#A useful function to compute probabilities with 2-Poisson distribution
poiss_2_est <- function(n , alpha , lambda1 , lambda2){
  se <- seq(0,n,1)
  alpha*dpois(se,lambda1,log = FALSE) + (1-alpha)*dpois(se,lambda2,log=FALSE)
}
par(mfrow = c(1,3))


plot(x = seq(0,35,1), poiss_2_est(35, 0.5, 5, 1), col = 'darkgreen', pch = 20,lwd = 2,type = 'h' ,ylab = "2-Poisson(k)", xlab = "", main = "lambda2 = 5, alpha = 0.5", xlim = c(0,35))
legend(x = "topright",legend = c("lambda1 = 1"), col = c("darkgreen"),lwd = 2)  

plot(x = seq(0,35,1), poiss_2_est(35, 0.5, 5, 15), col = 'red', pch = 20,lwd = 2,type = 'h' ,ylab = "2-Poisson(k)", xlab = "", main = "", xlim = c(0,35))
legend(x = "topright",legend = c("lambda1 = 15"), col = c("red"),lwd = 2)  

plot(x = seq(0,35,1), poiss_2_est(35, 0.5, 5, 25), col = 'blue', pch = 20,lwd = 2,type = 'h' ,ylab = "2-Poisson(k)", xlab = "", main = "", xlim = c(0,35))
legend(x = "topright",legend = c("lambda1 = 25"), col = c("blue"),lwd = 2)  



#MOVE ALPHA
par(mfrow = c(1,2))


plot(x = seq(0,35,1), poiss_2_est(35, 0.2, 5, 20), col = 'black', pch = 20,lwd = 2,type = 'h' ,ylab = "2-Poisson(k)", xlab = "", main = "lambda1 = 5, lambda2 = 20", xlim = c(0,35))
legend(x = "topright",legend = c("alpha = 0.2"), col = c("black"),lwd = 2)  

plot(x = seq(0,35,1), poiss_2_est(35, 0.8, 5, 20), col = 'red', pch = 20,lwd = 2,type = 'h' ,ylab = "2-Poisson(k)", xlab = "", main = "lambda1 = 5, lambda2 = 20", xlim = c(0,35))
legend(x = "topright",legend = c("alpha = 0.8"), col = c("red"),lwd = 2)  


#K-MIXTURE
#A useful function to compute probabilities with K-Mixture distribution
katz_est <- function(n, alpha, beta){
  se <- seq(0,n,1)
  ifelse((se>0),(alpha/(beta + 1))*((beta/(beta + 1))^se),(1 - alpha))
}

#MOVE ALPHA 

par(mfrow = c(1,3))

w <- seq(0,30,1)
plot(w,katz_est(30,0.2,5), col = 'red', pch = 20, type = 'h',lwd = 2, ylab = "Katz(k)", xlab = "k", main = "beta constant at 5")
legend(x = "topright", legend = c("alpha = 0.2"), lty = 1,col = c("red"),lwd = 2)  
plot(w,katz_est(30,0.5,5), col = 'blue', pch = 20, type = 'h',lwd = 2, ylab = "Katz(k)", xlab = "k", main = "beta constant at 5")
legend(x = "topright", legend = c("alpha = 0.5"), lty = 1,col = c("blue"),lwd = 2)  
plot(w,katz_est(30,0.9,5), col = 'chocolate3', pch = 20, type = 'h',lwd = 2, ylab = "Katz(k)", xlab = "k", main = "beta constant at 5")
legend(x = "topright", legend = c("alpha = 0.9"), lty = 1,col = c("chocolate3"),lwd = 2)


#MOVE BETA

par(mfrow = c(1,3))

plot(w,katz_est(30,0.9,1), col = 'orange', pch = 20, type = 'h',lwd = 2, ylab = "Katz(k)", xlab = "k", main = "alpha constant at 0.9")
legend(x = "topright", legend = c("beta = 1"), lty = 1,col = c("orange"),lwd = 2)  
plot(w,katz_est(30,0.9,10), col = 'aquamarine3', pch = 20, type = 'h',lwd = 2, ylab = "Katz(k)", xlab = "k", main = "alpha constant at 0.9")
legend(x = "topright", legend = c("beta = 10"), lty = 1,col = c("aquamarine3"),lwd = 2)  
plot(w,katz_est(30,0.9,30), col = 'purple', pch = 20, type = 'h',lwd = 2, ylab = "Katz(k)", xlab = "k", main = "alpha constant at 0.9")
legend(x = "topright", legend = c("beta = 30"), lty = 1,col = c("purple"),lwd = 2)  


par(mfrow = c(3,1))


# QUESTION 3


#Step 1 : choose words

# We choose "amazing" and "investment" : one common word and one specialized word

#AMAZING
#We take, for each document, the number of occurrences of amazing
#If there is no "amazing" in a document, the value will be missing, so we have to write 0 by hand
amazing_sample <- c(bt1_count$n[bt1_count$word == "amazing"],
                    bt2_count$n[bt2_count$word == "amazing"],
                    bt3_count$n[bt3_count$word == "amazing"],
                    bt4_count$n[bt4_count$word == "amazing"],
                    bt5_count$n[bt5_count$word == "amazing"],
                    bt6_count$n[bt6_count$word == "amazing"],
                    bt7_count$n[bt7_count$word == "amazing"],
                    bt8_count$n[bt8_count$word == "amazing"],
                    bt9_count$n[bt9_count$word == "amazing"],
                    bt10_count$n[bt10_count$word == "amazing"],
                    bt11_count$n[bt11_count$word == "amazing"],
                    bt12_count$n[bt12_count$word == "amazing"],
                    bt13_count$n[bt13_count$word == "amazing"],
                    bt14_count$n[bt14_count$word == "amazing"],
                    bt15_count$n[bt15_count$word == "amazing"],
                    0,
                    bt17_count$n[bt17_count$word == "amazing"],
                    bt18_count$n[bt18_count$word == "amazing"])

amazing_sample

#Some statistics about the sample
mean(amazing_sample)
sd(amazing_sample)^2

#This graph shows the number of occurrences for each document
plot(x = amazing_sample, pch = 20)

#This graph shows how our sample is distributed, we can see which value appears the most
plotdist(amazing_sample,  discrete = TRUE)


#We will compare those values to different estimated values proposed by the 4 distributions

#To plot the density function and cumulative distribution of our sample
amazing_dist <- demp(x = seq(0,20,1), obs = amazing_sample, discrete = TRUE)
par(mfrow=c(1,2))
plot(x = seq(0,20,1), amazing_dist, type  = 'h')
plot(cumsum(amazing_dist))



#Step 2 : estimate the parameters of studied distributions (poisson, neg-binomial, 2-poisson, k-mixture)

#With fitdist, we fit our sample for both poisson and neg-binomial distributions

# Negative binomial first
nbin_amazing <- fitdist(amazing_sample, "nbinom")
# Then Poisson
poiss_amazing <- fitdist(amazing_sample, "pois")


# We obtain estimates for the 2 distributions

#Here we have the estimated values for the poisson distribution
poiss_amazing_est <- dpois(seq(0,20,1),poiss_amazing$estimate, log = FALSE)
poiss_amazing_est

#Here we have the estimated values for the neg-binomial distribution
nbin_amazing_est <- dnbinom(seq(0,20,1), size = nbin_amazing$estimate[1]   , mu = nbin_amazing$estimate[2] , log = FALSE)
nbin_amazing_est


#2-Poisson

# We estimate the parameters of 2-poisson distribution
#    We can use the moment function of the package "e1071"



#this function gives the parameters lambda 1, lambda 2, alpha

poiss_2_coef <- function(data){
  
  #    We compute the 3 first moments
  R1 <- moment(data, order=1, center=FALSE, absolute=FALSE, na.rm=FALSE)
  R2 <- moment(data, order=2, center=FALSE, absolute=FALSE, na.rm=FALSE)
  R3 <- moment(data, order=3, center=FALSE, absolute=FALSE, na.rm=FALSE)
  
  a <- R1^2 + R1 - R2
  b <- R1^2 - R1*R2 + 2*R1 - 3*R2 + R3
  c <-  R2^2 - R1^2 + R1*R2 - R1*R3
  
  alpha <- ifelse((Re(polyroot(c(c,b,a))[2]) > Re(polyroot(c(c,b,a))[1])),
                  ((R1 - polyroot(c(c,b,a))[1])/(polyroot(c(c,b,a))[2] - polyroot(c(c,b,a))[1])),
                  ((R1 - polyroot(c(c,b,a))[2])/(polyroot(c(c,b,a))[1] - polyroot(c(c,b,a))[2])))
  
  print(c(polyroot(c(c,b,a))[1],
          polyroot(c(c,b,a))[2],
          alpha)
  )
}

#estimated parameters
# alpha  is the last parameter given by this function
ama_coef_2poiss <- poiss_2_coef(amazing_sample)
ama_coef_2poiss

#we get estimated probabilities for k=0, k=1, ... , k=20 with the 2-poiss
poiss_2_ama <- poiss_2_est(n = 20, alpha = Re(ama_coef_2poiss[3]), lambda1 = Re(ama_coef_2poiss[2]), lambda2 = Re(ama_coef_2poiss[1]))
# We take lambda1 as the largest coefficient estimated
#K-mixture

katz_coef <- function(data){
  
  #Beta^ = Burstiness (empirical) - 1
  beta <- mean(data)/(((length(data)- sum(data == 0))) /length(data)) - 1
  
  #Alpha^ = mean (empirical) / beta^
  alpha <- mean(data)/beta
  
  print(c(alpha,beta))
}

#first element is alpha, second one is beta

katz_coef_ama <- katz_coef(amazing_sample)

# PROBLEM : ALPHA IS OUT OF RANGE [0,1]

#We use these 2 estimated parameters to find estimated values by katz mixture

#this function gives estimated probabilities for k=0, k=1 , ... , k=n with Katz mixture distribution
#n is the maximum number of occurrence in our sample.


#For amazing, this value is 19 but we can set n = 20

katz_est_ama <- katz_est(20, alpha = katz_coef_ama[1], beta = katz_coef_ama[2])

#So here we have our estimations of k-mixture for k=0, k=1, k=2, ... , k=20
# SINCE ALPHA > 1 , P(k=0) < 0 !!!!!!!!!!!!!!!!!! BIG PROBLEM HERE

par(mfrow = c(1,2))
plot(y = amazing_dist, x = seq(0,20,1), type = 'h', ylim = c(-0.3,0.3), main = 'Densities',
     xlab = 'Data', ylab = 'Densities', las = 1)
lines(x = seq(0,20,1)+0.1, y = poiss_2_ama, col = 'purple', pch = 20,
      ylab = "2-Poisson(k)", xlab = "k", main = "", type = 'h', lty = 1)
lines(x = seq(0,20,1)+0.2, y = katz_est_ama, col = 'aquamarine3', pch = 20,
      ylab = "Katz(k)", xlab = "k", main = "", type = 'h', lty = 3)
legend(x = "topright",          # Position
       legend = c("empirical", "2-Poisson", "K-mixture"),  # Legend texts
       lty = c(1,1,3),           # Line types
       col = c("black","purple","aquamarine3"),           # Line colors
       lwd = 2)  


plot(ecdf(amazing_sample),ylim = c(0,1), main = 'Cumulative Distribution Function',
     xlab = 'Data', ylab = 'CDF', las = 1)
lines(cumsum(y = poiss_2_ama), col = 'purple', type = 's', ylim = c(0,1),
      ylab = "2-Poisson(k)", xlab = "k", main = "" )
lines(cumsum(y = katz_est_ama), col = 'aquamarine3', type = 's', ylim = c(0,1),
      ylab = "Katz(k)", xlab = "k", main = "" , lty = 3)
legend(x = "right",          # Position
       legend = c("empirical", "2-Poisson", "K-mixture"),  # Legend texts
       lty = c(1,1,3),           # Line types
       col = c("black","purple","aquamarine3"),           # Line colors
       lwd = 2)  


cdf_2poiss_ama <- cumsum(poiss_2_ama)
x <- seq(0,20,1)
X_2poiss_ama <- cbind(x, cdf_2poiss_ama)

#this function gives the quantile
my_quantile <- function(x, prob) {
  if (is.unsorted(x)) x <- sort(x)
  n <- length(x)
  approx(seq(0, 1, length = n), x, prob)$y
}

quant_2poiss_ama <- my_quantile(X_2poiss_ama, prob = seq(0,1,0.05))

#same for k-mixture
cdf_katz_ama <- cumsum(katz_est_ama)
x <- seq(0,20,1)
X_katz_ama <- cbind(x, katz_est_ama)

quant_katz_ama <- my_quantile(X_katz_ama, prob = seq(0,1,0.05))

quant_ama <- quantile(amazing_sample, p = seq(0,1,0.05))

cdf_ama <- cumsum(amazing_dist)


par(mfrow = c(2, 2))

#poisson and neg-binomial
plot.legend <- c("nbin", "pois")
denscomp(list(nbin_amazing, poiss_amazing), legendtext = plot.legend)
qqcomp(list(nbin_amazing, poiss_amazing), legendtext = plot.legend)
cdfcomp(list(nbin_amazing, poiss_amazing), legendtext = plot.legend)
ppcomp(list(nbin_amazing, poiss_amazing), legendtext = plot.legend)

par(mfrow = c(2, 2))

#2-poisson and k-mixture
plot(y = amazing_dist, x = seq(0,20,1), type = 'h', ylim = c(-0.3,0.3), main = 'Densities',
     xlab = 'Data', ylab = 'Densities', las = 1)
lines(x = seq(0,20,1)+0.1, y = poiss_2_ama, col = 'purple', pch = 20,
      ylab = "2-Poisson(k)", xlab = "k", main = "", type = 'h', lty = 1)
lines(x = seq(0,20,1)+0.2, y = katz_est_ama, col = 'aquamarine3', pch = 20,
      ylab = "Katz(k)", xlab = "k", main = "", type = 'h', lty = 3)
legend(x = "topright",          # Position
       legend = c("empirical", "2-Poisson", "K-mixture"),  # Legend texts
       lty = c(1,1,3),           # Line types
       col = c("black","purple","aquamarine3"),           # Line colors
       lwd = 2)  

plot(y = quant_ama, x = quant_2poiss_ama, ylab = 'Empirical quantiles', xlab = 'Theorical quantiles',
     main = 'Q-Q plot', col = 'purple', pch = 17)
points(y = quant_ama, x = quant_katz_ama, pch = 8, col = 'aquamarine3')
abline(0, 1)
legend(x = "topleft",          # Position
       legend = c("2-Poisson", "K-mixture"),           # Line types
       col = c("purple","aquamarine3"),           # Line colors
       lwd = 2, pch=c(17,8), lty=c(NA,NA))  

plot(ecdf(amazing_sample),ylim = c(0,1), main = 'Cumulative Distribution Function',
     xlab = 'Data', ylab = 'CDF', las = 1)
lines(cumsum(y = poiss_2_ama), col = 'purple', type = 's', ylim = c(0,1),
      ylab = "2-Poisson(k)", xlab = "k", main = "" )
lines(cumsum(y = katz_est_ama), col = 'aquamarine3', type = 's', ylim = c(0,1),
      ylab = "Katz(k)", xlab = "k", main = "" , lty = 3)
legend(x = "right",          # Position
       legend = c("empirical", "2-Poisson", "K-mixture"),  # Legend texts
       lty = c(1,1,3),           # Line types
       col = c("black","purple","aquamarine3"),           # Line colors
       lwd = 2)  

qqplot(cdf_ama,cdf_2poiss_ama, xlim = c(0,1), ylim = c(0,1), col = 'purple', pch = 17,
       ylab = 'Empirical probabilities', xlab = 'Theorical probabilities',
       main = 'P-P plot')
points(y = cdf_ama, x = cdf_katz_ama, pch = 8, col = 'aquamarine3')
abline(0, 1)
legend(x = "topleft",          # Position
       legend = c("2-Poisson", "K-mixture"),           # Line types
       col = c("purple","aquamarine3"),           # Line colors
       lwd = 2, pch=c(17,8), lty=c(NA,NA)) 

#INVESTMENT

#We do the same for the word "investment"
bt1_count$n[bt1_count$word == "investment"]
bt2_count$n[bt2_count$word == "investment"]
bt3_count$n[bt3_count$word == "investment"]
bt4_count$n[bt4_count$word == "investment"]
bt5_count$n[bt5_count$word == "investment"]
bt6_count$n[bt6_count$word == "investment"]
bt7_count$n[bt7_count$word == "investment"]
bt8_count$n[bt8_count$word == "investment"]
bt9_count$n[bt9_count$word == "investment"]
bt10_count$n[bt10_count$word == "investment"]
bt11_count$n[bt11_count$word == "investment"]
bt12_count$n[bt12_count$word == "investment"]
bt13_count$n[bt13_count$word == "investment"]
bt14_count$n[bt14_count$word == "investment"]
bt15_count$n[bt15_count$word == "investment"]
bt16_count$n[bt16_count$word == "investment"]
bt17_count$n[bt17_count$word == "investment"]
bt18_count$n[bt18_count$word == "investment"]

invest_sample <- c(bt1_count$n[bt1_count$word == "investment"],
                   0,
                   0,
                   bt4_count$n[bt4_count$word == "investment"],
                   0,
                   0,
                   0,
                   0,
                   bt9_count$n[bt9_count$word == "investment"],
                   bt10_count$n[bt10_count$word == "investment"],
                   0,
                   0,
                   0,
                   0,
                   0,
                   bt16_count$n[bt16_count$word == "investment"],
                   0,
                   0)

invest_sample

#Some statistics about the sample
mean(invest_sample)
sd(invest_sample)^2

#This graph shows the number of occurrences for each document
plot(x = invest_sample, pch = 20)

#This graph shows how our sample is distributed, we can see which value appears the most
plotdist(invest_sample,  discrete = TRUE)


#This gives values of the density of our sample
#(we need to be careful : if we change the number of documents
# -> we need to change x and take larger value,
# for instance seq(0,50,1) if we have something like 45-50 documents)

#We will compare those values to different estimated values proposed by the 4 distributions

#To plot the density function and cumulative distribution of our sample
inv_dist <- demp(x = seq(0,25,1), obs = invest_sample, discrete = TRUE)
par(mfrow=c(1,2))
plot(x = seq(0,25,1), inv_dist, type  = 'h')



#Step 2 : estimate the parameters of studied distributions (poisson, neg-binomial, 2-poisson, k-mixture)

#With fitdist, we fit our sample for both poisson and neg-binomial distributions

# Negative binomial first
nbin_inv <- fitdist(invest_sample, "nbinom")
# Then Poisson
poiss_inv <- fitdist(invest_sample, "pois")


# We obtain estimates for the 2 distributions

#Here we have the estimated values for the poisson distribution
poiss_inv_est <- dpois(seq(0,25,1),poiss_inv$estimate, log = FALSE)
poiss_inv_est

#Here we have the estimated values for the neg-binomial distribution
nbin_inv_est <- dnbinom(seq(0,25,1), size = nbin_inv$estimate[1]   , mu = nbin_inv$estimate[2] , log = FALSE)
nbin_inv_est


#2-Poisson

# We estimate the parameters of 2-poisson distribution

#estimated parameters
# alpha  is the last parameter given by this function
inv_coef_2poiss <- poiss_2_coef(invest_sample)
inv_coef_2poiss


#we get estimated probabilities for k=0, k=1, ... , k=20 with the 2-poiss
poiss_2_inv <- poiss_2_est(n = 25, alpha = Re(inv_coef_2poiss[3]), lambda1 = Re(inv_coef_2poiss[2]), lambda2 = Re(inv_coef_2poiss[1]))
# We take lambda1 as the largest coefficient estimated


katz_coef_inv <- katz_coef(invest_sample)

# this time, alpha is in range [0,1]

#We use these 2 estimated parameters to find estimated values by katz mixture

katz_est_inv <- katz_est(25, alpha = katz_coef_inv[1], beta = katz_coef_inv[2])

#So here we have our estimations of k-mixture for k=0, k=1, k=2, ... , k=20


cdf_2poiss_inv <- cumsum(poiss_2_inv)
xb <- seq(0,25,1)
X_2poiss_inv <- cbind(xb, cdf_2poiss_inv)

quant_2poiss_inv <- my_quantile(X_2poiss_inv, prob = seq(0,1,0.05))

#same for k-mixture
cdf_katz_inv <- cumsum(katz_est_inv)
X_katz_inv <- cbind(xb, katz_est_inv)

quant_katz_inv <- my_quantile(X_katz_inv, prob = seq(0,1,0.05))

quant_inv <- quantile(invest_sample, p = seq(0,1,0.05))

cdf_inv <- cumsum(inv_dist)


par(mfrow = c(2, 2))

#poisson and neg-binomial
denscomp(list(nbin_inv, poiss_inv), legendtext = plot.legend)
qqcomp(list(nbin_inv, poiss_inv), legendtext = plot.legend)
cdfcomp(list(nbin_inv, poiss_inv), legendtext = plot.legend)
ppcomp(list(nbin_inv, poiss_inv), legendtext = plot.legend)

par(mfrow = c(2, 2))

#2-poisson and k-mixture
plot(y = inv_dist, x = seq(0,25,1), type = 'h', main = 'Densities', ylim = c(0,1),
     xlab = 'Data', ylab = 'Densities', las = 1)
lines(x = seq(0,25,1)+0.1, y = poiss_2_inv, col = 'purple', pch = 20,
      ylab = "2-Poisson(k)", xlab = "k", main = "", type = 'h', lty = 1)
lines(x = seq(0,25,1)+0.2, y = katz_est_inv, col = 'aquamarine3', pch = 20,
      ylab = "Katz(k)", xlab = "k", main = "", type = 'h', lty = 3)
legend(x = "topright",          # Position
       legend = c("empirical", "2-Poisson", "K-mixture"),  # Legend texts
       lty = c(1,1,3),           # Line types
       col = c("black","purple","aquamarine3"),           # Line colors
       lwd = 2)  

plot(y = quant_inv, x = quant_2poiss_inv, ylab = 'Empirical quantiles', xlab = 'Theorical quantiles',
     main = 'Q-Q plot', col = 'purple', pch = 17)
points(y = quant_inv, x = quant_katz_inv, pch = 8, col = 'aquamarine3')
abline(0, 1)
legend(x = "topleft",          # Position
       legend = c("2-Poisson", "K-mixture"),           # Line types
       col = c("purple","aquamarine3"),           # Line colors
       lwd = 2, pch=c(17,8), lty=c(NA,NA))  

plot(ecdf(invest_sample),ylim = c(0,1), main = 'Cumulative Distribution Function',
     xlab = 'Data', ylab = 'CDF', las = 1)
lines(cumsum(y = poiss_2_inv), col = 'purple', type = 's', ylim = c(0,1),
      ylab = "2-Poisson(k)", xlab = "k", main = "" )
lines(cumsum(y = katz_est_inv), col = 'aquamarine3', type = 's', ylim = c(0,1),
      ylab = "Katz(k)", xlab = "k", main = "" , lty = 3)
legend(x = "right",          # Position
       legend = c("empirical", "2-Poisson", "K-mixture"),  # Legend texts
       lty = c(1,1,3),           # Line types
       col = c("black","purple","aquamarine3"),           # Line colors
       lwd = 2)  

qqplot(cdf_inv,cdf_2poiss_inv, xlim = c(0,1), ylim = c(0,1), col = 'purple', pch = 17,
       ylab = 'Empirical probabilities', xlab = 'Theorical probabilities',
       main = 'P-P plot')
points(y = cdf_inv, x = cdf_katz_inv, pch = 8, col = 'aquamarine3')
abline(0, 1)
legend(x = "topleft",          # Position
       legend = c("2-Poisson", "K-mixture"),           # Line types
       col = c("purple","aquamarine3"),           # Line colors
       lwd = 2, pch=c(17,8), lty=c(NA,NA)) 

# QUESTION 4
#We choose 5 common words : amazing, bad, love, good, interesting. It's a blog so sentiment #word are more common
#And 5 specialized word : investment, war, construction, software, animal. These words #characterize particular subject 


#COMMON WORDS


#LOVE
love_sample <- c(
  bt1_count$n[bt1_count$word == "love"],
  bt2_count$n[bt2_count$word == "love"],
  bt3_count$n[bt3_count$word == "love"],
  bt4_count$n[bt4_count$word == "love"],
  bt5_count$n[bt5_count$word == "love"],
  bt6_count$n[bt6_count$word == "love"],
  bt7_count$n[bt7_count$word == "love"],
  bt8_count$n[bt8_count$word == "love"],
  bt9_count$n[bt9_count$word == "love"],
  bt10_count$n[bt10_count$word == "love"],
  bt11_count$n[bt11_count$word == "love"],
  bt12_count$n[bt12_count$word == "love"],
  bt13_count$n[bt13_count$word == "love"],
  bt14_count$n[bt14_count$word == "love"],
  bt15_count$n[bt15_count$word == "love"],
  bt16_count$n[bt16_count$word == "love"],
  bt17_count$n[bt17_count$word == "love"],
  bt18_count$n[bt18_count$word == "love"])

#INTERESTING
interest_sample <- c(
  bt1_count$n[bt1_count$word == "interesting"],
  bt2_count$n[bt2_count$word == "interesting"],
  bt3_count$n[bt3_count$word == "interesting"],
  bt4_count$n[bt4_count$word == "interesting"],
  bt5_count$n[bt5_count$word == "interesting"],
  bt6_count$n[bt6_count$word == "interesting"],
  bt7_count$n[bt7_count$word == "interesting"],
  bt8_count$n[bt8_count$word == "interesting"],
  bt9_count$n[bt9_count$word == "interesting"],
  bt10_count$n[bt10_count$word == "interesting"],
  bt11_count$n[bt11_count$word == "interesting"],
  bt12_count$n[bt12_count$word == "interesting"],
  bt13_count$n[bt13_count$word == "interesting"],
  bt14_count$n[bt14_count$word == "interesting"],
  bt15_count$n[bt15_count$word == "interesting"],
  bt16_count$n[bt16_count$word == "interesting"],
  bt17_count$n[bt17_count$word == "interesting"],
  bt18_count$n[bt18_count$word == "interesting"])
length(interest_sample)
interest_sample

#GOOD
good_sample <- c(
  bt1_count$n[bt1_count$word == "good"],
  bt2_count$n[bt2_count$word == "good"],
  bt3_count$n[bt3_count$word == "good"],
  bt4_count$n[bt4_count$word == "good"],
  bt5_count$n[bt5_count$word == "good"],
  bt6_count$n[bt6_count$word == "good"],
  bt7_count$n[bt7_count$word == "good"],
  bt8_count$n[bt8_count$word == "good"],
  bt9_count$n[bt9_count$word == "good"],
  bt10_count$n[bt10_count$word == "good"],
  bt11_count$n[bt11_count$word == "good"],
  bt12_count$n[bt12_count$word == "good"],
  bt13_count$n[bt13_count$word == "good"],
  bt14_count$n[bt14_count$word == "good"],
  bt15_count$n[bt15_count$word == "good"],
  bt16_count$n[bt16_count$word == "good"],
  bt17_count$n[bt17_count$word == "good"],
  bt18_count$n[bt18_count$word == "good"])
length(good_sample)
good_sample

#BAD
bad_sample <- c(
  bt1_count$n[bt1_count$word == "bad"],
  bt2_count$n[bt2_count$word == "bad"],
  bt3_count$n[bt3_count$word == "bad"],
  bt4_count$n[bt4_count$word == "bad"],
  bt5_count$n[bt5_count$word == "bad"],
  bt6_count$n[bt6_count$word == "bad"],
  bt7_count$n[bt7_count$word == "bad"],
  bt8_count$n[bt8_count$word == "bad"],
  bt9_count$n[bt9_count$word == "bad"],
  bt10_count$n[bt10_count$word == "bad"],
  bt11_count$n[bt11_count$word == "bad"],
  bt12_count$n[bt12_count$word == "bad"],
  bt13_count$n[bt13_count$word == "bad"],
  bt14_count$n[bt14_count$word == "bad"],
  bt15_count$n[bt15_count$word == "bad"],
  bt16_count$n[bt16_count$word == "bad"],
  bt17_count$n[bt17_count$word == "bad"],
  bt18_count$n[bt18_count$word == "bad"])
length(bad_sample)
bad_sample


#SPECIALIZED WORDS

#WAR
war_sample <- c(
  bt1_count$n[bt1_count$word == "war"],
  0,
  bt3_count$n[bt3_count$word == "war"],
  bt4_count$n[bt4_count$word == "war"],
  bt5_count$n[bt5_count$word == "war"],
  bt6_count$n[bt6_count$word == "war"],
  0,
  bt8_count$n[bt8_count$word == "war"],
  0,
  bt10_count$n[bt10_count$word == "war"],
  0,
  0,
  bt13_count$n[bt13_count$word == "war"],
  0,
  bt15_count$n[bt15_count$word == "war"],
  bt16_count$n[bt16_count$word == "war"],
  bt17_count$n[bt17_count$word == "war"],
  bt18_count$n[bt18_count$word == "war"])
length(war_sample)
war_sample


#CONSTRUCTION

constr_sample <- c(
  bt1_count$n[bt1_count$word == "construction"],
  0,
  0,
  0,
  0,
  bt6_count$n[bt6_count$word == "construction"],
  bt7_count$n[bt7_count$word == "construction"],
  bt8_count$n[bt8_count$word == "construction"],
  0,
  0,
  bt11_count$n[bt11_count$word == "construction"],
  bt12_count$n[bt12_count$word == "construction"],
  0,
  0,
  0,
  bt16_count$n[bt16_count$word == "construction"],
  0,
  0)
length(constr_sample)
constr_sample

#SOFTWARE

soft_sample <- c(
  0,
  bt2_count$n[bt2_count$word == "software"],
  bt3_count$n[bt3_count$word == "software"],
  bt4_count$n[bt4_count$word == "software"],
  0,
  0,
  0,
  0,
  0,
  bt10_count$n[bt10_count$word == "software"],
  0,
  bt12_count$n[bt12_count$word == "software"],
  0,
  bt14_count$n[bt14_count$word == "software"],
  bt15_count$n[bt15_count$word == "software"],
  bt16_count$n[bt16_count$word == "software"],
  bt17_count$n[bt17_count$word == "software"],
  bt18_count$n[bt18_count$word == "software"])
length(soft_sample)
soft_sample

#ANIMAL

animal_sample <- c(
  0,
  0,
  0,
  0,
  0,
  bt6_count$n[bt6_count$word == "animal"],
  0,
  bt8_count$n[bt8_count$word == "animal"],
  bt9_count$n[bt9_count$word == "animal"],
  bt10_count$n[bt10_count$word == "animal"],
  0,
  0,
  0,
  0,
  0,
  bt16_count$n[bt16_count$word == "animal"],
  bt17_count$n[bt17_count$word == "animal"],
  0)
length(animal_sample)
animal_sample



#EMPIRICAL

#FIRST : EMPIRICAL (observations) : COMMON FOR ALL OF US

#AMAZING
#mean
mean(amazing_sample)
#variance
sd(amazing_sample)^2
#IDF
-log((length(amazing_sample) - sum(amazing_sample == 0))/length(amazing_sample), base = 2)
#Burstiness
mean(amazing_sample)/((length(amazing_sample) - sum(amazing_sample == 0))/length(amazing_sample))
#Adaptation
((length(amazing_sample) - (sum(amazing_sample == 0) + sum(amazing_sample == 1)))/length(amazing_sample))/((length(amazing_sample) - sum(amazing_sample == 0))/length(amazing_sample))
#Entropy
Entropy(amazing_sample)

#BAD
#mean
mean(bad_sample)
#variance
sd(bad_sample)^2
#IDF
-log((length(bad_sample) - sum(bad_sample == 0))/length(bad_sample), base = 2)
#Burstiness
mean(bad_sample)/((length(bad_sample) - sum(bad_sample == 0))/length(bad_sample))
#Adaptation
((length(bad_sample) - (sum(bad_sample == 0) + sum(bad_sample == 1)))/length(bad_sample))/((length(bad_sample) - sum(bad_sample == 0))/length(bad_sample))
#Entropy
Entropy(bad_sample)

#INTERESTING
#mean
mean(interest_sample)
#variance
sd(interest_sample)^2
#IDF
-log((length(interest_sample) - sum(interest_sample == 0))/length(interest_sample), base = 2)
#Burstiness
mean(interest_sample)/((length(interest_sample) - sum(interest_sample == 0))/length(interest_sample))
#Adaptation
((length(interest_sample) - (sum(interest_sample == 0) + sum(interest_sample == 1)))/length(interest_sample))/((length(interest_sample) - sum(interest_sample == 0))/length(interest_sample))
#Entropy
Entropy(interest_sample)

#LOVE
#mean
mean(love_sample)
#variance
sd(love_sample)^2
#IDF
-log((length(love_sample) - sum(love_sample == 0))/length(love_sample), base = 2)
#Burstiness
mean(love_sample)/((length(love_sample) - sum(love_sample == 0))/length(love_sample))
#Adaptation
((length(love_sample) - (sum(love_sample == 0) + sum(love_sample == 1)))/length(love_sample))/((length(love_sample) - sum(love_sample == 0))/length(love_sample))
#Entropy
Entropy(love_sample)

#GOOD
#mean
mean(good_sample)
#variance
sd(good_sample)^2
#IDF
-log((length(good_sample) - sum(good_sample == 0))/length(good_sample), base = 2)
#Burstiness
mean(good_sample)/((length(good_sample) - sum(good_sample == 0))/length(good_sample))
#Adaptation
((length(good_sample) - (sum(good_sample == 0) + sum(good_sample == 1)))/length(good_sample))/((length(good_sample) - sum(good_sample == 0))/length(good_sample))
#Entropy
Entropy(good_sample)

#INVESTMENT
#mean
mean(invest_sample)
#variance
sd(invest_sample)^2
#IDF
-log((length(invest_sample) - sum(invest_sample == 0))/length(invest_sample), base = 2)
#Burstiness
mean(invest_sample)/((length(invest_sample) - sum(invest_sample == 0))/length(invest_sample))
#Adaptation
((length(invest_sample) - (sum(invest_sample == 0) + sum(invest_sample == 1)))/length(invest_sample))/((length(invest_sample) - sum(invest_sample == 0))/length(invest_sample))
#Entropy
Entropy(invest_sample)

#WAR
#mean
mean(war_sample)
#variance
sd(war_sample)^2
#IDF
-log((length(war_sample) - sum(war_sample == 0))/length(war_sample), base = 2)
#Burstiness
mean(war_sample)/((length(war_sample) - sum(war_sample == 0))/length(war_sample))
#Adaptation
((length(war_sample) - (sum(war_sample == 0) + sum(war_sample == 1)))/length(war_sample))/((length(war_sample) - sum(war_sample == 0))/length(war_sample))
#Entropy
Entropy(war_sample)

#CONSTRUCTION
#mean
mean(constr_sample)
#variance
sd(constr_sample)^2
#IDF
-log((length(constr_sample) - sum(constr_sample == 0))/length(constr_sample), base = 2)
#Burstiness
mean(constr_sample)/((length(constr_sample) - sum(constr_sample == 0))/length(constr_sample))
#Adaptation
((length(constr_sample) - (sum(constr_sample == 0) + sum(constr_sample == 1)))/length(constr_sample))/((length(constr_sample) - sum(constr_sample == 0))/length(constr_sample))
#Entropy
Entropy(constr_sample)

#SOFTWARE
#mean
mean(soft_sample)
#variance
sd(soft_sample)^2
#IDF
-log((length(soft_sample) - sum(soft_sample == 0))/length(soft_sample), base = 2)
#Burstiness
mean(soft_sample)/((length(soft_sample) - sum(soft_sample == 0))/length(soft_sample))
#Adaptation
((length(soft_sample) - (sum(soft_sample == 0) + sum(soft_sample == 1)))/length(soft_sample))/((length(soft_sample) - sum(soft_sample == 0))/length(soft_sample))
#Entropy
Entropy(soft_sample)

#ANIMAL
#mean
mean(animal_sample)
#variance
sd(animal_sample)^2
#IDF
-log((length(animal_sample) - sum(animal_sample == 0))/length(animal_sample), base = 2)
#Burstiness
mean(animal_sample)/((length(animal_sample) - sum(animal_sample == 0))/length(animal_sample))
#Adaptation
((length(animal_sample) - (sum(animal_sample == 0) + sum(animal_sample == 1)))/length(animal_sample))/((length(animal_sample) - sum(animal_sample == 0))/length(animal_sample))
#Entropy
Entropy(animal_sample)



#PARAMETERS POISSON

poiss_coef <- function(data){
  #    We compute the first moment
  alpha <- moment(data, order=1, center=FALSE, absolute=FALSE, na.rm=FALSE)
  print(alpha)
}

#exp :
exp_poisson=function(data){print(poiss_coef(data))}
#var :
var_poisson=function(data){print(poiss_coef(data))}
#idf :
IDF_poisson=function(data){print(-log(1-exp(-poiss_coef(data)),base = 2))}
#burstiness :
bur_poisson=function(data){print(poiss_coef(data)/(1-exp(-poiss_coef(data))))}
#adaptation :
adap_poisson=function(data){print((1-exp(-poiss_coef(data))-poiss_coef(data)*exp(-poiss_coef(data)))/(1-exp(-poiss_coef(data))))}
#entropy :
entropy_poisson=function(data){
  print(-sum((exp(-poiss_coef(data))*poiss_coef(data)^data/factorial(data))*log(exp(-poiss_coef(data))*poiss_coef(data)^data/factorial(data),base=2)))
}


poisson_para <- function(data){
  name <- c("exp", "var", "IDF", 'Burstiness', "Adaptation","Entropy")
  value <- c(
    exp_poisson(data),
    var_poisson(data),
    IDF_poisson(data),
    bur_poisson(data),
    adap_poisson(data),
    entropy_poisson(data)
  )
  cbind(name,value)
}

poisson_para(amazing_sample)
poisson_para(love_sample)
poisson_para(interest_sample)
poisson_para(good_sample)
poisson_para(bad_sample)
poisson_para(invest_sample)
poisson_para(war_sample)
poisson_para(constr_sample)
poisson_para(soft_sample)
poisson_para(animal_sample)


#PARAMETERS NEGATIVE BINOMIAL

#AMAZING
nbin_amazing <- fitdist(amazing_sample, "nbinom")
summary(nbin_amazing)
#size estimated = 1.868387 and mu estimated = 4.824009 (p=0.05787315)

N1=1.868387
mu1=4.824009
P1=N1/((mu1+N1)*mu1) #to find proba of sucess
Q1=P1+1 #to find Q

#mean
nbestmean_amazing = N1*P1
nbestmean_amazing
#variance
nbestvar_amazing = N1*P1*Q1
nbestvar_amazing
#IDF
nbestidf_amazing = -log(1-Q1^(-N1), base = 2)
nbestidf_amazing
#Burstiness
nbestburst_amazing = N1*P1/(1-Q1^(-N1))
nbestburst_amazing
#Adaptation
nbestadaptation_amazing = 1-Q1^(-N1)-N1*P1*Q1^(-N1-1)/(1-Q1^(-N1))
nbestadaptation_amazing
#Entropy
nbentropy_amazing = -(sum(dnbinom(amazing_sample,1.868387,0.05787315)))*log(sum(dnbinom(amazing_sample,1.868387,0.05787315)), base = 2)
nbentropy_amazing


#BAD 
nbin_bad <- fitdist(bad_sample, "nbinom")
summary(nbin_bad)

#size estimated =14.00352  and mu estimated = 16.27712
N2=14.00352
mu2=16.27712
P2=N2/((mu2+N2)*mu2) #to find proba of sucess p=0.02841153
Q2=P2+1 #to find Q

#mean
nbestmean_bad = N2*P2
nbestmean_bad
#variance
nbestvar_bad = N2*P2*Q2
nbestvar_bad
#IDF
nbestidf_bad = -log(1-Q2^(-N2), base = 2)
nbestidf_bad
#Burstiness
nbestburst_bad = N2*P2/(1-Q2^(-N2))
nbestburst_bad
#Adaptation
nbestadaptation_bad = 1-Q2^(-N2)-N2*P2*Q2^(-N2-1)/(1-Q2^(-N2))
nbestadaptation_bad
#Entropy
nbentropy_bad = -(sum(dnbinom(bad_sample,14.00352,0.02841153)))*log(sum(dnbinom(bad_sample,14.00352,0.02841153)), base = 2)
nbentropy_bad


#INTERESTING: 
nbin_interest <- fitdist(interest_sample, "nbinom")
summary(nbin_interest)

#size estimated =2.963354  and mu estimated = 9.775550
N3=2.963354
mu3=9.775550
P3=N3/((mu3+N3)*mu3) #to find proba of sucess #0.02379635
Q3=P3+1 #to find Q

#mean
nbestmean_interesting = N3*P3
nbestmean_interesting
#variance
nbestvar_life = N3*P3*Q3
nbestvar_life
#IDF
nbestidf_interesting= -log(1-Q3^(-N3), base = 2)
nbestidf_interesting
#Burstiness
nbestburst_interesting = N3*P3/(1-Q3^(-N3))
nbestburst_interesting
#Adaptation
nbestadaptation_interesting = 1-Q3^(-N3)-N3*P3*Q3^(-N3-1)/(1-Q3^(-N3))
nbestadaptation_interesting
#Entropy
nbentropy_interesting = -(sum(dnbinom(interest_sample,2.963354,0.02379635)))*log(sum(dnbinom(interest_sample,2.963354,0.02379635)), base = 2)
nbentropy_interesting



#LOVE : 
nbin_love <- fitdist(love_sample, "nbinom")
summary(nbin_love)

#size estimated =5,329779  and mu estimated =34,276100 
N4=5.329779
mu4=34.2761
P4=N4/((mu4+N4)*mu4) #to find proba of sucess #p=0.003926071
Q4=P4+1 #to find Q

#mean
nbestmean_love = N4*P4
nbestmean_love
#variance
nbestvar_love = N4*P4*Q4
nbestvar_love
#IDF
nbestidf_love = -log(1-Q4^(-N4), base = 2)
nbestidf_love
#Burstiness
nbestburst_love = N4*P4/(1-Q4^(-N4))
nbestburst_love
#Adaptation
nbestadaptation_love = 1-Q4^(-N4)-N4*P4*Q4^(-N4-1)/(1-Q4^(-N4))
nbestadaptation_love
#Entropy
nbentropy_love = -(sum(dnbinom(love_sample,5.329779,0.003926071)))*log(sum(dnbinom(love_sample,5.329779,0.003926071)), base = 2)
nbentropy_love

#GOOD 
nbin_good <- fitdist(good_sample, "nbinom")
summary(nbin_good)

#size estimated = 9.917367  and mu estimated =55.949856 
N5=9.917367
mu5=55.949856
P5=N5/((mu5+N5)*mu5) #to find proba of sucess p=0.002691089
Q5=P5+1 #to find Q

#mean
nbestmean_good = N5*P5
nbestmean_good
#variance
nbestvar_good = N5*P5*Q5
nbestvar_good
#IDF
nbestidf_good = -log(1-Q5^(-N5), base = 2)
nbestidf_good
#Burstiness
nbestburst_good = N5*P5/(1-Q5^(-N5))
nbestburst_good
#Adaptation
nbestadaptation_good = 1-Q5^(-N5)-N5*P5*Q5^(-N5-1)/(1-Q5^(-N5))
nbestadaptation_good
#Entropy
nbentropy_good = -(sum(dnbinom(good_sample,9.917367,0.002691089)))*log(sum(dnbinom(good_sample,9.917367,0.002691089)), base = 2)
nbentropy_good


#INVESTMENT : 
nbin_invest<- fitdist(invest_sample, "nbinom")
summary(nbin_invest)

#size estimated = 0.2187714 and mu estimated =0.6665319
N6=0.2187714
mu6=0.6665319
P6=N6/((mu6+N6)*mu6) #to find proba of sucess p=0.3707469
Q6=P6+1 #to find Q

#mean
N6*P6
#variance
N6*P6*Q6
#IDF
-log(1-Q6^(-N6), base = 2)
#Burstiness
N6*P6/(1-Q6^(-N6))
#Adaptation
1-Q6^(-N6)-N6*P6*Q6^(-N6-1)/(1-Q6^(-N6))
#Entropy
-(sum(dnbinom(invest_sample,0.2187714,0.3707469)))*log(sum(dnbinom(invest_sample,0.2187714,0.3707469)), base = 2)

#WAR
nbin_war <- fitdist(war_sample, "nbinom")
summary(nbin_war)
# size estimated =0.2866813  and mu estimated = 9.665433
N7=0.2866813
mu7=9.665433
P7=N7/((mu7+N7)*mu7) #to find proba of sucess #P=0.002980319
Q7=P7+1 #to find Q

#mean
N7*P7
#variance
N7*P7*Q7
#IDF
-log(1-Q7^(-N7), base = 2)
#Burstiness
N7*P7/(1-Q7^(-N7))
#Adaptation
1-Q7^(-N7)-N7*P7*Q7^(-N7-1)/(1-Q7^(-N7))
#Entropy
-(sum(dnbinom(war_sample,0.2866813,0.002980319)))*log(sum(dnbinom(war_sample,0.2866813,0.002980319)), base = 2)


#CONSTRUCTION
nbin_constr <- fitdist(constr_sample, "nbinom")
summary(nbin_constr)
# size estimated = 0.00000019 and mu estimated = 0.4444972 (p=0.)
N8=0.00000019
mu8=0.444972
P8=N8/((mu8+N8)*mu8) #to find proba of sucess P=0.00000009
Q8=P8+1 #to find Q

#mean
N8*P8
#variance
N8*P8*Q8
#IDF
-log(1-Q8^(-N8), base = 2)
#Burstiness
N8*P8/(1-Q8^(-N8))
#Adaptation
1-Q8^(-N8)-N8*P8*Q8^(-N8-1)/(1-Q8^(-N8))
#Entropy
-(sum(dnbinom(constr_sample,0.00000019,0.00000009)))*log(sum(dnbinom(constr_sample,0.00000019,0.00000009)), base = 2)


#SOFTWARE 
nbin_software <- fitdist(soft_sample, "nbinom")
summary(nbin_software)
#size estimated = 1.209820 and mu estimated =1.055776
N9=1.209820
mu9=1.055776
P9=N9/((mu9+N9)*mu9) #to find proba of sucess #P=0.5057857
Q9=P9+1 #to find Q

#mean
N9*P9
#variance
N9*P9*Q9
#IDF
-log(1-Q9^(-N9), base = 2)
#Burstiness
N9*P9/(1-Q9^(-N9))
#Adaptation
1-Q9^(-N9)-N9*P9*Q9^(-N9-1)/(1-Q9^(-N9))
#Entropy
-(sum(dnbinom(soft_sample,1.209820,0.507857)))*log(sum(dnbinom(soft_sample,1.209820,0.5057857)), base = 2)


#ANIMAL
nbin_animal <- fitdist(animal_sample, "nbinom")
summary(nbin_animal)

# size estimated = 0.2448046 and mu estimated = 0.9998388
N10=0.2448046
mu10=0.9998388
P10=N10/((mu10+N10)*mu10) #to find proba of sucess P=0.1967182
Q10=P10+1 #to find Q

#mean
N10*P10
#variance
N10*P10*Q10
#IDF
-log(1-Q10^(-N10), base = 2)
#Burstiness
N10*P10/(1-Q10^(-N10))
#Adaptation
1-Q10^(-N10)-N10*P10*Q10^(-N10-1)/(1-Q10^(-N10))
#Entropy
-(sum(dnbinom(animal_sample,0.2448046,0.1967182)))*log(sum(dnbinom(animal_sample,0.2448046,0.1967182)), base = 2)


#PARAMETERS 2-POISSON
#amazing:


ama_coef_2poiss <- poiss_2_coef(amazing_sample)
lambda1.1=ama_coef_2poiss[1]
lambda2.1=ama_coef_2poiss[2]
alpha.1=ama_coef_2poiss[3]


#mean:
poiss_est_mean_ama=(alpha.1)*(lambda1.1) + (1-alpha.1)*(lambda2.1)
#var:
poiss_est_var_ama=(alpha.1^2)*lambda1.1+((1-alpha.1)^2)*lambda2.1
#idf :
poiss_est_idf_ama=alpha.1*(-log(1-exp(-lambda1.1),base = 2))+(1-alpha.1)*(-log(1-exp(-lambda2.1),base = 2))
#burstiness :
poiss_est_burst_ama=alpha.1*(lambda1.1/(1-exp(-lambda1.1))) + (1-alpha.1)*(lambda2.1/(1-exp(-lambda2.1)))
#adaptation :
poiss_est_adap_ama=alpha.1*((1-exp(-lambda1.1)-lambda1.1*exp(-lambda1.1))/(1-exp(-lambda1.1)))+(1-alpha.1)*((1-exp(-lambda2.1)-lambda2.1*exp(-lambda2.1))/(1-exp(-lambda2.1)))
#entropy :
poiss2_ama <- poiss_2_est(20,Re(alpha.1),Re(lambda1.1), Re(lambda2.1))
Entropy(poiss2_ama)

#bad :

bad_coef_2poiss <- poiss_2_coef(bad_sample)
lambda1.2=bad_coef_2poiss[1]
lambda2.2=bad_coef_2poiss[2]
alpha.2=bad_coef_2poiss[3]


#mean:
poiss_est_mean_bad=(alpha.2)*(lambda1.2) + (1-alpha.2)*(lambda2.2)
#var:
poiss_est_var_bad=(alpha.2^2)*lambda1.2+((1-alpha.2)^2)*lambda2.2
#idf :
poiss_est_idf_bad=alpha.2*(-log(1-exp(-lambda1.2),base = 2))+(1-alpha.2)*(-log(1-exp(-lambda2.2),base = 2))
#burstiness :
poiss_est_burst_bad=alpha.2*(lambda1.2/(1-exp(-lambda1.2))) + (1-alpha.2)*(lambda2.2/(1-exp(-lambda2.2)))
#adaptation :
poiss_est_adap_bad=alpha.2*((1-exp(-lambda1.2)-lambda1.2*exp(-lambda1.2))/(1-exp(-lambda1.2)))+(1-alpha.2)*((1-exp(-lambda2.2)-lambda2.2*exp(-lambda2.2))/(1-exp(-lambda2.2)))
#entropy :
poiss2_bad <- poiss_2_est(30,Re(alpha.2),Re(lambda1.2), Re(lambda2.2))
Entropy(poiss2_bad)


#interesting :

interes_coef_2poiss <- poiss_2_coef(interest_sample)
lambda1.3=interes_coef_2poiss[1]
lambda2.3=interes_coef_2poiss[2]
alpha.3=interes_coef_2poiss[3]


#mean:
poiss_est_mean_interes=(alpha.3)*(lambda1.3) + (1-alpha.3)*(lambda2.3)
#var:
poiss_est_var_interes=(alpha.3^2)*lambda1.3+((1-alpha.3)^2)*lambda2.3
#idf :
poiss_est_idf_interes=alpha.3*(-log(1-exp(-lambda1.3),base = 2))+(1-alpha.3)*(-log(1-exp(-lambda2.3),base = 2))
#burstiness :
poiss_est_burst_interes=alpha.3*(lambda1.3/(1-exp(-lambda1.3))) + (1-alpha.3)*(lambda2.3/(1-exp(-lambda2.3)))
#adaptation :
poiss_est_adap_interes=alpha.3*((1-exp(-lambda1.3)-lambda1.3*exp(-lambda1.3))/(1-exp(-lambda1.3)))+(1-alpha.3)*((1-exp(-lambda2.3)-lambda2.3*exp(-lambda2.3))/(1-exp(-lambda2.3)))
#entropy :
poiss2_int<- poiss_2_est(30,Re(alpha.3),Re(lambda1.3), Re(lambda2.3))
Entropy(poiss2_bad)

#love :

love_coef_2poiss <- poiss_2_coef(love_sample)
lambda1.4=love_coef_2poiss[1]
lambda2.4=love_coef_2poiss[2]
alpha.4=love_coef_2poiss[3]


#mean:
poiss_est_mean_love=(alpha.4)*(lambda1.4) + (1-alpha.4)*(lambda2.4)
#var:
poiss_est_var_love=(alpha.4^2)*lambda1.4+((1-alpha.4)^2)*lambda2.4
#idf :
poiss_est_idf_love=alpha.4*(-log(1-exp(-lambda1.4),base = 2))+(1-alpha.4)*(-log(1-exp(-lambda2.4),base = 2))
#burstiness :
poiss_est_burst_love=alpha.4*(lambda1.4/(1-exp(-lambda1.4))) + (1-alpha.4)*(lambda2.4/(1-exp(-lambda2.4)))
#adaptation :
poiss_est_adap_love=alpha.4*((1-exp(-lambda1.4)-lambda1.4*exp(-lambda1.4))/(1-exp(-lambda1.4)))+(1-alpha.4)*((1-exp(-lambda2.4)-lambda2.4*exp(-lambda2.4))/(1-exp(-lambda2.4)))
#entropy :
poiss2_love<- poiss_2_est(30,Re(alpha.4),Re(lambda1.4), Re(lambda2.4))
Entropy(poiss2_love)


#good:

good_coef_2poiss <- poiss_2_coef(good_sample)
lambda1.5=good_coef_2poiss[1]
lambda2.5=good_coef_2poiss[2]
alpha.5=good_coef_2poiss[3]


#mean:
poiss_est_mean_good=(alpha.5)*(lambda1.5) + (1-alpha.5)*(lambda2.5)
#var:
poiss_est_var_good=(alpha.5^2)*lambda1.5+((1-alpha.5)^2)*lambda2.5
#idf :
poiss_est_idf_good=alpha.5*(-log(1-exp(-lambda1.5),base = 2))+(1-alpha.5)*(-log(1-exp(-lambda2.5),base = 2))
#burstiness :
poiss_est_burst_good=alpha.5*(lambda1.5/(1-exp(-lambda1.5))) + (1-alpha.5)*(lambda2.5/(1-exp(-lambda2.5)))
#adaptation :
poiss_est_adap_good=alpha.5*((1-exp(-lambda1.5)-lambda1.5*exp(-lambda1.5))/(1-exp(-lambda1.5)))+(1-alpha.5)*((1-exp(-lambda2.5)-lambda2.5*exp(-lambda2.5))/(1-exp(-lambda2.5)))
#entropy :
poiss2_good<- poiss_2_est(90,Re(alpha.5),Re(lambda1.5), Re(lambda2.5))
Entropy(poiss2_good)

#investment:

invest_coef_2poiss <- poiss_2_coef(invest_sample)
lambda1.6=invest_coef_2poiss[1]
lambda2.6=invest_coef_2poiss[2]
alpha.6=invest_coef_2poiss[3]


#mean:
poiss_est_mean_invest=(alpha.6)*(lambda1.6) + (1-alpha.6)*(lambda2.6)
#var:
poiss_est_var_invest=(alpha.6^2)*lambda1.6+((1-alpha.6)^2)*lambda2.6
#idf :
poiss_est_idf_invest=alpha.6*(-log(1-exp(-lambda1.6),base = 2))+(1-alpha.6)*(-log(1-exp(-lambda2.6),base = 2))
#burstiness :
poiss_est_burst_invest=alpha.6*(lambda1.6/(1-exp(-lambda1.6))) + (1-alpha.6)*(lambda2.6/(1-exp(-lambda2.6)))
#adaptation :
poiss_est_adap_invest=alpha.6*((1-exp(-lambda1.6)-lambda1.6*exp(-lambda1.6))/(1-exp(-lambda1.6)))+(1-alpha.6)*((1-exp(-lambda2.6)-lambda2.6*exp(-lambda2.6))/(1-exp(-lambda2.6)))
#entropy :
poiss2_invest<- poiss_2_est(30,Re(alpha.6),Re(lambda1.6), Re(lambda2.6))
Entropy(poiss2_invest)


#war:

war_coef_2poiss <- poiss_2_coef(war_sample)
lambda1.7=war_coef_2poiss[1]
lambda2.7=war_coef_2poiss[2]
alpha.7=war_coef_2poiss[3]


#mean:
poiss_est_mean_war=(alpha.7)*(lambda1.7) + (1-alpha.7)*(lambda2.7)
#var:
poiss_est_var_war=(alpha.7^2)*lambda1.7+((1-alpha.7)^2)*lambda2.7
#idf :
poiss_est_idf_war=alpha.7*(-log(1-exp(-lambda1.7),base = 2))+(1-alpha.7)*(-log(1-exp(-lambda2.7),base = 2))
#burstiness :
poiss_est_burst_war=alpha.7*(lambda1.7/(1-exp(-lambda1.7))) + (1-alpha.7)*(lambda2.7/(1-exp(-lambda2.7)))
#adaptation :
poiss_est_adap_war=alpha.7*((1-exp(-lambda1.7)-lambda1.7*exp(-lambda1.7))/(1-exp(-lambda1.7)))+(1-alpha.7)*((1-exp(-lambda2.7)-lambda2.7*exp(-lambda2.7))/(1-exp(-lambda2.7)))
#entropy :
poiss2_war<- poiss_2_est(30,Re(alpha.7),Re(lambda1.7), Re(lambda2.7))
Entropy(poiss2_war)


#construction:

constru_coef_2poiss <- poiss_2_coef(constr_sample)
lambda1.8=constru_coef_2poiss[1]
lambda2.8=constru_coef_2poiss[2]
alpha.8=constru_coef_2poiss[3]


#mean:
poiss_est_mean_constru=(alpha.8)*(lambda1.8) + (1-alpha.8)*(lambda2.8)
#var:
poiss_est_var_constru=(alpha.8^2)*lambda1.8+((1-alpha.8)^2)*lambda2.8
#idf :
poiss_est_idf_constru=alpha.8*(-log(1-exp(-lambda1.8),base = 2))+(1-alpha.8)*(-log(1-exp(-lambda2.8),base = 2))
#burstiness :
poiss_est_burst_constru=alpha.8*(lambda1.8/(1-exp(-lambda1.8))) + (1-alpha.8)*(lambda2.8/(1-exp(-lambda2.8)))
#adaptation :
poiss_est_adap_constru=alpha.8*((1-exp(-lambda1.8)-lambda1.8*exp(-lambda1.8))/(1-exp(-lambda1.8)))+(1-alpha.8)*((1-exp(-lambda2.8)-lambda2.8*exp(-lambda2.8))/(1-exp(-lambda2.8)))
#entropy :
poiss2_const<- poiss_2_est(30,Re(alpha.8),Re(lambda1.8), Re(lambda2.8))
Entropy(poiss2_const)



#software:
softw_coef_2poiss <- poiss_2_coef(soft_sample)
lambda1.9=softw_coef_2poiss[1]
lambda2.9=softw_coef_2poiss[2]
alpha.9=softw_coef_2poiss[3]


#mean:
poiss_est_mean_softw=(alpha.9)*(lambda1.9) + (1-alpha.9)*(lambda2.9)
#var:
poiss_est_var_softw=(alpha.9^2)*lambda1.9+((1-alpha.9)^2)*lambda2.9
#idf :
poiss_est_idf_softw=alpha.9*(-log(1-exp(-lambda1.9),base = 2))+(1-alpha.9)*(-log(1-exp(-lambda2.9),base = 2))
#burstiness :
poiss_est_burst_softw=alpha.9*(lambda1.9/(1-exp(-lambda1.9))) + (1-alpha.9)*(lambda2.9/(1-exp(-lambda2.9)))
#adaptation :
poiss_est_adap_softw=alpha.9*((1-exp(-lambda1.9)-lambda1.9*exp(-lambda1.9))/(1-exp(-lambda1.9)))+(1-alpha.9)*((1-exp(-lambda2.9)-lambda2.9*exp(-lambda2.9))/(1-exp(-lambda2.9)))
#entropy :
poiss2_soft <- poiss_2_est(10,Re(alpha.9),Re(lambda1.9), Re(lambda2.9))
Entropy(poiss2_soft)


#animal
anim_coef_2poiss <- poiss_2_coef(animal_sample)
lambda1.10=anim_coef_2poiss[1]
lambda2.10=anim_coef_2poiss[2]
alpha.10=anim_coef_2poiss[3]


#mean:
poiss_est_mean_anim=(alpha.10)*(lambda1.10) + (1-alpha.10)*(lambda2.10)
#var:
poiss_est_var_anim=(alpha.10^2)*lambda1.10+((1-alpha.10)^2)*lambda2.10
#idf :
poiss_est_idf_anim=alpha.10*(-log(1-exp(-lambda1.10),base = 2))+(1-alpha.10)*(-log(1-exp(-lambda2.10),base = 2))
#burstiness :
poiss_est_burst_anim=alpha.10*(lambda1.10/(1-exp(-lambda1.10))) + (1-alpha.10)*(lambda2.10/(1-exp(-lambda2.10)))
#adaptation :
poiss_est_adap_anim=alpha.10*((1-exp(-lambda1.10)-lambda1.10*exp(-lambda1.10))/(1-exp(-lambda1.10)))+(1-alpha.10)*((1-exp(-lambda2.10)-lambda2.10*exp(-lambda2.10))/(1-exp(-lambda2.10)))
#entropy :
poiss2_ani <- poiss_2_est(10,Re(alpha.10),Re(lambda1.10), Re(lambda2.10))
Entropy(poiss2_ani)






#PARAMETERS K-MIXTURE
katz_love <- katz_coef(love_sample)
interest_katz <- katz_coef(interest_sample)
good_katz <- katz_coef(good_sample)
bad_katz <- katz_coef(bad_sample)

war_katz <- katz_coef(war_sample)
constr_katz <- katz_coef(constr_sample)
soft_katz <- katz_coef(soft_sample)
animal_katz <- katz_coef(animal_sample)

exp_katz <- function(alpha, beta){alpha*beta}
var_katz <- function(alpha, beta){(alpha*beta)*((2 - alpha)*beta + 1)}
IDF_katz <- function(alpha, beta){log( (beta + 1)/(alpha*beta), base = 2)}
ent_katz <- function(alpha, beta){(alpha*beta)*log( (beta + 1)/beta, base=2) + alpha*(beta/(beta + 1))*log( (beta + 1)/alpha, base = 2) - katz_est(0,alpha,beta)*log(katz_est(0,alpha,beta), base=2)}
bur_katz <- function(alpha, beta){beta + 1}
adap_katz <- function(alpha, beta){beta/(beta+1)}

katz_para <- function(data){
  name <- c("exp", "var", "IDF", "Entropy", 'Burstiness', "Adaptation")
  value <- c(
    exp_katz(data[1], data[2]),
    var_katz(data[1], data[2]),
    IDF_katz(data[1], data[2]),
    ent_katz(data[1], data[2]),    
    bur_katz(data[1], data[2]),
    adap_katz(data[1], data[2])
  )
  cbind(name,value)
}



#AMAZING
katz_para(katz_coef_ama)
#entropy cannot be computed because of P(k=0) < 0

#LOVE
katz_para(katz_love)
#entropy cannot be computed because of P(k=0) < 0

#INTERESTING
katz_para(interest_katz)
#entropy cannot be computed because of P(k=0) < 0

#GOOD
katz_para(good_katz)
#entropy cannot be computed because of P(k=0) < 0

#BAD
katz_para(bad_katz)
#entropy cannot be computed because of P(k=0) < 0

#INVESTMENT
katz_para(katz_est_inv)

#WAR
katz_para(war_katz)

#CONSTRUCTION
katz_para(constr_katz)
#entropy cannot be computed because of P(k=0) < 0

#SOFTWARE
katz_para(soft_katz)
#entropy cannot be computed because of P(k=0) < 0

#ANIMAL
katz_para(animal_katz)









# QUESTION 5

#goodness fit

#amazing

test_ama_2p <- cbind(amazing_sample, poiss_2_ama[1:18]*18)
chi2_ama_2p <- chisq.test(test_ama_2p)
chi2_ama_2p

test_ama_k <- cbind(amazing_sample, katz_est_ama[1:18]*18)
#because of the negative probability on 0
chi2_ama_k <- chisq.test(test_ama_k[-1,])
chi2_ama_k


test_ama_p <- cbind(amazing_sample,poiss_amazing_est[1:18]*18)
chi2_ama_p <- chisq.test(test_ama_p)
chi2_ama_p

test_ama_nb <- cbind(amazing_sample,nbin_amazing_est[1:18]*18)
chi2_ama_nb <- chisq.test(test_ama_nb)
chi2_ama_nb

#investment

test_inv_2p <- cbind(invest_sample, poiss_2_inv[1:18]*18)
chi2_inv_2p <- chisq.test(test_inv_2p)
chi2_inv_2p

test_inv_k <- cbind(invest_sample, katz_est_inv[1:18]*18)
chi2_inv_k <- chisq.test(test_inv_k)
chi2_inv_k


test_inv_p <- cbind(invest_sample,poiss_inv_est[1:18]*18)
chi2_inv_p <- chisq.test(test_inv_p)
chi2_inv_p

nbin_inv_est <- dnbinom(seq(0,20,1), size = 0.219, mu = 0.667)

test_inv_nb <- cbind(invest_sample,nbin_inv_est[1:18]*18)
chi2_inv_nb <- chisq.test(test_inv_nb)
chi2_inv_nb


#bad

poiss_2_bad <- poiss_2_est(30, 0.789, 18.36, 8.479)

test_bad_2p <- cbind(bad_sample, poiss_2_bad[1:18]*18)
chi2_bad_2p <- chisq.test(test_bad_2p)
chi2_bad_2p

katz_est_bad <- katz_est(30, bad_katz[1], bad_katz[2])

test_bad_k <- cbind(bad_sample, katz_est_bad[1:18]*18)
chi2_bad_k <- chisq.test(test_inv_k[-1,])
chi2_bad_k

poiss_bad_est <- dpois(seq(0,30,1), lambda = poiss_coef(bad_sample))

test_bad_p <- cbind(bad_sample,poiss_bad_est[1:18]*18)
chi2_bad_p <- chisq.test(test_bad_p)
chi2_bad_p

nbin_bad_est <- dnbinom(seq(0,20,1), size = 14, mu = 16.3)

test_bad_nb <- cbind(bad_sample,nbin_bad_est[1:18]*18)
chi2_bad_nb <- chisq.test(test_bad_nb)
chi2_bad_nb

#animal

test_ani_2p <- cbind(animal_sample, poiss_2_est(18, lambda1 = 6.22, lambda2 = 0.27, alpha = 0.12)[1:18]*18)
chi2_ani_2p <- chisq.test(test_ani_2p)
chi2_ani_2p

k_ani <- katz_est(n = 20,alpha = 0.5,beta= 2)

test_ani_k <- cbind(animal_sample, k_ani[1:18]*18)
chi2_ani_k <- chisq.test(test_ani_k)
chi2_ani_k


test_ani_p <- cbind(animal_sample,dpois(seq(0,20,1), lambda = 1)[1:18]*18)
chi2_ani_p <- chisq.test(test_ani_p)
chi2_ani_p

nbin_ani_est <- dnbinom(seq(0,20,1), size = 0.2448, mu = 0.99983)

test_ani_nb <- cbind(animal_sample,nbin_ani_est[1:18]*18)
chi2_ani_nb <- chisq.test(test_ani_nb)
chi2_ani_nb


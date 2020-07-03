library("RcppHMM")

data <- read.csv("data.csv")
#str(data)
N <- 601
M <- 850
T <- 910
open_train <- data$Open[N:M]
high_train <- data$High[N:M]
low_train <- data$Low[N:M]
close_train <- data$Close[N:M]
#print(open_train)
open_test <- data$Open[(M+1):T]
high_test <- data$High[(M+1):T]
low_test <- data$Low[(M+1):T]
close_test <- data$Close[(M+1):T]
#print(data)
test_plot <- cbind(open_test, close_test, high_test, low_test)

frac_close_train <- 100*(close_train-open_train)/open_train
frac_high_train <- 100*(high_train-open_train)/open_train
frac_low_train <- 100*(low_train-open_train)/open_train

vola <- c(100*(close_train[1] - data$Close[N-1])/data$Close[N-1])
for(j in 1:(M-N))
{
  vola[j+1] <- 100*(close_train[j+1] - close_train[j])/ close_train[j]
}

vola_test <- c(100*(close_test[1] - close_train[M-N+1])/close_train[M-N+1])
for(k in 1:(T-M-1))
{
  vola_test[k+1] <- 100*(close_test[k+1] - close_test[k])/ close_test[k]
}

frac_close_test <- 100*(close_test-open_test)/open_test
frac_high_test <- 100*(high_test-open_test)/open_test
frac_low_test <- 100*(low_test-open_test)/open_test
#str(frac_low_train)
#str(frac_low_test)
set.seed(42)

train <- cbind(frac_close_train, frac_high_train, frac_low_train)
#str(train)
test <- cbind(frac_close_test, frac_high_test, frac_low_test)
#print(test)
#print(t(test))
initia <- kmeans(train, 4)
#str(initia)
#str(train)

num_1 <-0
num_2 <-0
num_3 <-0
num_4 <-0

for(i in 1:(M-N+1))
{	
  if(initia$cluster[i] == 1)
  {
    num_1 <- num_1 + 1
  }
  
  else if(initia$cluster[i] == 2)
  {
    num_2 <- num_2 + 1
  }
  
  else if(initia$cluster[i] == 3)
  {
    num_3 <- num_3 + 1
  }
  
  else
  {
    num_4 <- num_4 + 1
  }
}

#num_1
#num_2
#num_3
#num_4

cluster_1 <- matrix(, nrow = 0, ncol = 3)
cluster_2 <- matrix(, nrow = 0, ncol = 3)
cluster_3 <- matrix(, nrow = 0, ncol = 3)
cluster_4 <- matrix(, nrow = 0, ncol = 3)

for(o in 1:(M-N+1))
{	
  if(initia$cluster[c(o)] == 1)
  {
    cluster_1 <- rbind(cluster_1, train[o, 1:3])
  }
  
  else if(initia$cluster[c(o)] == 2)
  {
    cluster_2 <- rbind(cluster_2, train[o, 1:3])
  }
  
  else if(initia$cluster[c(o)] == 3)
  {
    cluster_3 <- rbind(cluster_3, train[o, 1:3])
  }
  
  else
  {
    cluster_4 <- rbind(cluster_4, train[o, 1:3])
  }
}
#str(cluster_1)
#str(cluster_2)
cov_1 <- cov(cluster_1)
cov_2 <- cov(cluster_2)
cov_3 <- cov(cluster_3)
cov_4 <- cov(cluster_4)

cor_1 <- cov2cor(cov_1)
cor_2 <- cov2cor(cov_2)
cor_3 <- cov2cor(cov_3)
cor_4 <- cov2cor(cov_4)
#str(cov_1)

covare <- simplify2array(list(cov_1, cov_2, cov_3, cov_4))
#str(covare)

model <- initGHMM(6, 1)
model <- learnEM(model, t(vola), iter=200, delta= 1E-5, print = FALSE)
#print(model)

llk_train <- loglikelihood(model, t(vola))
llk_test <- loglikelihood(model, t(vola_test))
#str(llk_train)
#str(llk_test)
predicted <- generateObservations(model, (T-M))
#print(predicted)
#.prim_1 <- matrix(, nrow=3, ncol=0)
#.prim_1 <- cbind(((predicted$Y[,1]/100) + 1)*data$Open[M+1], prim_1)
#str(prim_1)
#str(data$Open[M+1])
#.prim_2 <- matrix(, nrow=1, ncol=0)
#.prim_2 <- cbind(data$Open[M+1], prim_2)
#str(prim_2)

#.final <- matrix(, nrow=4, ncol=0)
#.final <-cbind(rbind(prim_2, prim_1), final)

#.for(j in 1:(T-M-1))
#.{
#.  prim_loop <- matrix(, nrow=3, ncol=0)
#.  open <- final[2,j]
  
#.  if(final[2,j] > final[1,j]) open <- (final[2,j] + final[3,j])/2
#.  else
#. 	  {
#.  	  open <- (final[2,j] + final[4,j])/2
#.  	}
  
#.  prim_loop <- cbind(((predicted$Y[,(j+1)]/100) + 1)*open, prim_loop)
#.  final <- cbind(final, rbind(open, prim_loop))
#.}

#str(final)
#.plot(test_plot[, 1], type = "l", col = "green")
#.lines(final[1,], type = "l", col = "red")
#.plot(final[2,], type = "l", col = "red")
#.lines(test_plot[, 2], type = "l", col = "green")
#.plot(final[3,], type = "l", col = "red")
#.lines(test_plot[, 3], type = "l", col = "green")
#.plot(final[4,], type = "l", col = "red")
#.lines(test_plot[, 4], type = "l", col = "green")
me = mean(vola_test - predicted$Y[1, 1:60])

plot(abs(vola_test - predicted$Y[1, 1:60]), type = "l", col = "green")
#lines(predicted$Y[1, 1:60], type = "l", col = "red")


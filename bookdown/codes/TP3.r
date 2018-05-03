rhastib <- function(n.train = 100, 
                    n.test = 100, 
                    n.subclass = 10, 
                    sigma2 = .1){
  Id <- diag(1, nrow = 2) #matrice identité

  n.train <- floor(n.train/2)
  n.test <- floor(n.test/2)  
  n <- n.train + n.test

  # simulation des données "bleues"
  subclass0 <- MASS::mvrnorm(n.subclass, mu = c(1,0), Sigma = Id)
  m0 <- subclass0[sample(1:n.subclass, n, replace = TRUE),]
  class0 <-  m0 + MASS::mvrnorm(n, mu = c(0,0), Sigma = sigma2*Id)
  
  # simulation des données "orange"  
  subclass1 <- MASS::mvrnorm(n.subclass, mu = c(0,1), Sigma = Id)
  m1 <- subclass1[sample(1:n.subclass, n, replace = TRUE),]
  class1 <-  m1 + MASS::mvrnorm(n, mu = c(0,0), Sigma = sigma2*Id) 
  
  # creation des données train et test
  x.train <- rbind(class0[1:n.train,], class1[1:n.train,])
  x.test  <- rbind(class0[(n.train+1):n,], class1[(n.train+1):n,])

  # creation des classes train et test
  type.train <- rep(c("lightblue","orange"), each = n.train)  
  type.test <- rep(c("lightblue","orange"), each = n.test)
  
  return(list(train = x.train, 
              test = x.test, 
              class.train = type.train, 
              class.test = type.test))
}
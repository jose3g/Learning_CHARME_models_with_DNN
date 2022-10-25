#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#---------------  Multivariate Normality Tests (MVN) ---------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

setClass("mardia",
         slots = c(g1p = "numeric", chi.skew="numeric", p.value.skew="numeric",
                   chi.small.skew="numeric", p.value.small="numeric", 
                   g2p="numeric", z.kurtosis="numeric", p.value.kurt="numeric", 
                   dname="character", dataframe="data.frame"))


setGeneric("mardia", function(object) standardGeneric("mardia"))


setMethod("show",
          signature = "mardia",
          definition = function(object) {
            n=dim(object@dataframe)[1]
            cat("   Mardia's Multivariate Normality Test", "\n", sep = " ")
            cat("---------------------------------------", "\n", sep = " ")
            cat("   data :", object@dname, "\n\n", sep = " ")
            cat("   g1p            :", object@g1p, "\n", sep = " ")
            cat("   chi.skew       :", object@chi.skew, "\n", sep = " ")
            cat("   p.value.skew   :", object@p.value.skew, "\n\n", sep = " ")
            cat("   g2p            :", object@g2p, "\n", sep = " ")
            cat("   z.kurtosis     :", object@z.kurtosis, "\n", sep = " ")
            cat("   p.value.kurt   :", object@p.value.kurt, "\n\n", sep = " ")
            cat("   chi.small.skew :", object@chi.small.skew, "\n", sep = " ")
            cat("   p.value.small  :", object@p.value.small, "\n\n", sep = " ")
            if(n>=20){
              cat(if((object@p.value.skew > 0.05) & (object@p.value.kurt > 0.05)){"   Result         : Data are multivariate normal."}
                  else {"   Result          : Data are not multivariate normal."},"\n")
            }
            if(n<20){
              cat(if((object@p.value.small > 0.05) & (object@p.value.kurt > 0.05)){"   Result         : Data are multivariate normal."}
                  else {"   Result          : Data are not multivariate normal."},"\n")
            }
            cat("---------------------------------------", "\n\n", sep = " ")
            
            invisible(NULL)
          })


setClass("hz",
         slots = c(HZ = "numeric", p.value="numeric", dname="character", 
                   dataframe="data.frame"))


setGeneric("hz", function(object) standardGeneric("hz"))


setMethod("show",
          signature = "hz",
          definition = function(object) {
            cat("  Henze-Zirkler's Multivariate Normality Test", "\n", sep = " ")
            cat("---------------------------------------------", "\n", sep = " ")
            cat("  data :", object@dname, "\n\n", sep = " ")
            cat("  HZ      :", object@HZ, "\n", sep = " ")
            cat("  p-value :", object@p.value, "\n\n", sep = " ")
            cat(if(object@p.value > 0.05){"  Result  : Data are multivariate normal."}
                else {"  Result  : Data are not multivariate normal."},"\n")
            cat("---------------------------------------------", "\n\n", sep = " ")
            invisible(NULL)
          })



setClass("royston",
         slots = c(H = "numeric", p.value="numeric", dname="character", 
                   dataframe="data.frame"))


setGeneric("royston", function(object) standardGeneric("royston"))


setMethod("show",
          signature = "royston",
          definition = function(object) {
            cat("  Royston's Multivariate Normality Test", "\n", sep = " ")
            cat("---------------------------------------------", "\n", sep = " ")
            cat("  data :", object@dname, "\n\n", sep = " ")
            cat("  H       :", object@H, "\n", sep = " ")
            cat("  p-value :", object@p.value, "\n\n", sep = " ")
            cat(if(object@p.value > 0.05){"  Result  : Data are multivariate normal."}
                else {"  Result  : Data are not multivariate normal."},"\n")
            cat("---------------------------------------------", "\n\n", sep = " ")
            invisible(NULL)
          })

# ------------------------------------------------------------------------------

hzTest <-function (data, cov = TRUE, qqplot = FALSE){
  
    if (dim(data)[2] < 2 || is.null(dim(data))) {stop("number of variables must be equal or greater than 2")}
    if (!is.data.frame(data) && !is.matrix(data)) stop('Input must be one of classes \"data frame\" or \"matrix\"')
    
    dataframe=as.data.frame(data)
    dname <- deparse(substitute(data))
    data <- data[complete.cases(data),]
    data <- as.matrix(data)
    n <- dim(data)[1]
    p <- dim(data)[2]
    data.org = data
    
    if (cov){ 
      S <- ((n-1)/n)*cov(data)
    }
    else    {  
      S <- cov(data)
    }
    
    dif <- scale(data, scale = FALSE)
    
    
    Dj <- diag(dif%*%solve(S)%*%t(dif))  #squared-Mahalanobis' distances
    
    Y <- data%*%solve(S)%*%t(data)
    
    
    Djk <- - 2*t(Y) + matrix(diag(t(Y)))%*%matrix(c(rep(1,n)),1,n) + matrix(c(rep(1,n)),n,1)%*%diag(t(Y))
    
    b <- 1/(sqrt(2))*((2*p + 1)/4)^(1/(p + 4))*(n^(1/(p + 4))) #smoothing
    {                                                                 #parameter
      if (qr(S)$rank == p){    
        HZ = n * (1/(n^2) * sum(sum(exp( - (b^2)/2 * Djk))) - 2*
                    ((1 + (b^2))^( - p/2)) * (1/n) * (sum(exp( - ((b^2)/(2*
                                                                           (1 + (b^2)))) * Dj))) + ((1 + (2 * (b^2)))^( - p/2)))
      }
      else {
        HZ = n*4
      }  
      
    }
    wb <- (1 + b^2)*(1 + 3*b^2)
    
    a <- 1 + 2*b^2
    
    mu <- 1 - a^(- p/2)*(1 + p*b^2/a + (p*(p + 2)*(b^4))/(2*a^2)) #HZ mean
    
    si2 <- 2*(1 + 4*b^2)^(- p/2) + 2*a^( - p)*(1 + (2*p*b^4)/a^2 + (3*p*
                                                                      (p + 2)*b^8)/(4*a^4)) - 4*wb^( - p/2)*(1 + (3*p*b^4)/(2*wb) + (p*
                                                                                                                                       (p + 2)*b^8)/(2*wb^2)) #HZ variance
    
    pmu <- log(sqrt(mu^4/(si2 + mu^2))) #lognormal HZ mean
    psi <- sqrt(log((si2 + mu^2)/mu^2)) #lognormal HZ variance
    
    P <- 1 - plnorm(HZ,pmu,psi) #P-value associated to the HZ statistic
    
    
    
    if (qqplot){    
      d <- Dj    
      r <- rank(d)  
      chi2q <- qchisq((r-0.5)/n,p)
      plot(d, chi2q, pch = 19, main = "Chi-Square Q-Q Plot",
           xlab = "Squared Mahalanobis Distance",ylab="Chi-Square Quantile")
      abline(0, 1,lwd = 2, col = "black")
    }
    
    
    result <- new("hz", HZ = HZ, p.value = P, dname = dname, dataframe = dataframe)
    
    result
  }
#--------------------------------------------------------------------------------
  
mardiaTest <-function (data, cov = TRUE, qqplot = FALSE){
    if (dim(data)[2] < 2 || is.null(dim(data))) {stop("number of variables must be equal or greater than 2")}
    
    if (!is.data.frame(data) && !is.matrix(data)) stop('Input must be one of classes \"data frame\" or \"matrix\"')
    
    dataframe=as.data.frame(data)
    dname <- deparse(substitute(data))
    data <- data[complete.cases(data),]
    data <- as.matrix(data)
    n <- dim(data)[1]
    p <- dim(data)[2]
    data.org <- data
    data <- scale(data, scale = FALSE)
    if (cov) {
      S <- ((n - 1)/n) * cov(data)
    }
    else {
      S <- cov(data)
    }
    D <- data %*% solve(S) %*% t(data)
    g1p <- sum(D^3)/n^2
    g2p <- sum(diag((D^2)))/n
    df <- p * (p + 1) * (p + 2)/6
    k <- (p + 1) * (n + 1) * (n + 3)/(n * ((n + 1) * (p + 1) - 
                                             6))
    small.skew <- n * k * g1p/6
    skew <- n * g1p/6
    kurt <- (g2p - p * (p + 2)) * sqrt(n/(8 * p * (p + 2)))
    p.skew <- pchisq(skew, df, lower.tail = FALSE)
    p.small <- pchisq(small.skew, df, lower.tail = FALSE)
    p.kurt <- 2 * (1 - pnorm(abs(kurt)))
    if (qqplot) {
      d <- diag(D)
      r <- rank(d)
      chi2q <- qchisq((r - 0.5)/n, p)
      plot(d, chi2q, pch = 19, main = "Chi-Square Q-Q Plot",
           xlab = "Squared Mahalanobis Distance", ylab = "Chi-Square Quantile")
      abline(0, 1, lwd = 2, col = "black")
    }
    result <- new("mardia", g1p = g1p, chi.skew = skew, p.value.skew = p.skew,
                  chi.small.skew = small.skew, p.value.small = p.small, g2p = g2p,
                  z.kurtosis = kurt, p.value.kurt = p.kurt, dname = dname, dataframe = dataframe)
    
    result
}

#-------------------------------------------------------------------------------

roystonTest <- function (data, qqplot = FALSE){
    if (dim(data)[2] < 2 || is.null(dim(data))) {stop("number of variables must be equal or greater than 2")}
    
    if (!is.data.frame(data) && !is.matrix(data)) stop('Input must be one of classes \"data frame\" or \"matrix\"')
    
    dataframe=as.data.frame(data)
    dname <- deparse(substitute(data))
    data <- data[complete.cases(data),]
    data <- as.matrix(data)
    p <- dim(data)[2]
    n <- dim(data)[1]
    z <- matrix(nrow <- p, ncol = 1)
    z <- as.data.frame(z)
    w <- matrix(nrow <- p, ncol = 1)
    w <- as.data.frame(w)
    data.org <- data    
    if (n <= 3) {
      stop("n must be greater than 3")
    }
    else if (n >= 4 || n <= 11) {
      x <- n
      g <- -2.273 + 0.459 * x
      m <- 0.544 - 0.39978 * x + 0.025054 * x^2 - 0.0006714 * 
        x^3
      s <- exp(1.3822 - 0.77857 * x + 0.062767 * x^2 - 0.0020322 * 
                 x^3)
      for (i in 1:p) {
        a2 <- data[, i]
        {
          if (kurtosis(a2) > 3) {
            w <- sf.test(a2)$statistic
          }
          else {
            w <- shapiro.test(a2)$statistic
          }
        }
        z[i, 1] <- (-log(g - (log(1 - w))) - m)/s
      }
    }
    if (n > 2000) {
      stop("n must be less than 2000")
    }
    else if (n >= 12 || n <= 2000) {
      x <- log(n)
      g <- 0
      m <- -1.5861 - 0.31082 * x - 0.083751 * x^2 + 0.0038915 * 
        x^3
      s <- exp(-0.4803 - 0.082676 * x + 0.0030302 * x^2)
      for (i in 1:p) {
        a2 <- data[, i]
        {
          if (kurtosis(a2) > 3) {
            w <- sf.test(a2)$statistic
          }
          else {
            w <- shapiro.test(a2)$statistic
          }
        }
        z[i, 1] <- ((log(1 - w)) + g - m)/s
      }
    }
    else {
      stop("n is not in the proper range")
    }
    u <- 0.715
    v <- 0.21364 + 0.015124 * (log(n))^2 - 0.0018034 * (log(n))^3
    l <- 5
    C <- cor(data)
    NC <- (C^l) * (1 - (u * (1 - C)^u)/v)
    T <- sum(sum(NC)) - p
    mC <- T/(p^2 - p)
    edf <- p/(1 + (p - 1) * mC)
    Res <- matrix(nrow = p, ncol = 1)
    Res <- as.data.frame(Res)
    for (i in 1:p) {
      Res <- (qnorm((pnorm(-z[, ]))/2))^2
    }
    data <- scale(data, scale = FALSE)
    Sa <- cov(data)
    D <- data %*%solve(Sa)%*% t(data)
    
    if (qqplot) {    
      d <- diag(D)    
      r <- rank(d)  
      chi2q <- qchisq((r-0.5)/n,p)
      plot(d, chi2q, pch = 19, main = "Chi-Square Q-Q Plot", xlab = "Squared Mahalanobis Distance", ylab = "Chi-Square Quantile")
      abline(0, 1,lwd = 2, col = "black")
    }
    
    RH <- (edf * (sum(Res)))/p
    pv <- pchisq(RH, edf, lower.tail = FALSE)
    
    result <- new("royston", H = RH, p.value = pv, dname = dname, dataframe = dataframe)
    
    result
  }

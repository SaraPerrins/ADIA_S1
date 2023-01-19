library(CVXR)

find.weights <- function(phi_X, lambda=0) {
  #https://web.stanford.edu/~boyd/papers/pdf/cvxr_paper.pdf
  #  see page 18  Kelly gambling
  n             <-  dim(phi_X)[1]
  
  w             <- Variable(n)
  
  objtive       <- Minimize(sum((t(phi_X)%*%w )^2) + lambda*n*sum(w^2))
  
  constraints   <- list(w >= 0,sum(w)==1)
  
  problem       <- Problem(objtive,constraints = constraints)
  
  result        <- solve(problem)
  
  print(result$status)
  slw <- round(result$getValue(w),4)
  return(slw)
}
##test editing Ye
#the test worked!!!
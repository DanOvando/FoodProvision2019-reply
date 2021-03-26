#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector sim_mpa(NumericVector r, NumericVector k, NumericVector m, NumericVector u, NumericVector p_mpa,
                      int local_dd,int years) {
  
  
  int n = r.size();

  NumericVector mu(n,0.0);
  
  NumericVector yield(n,0.0);

  NumericVector hmpa(n,0.0);
  
  NumericVector inside_b(n,0.0);
  
  NumericVector last_inside_b(n,0.0);
  
  NumericVector outside_b(n,0.0);
  
  NumericVector last_outside_b(n,0.0);
  
  last_inside_b = k * p_mpa;
    
  last_outside_b = k * (1 - p_mpa);
  
  mu = m * (1 - p_mpa);
    
  for (int t = 0; t < years; t++){
    
    if (local_dd == 1){
      
      inside_b =
       last_inside_b +  r * last_inside_b * (1 - last_inside_b / (k * p_mpa)) - mu * (last_inside_b - (p_mpa / (1 - p_mpa)) * last_outside_b);
      
      outside_b =
       (1 - u) * last_outside_b +  r * last_outside_b * (1 - last_outside_b / (k * (1 - p_mpa))) + mu * (last_inside_b - (p_mpa / (1 - p_mpa)) * last_outside_b);
      
      last_inside_b = inside_b;
      
      last_outside_b = outside_b;
        
    } else {
      
      
      inside_b =
        last_inside_b +  p_mpa * r * (last_inside_b + last_outside_b)  * (1 - (last_inside_b + last_outside_b) / (k)) - mu * (last_inside_b - (p_mpa / (1 - p_mpa)) * last_outside_b);
      
      outside_b =
        (1 - u) * last_outside_b +  (1 - p_mpa) * r * (last_inside_b + last_outside_b) * (1 - (last_inside_b + last_outside_b) / (k)) + mu * (last_inside_b - (p_mpa / (1 - p_mpa)) * last_outside_b);
      
      last_inside_b = inside_b;
      
      last_outside_b = outside_b;
      
    
    }
 
 
  yield = u * outside_b;
 
  }
  
  // return Rcpp::List::create(
  //   Rcpp::Named("inside_b") = inside_b,
  //   Rcpp::Named("outside_b") = outside_b,
  //   Rcpp::Named("yield") = yield
  // );
  
  return(yield);
  

}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
library(tidyverse)
r <- 0.4

k <- 1000

m <- 0.1

p_mpa <- 1e-3

u <- r / 2

years <- 100



popfoo <- function(r,k,m,u,mpa, years = 100, dd = 0) {

  b <- matrix(NA, nrow = years, ncol = 2)

  b[1, ] <- c(k * mpa, k * (1 - mpa))

  mu <- m * (1 - mpa)

  h <- rep(NA, years)

  for (i in 2:years) {


    if (dd == 0){
      # inside MPA
      b[i, 1] <-
        b[i - 1, 1] + mpa * r * (sum(b[i - 1, ])) * (1 - sum(b[i - 1, ]) / k) - mu * (b[i - 1, 1] - (mpa / (1 - mpa)) * b[i - 1, 2])


      # outside MPA

      b[i, 2] <-
        (1 - u) * b[i - 1, 2] + (1 - mpa) * r * (sum(b[i - 1, ])) * (1 - sum(b[i - 1, ]) / k) + mu * (b[i - 1, 1] - (mpa / (1 - mpa)) * b[i - 1, 2])


    } else {


      
      move <- -m * b[i - 1, 1] * (b[i - 1, 1] / (k * mpa) - b[i - 1, 2] / (k * (1 - mpa))) + m * b[i - 1, 2] * (b[i - 1, 2] / (k * (1 - mpa)) - b[i - 1, 1] / (k *  mpa))
      
      # b[i, 1] <-
      #   b[i - 1, 1] +  r * b[i - 1,1] * (1 - b[i - 1, 1] / max(1e-3,(k * mpa))) - mu * (b[i - 1, 1] - (mpa / (1 - mpa)) * b[i - 1, 2])


      # outside MPA

      # b[i, 2] <-
      #   (1 - u) * b[i - 1, 2] + r * b[i - 1, 2] * (1 - b[i - 1 , 2] / max(1e-3,(k * (1 - mpa)))) + mu * (b[i - 1, 1] - (mpa / (1 - mpa)) * b[i - 1, 2])
      # 
      # 

      b[i, 1] <-
        b[i - 1, 1] +  r * b[i - 1,1] * (1 - b[i - 1, 1] / max(1e-3,(k * mpa))) + move

      
      # outside MPA
      
      b[i, 2] <-
        (1 - u) * b[i - 1, 2] + r * b[i - 1, 2] * (1 - b[i - 1 , 2] / max(1e-3,(k * (1 - mpa)))) - move
      
      
      
      
    }

    h[i-1] <- u *  b[i - 1, 2]

  }

  h[i] <- u * b[i,2]


  check <- ((m * k * (1 - mpa)) / (u * mpa + m)) * (1 - (u * (1 - mpa) * m)/((u * mpa + m) * r))

  check / b[years,2]

  out <- b %>%
    data.frame() %>%
    mutate(year = 1:years)

  colnames(out) <- c("inside",'outside',"year")

  out$h <- h

  return(out)

  # out %>%
  #   ggplot(aes(1:nrow(.), inside)) +
  #   geom_line()

}

a <- popfoo(r = r,k =k,m = m,u = u,mpa = p_mpa,dd = 1, years = years)

a[years,]

a$inside %>% plot()

a$inside[years] / (k * p_mpa)

sim_mpa(r,k,m,u,p_mpa,1,years)

b = sim_mpa(rep(r,2000),rep(k,2000),rep(m,2000),rep(u,2000),rep(p_mpa,2000),0,years)

mean(b / ((r * k)/4))

# mean((b$inside_b) / (k * (p_mpa)))


# b_outside_bau <-
#   ((m * k * (1 - p_mpa)) / (u * p_mpa + m)) * (1 - (u * (1 - p_mpa) * m) /
#                                                        ((u + m) * r))
# last(a$outside) / b_outside_bau

*/

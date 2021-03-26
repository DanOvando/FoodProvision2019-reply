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
NumericMatrix timesTwo(NumericVector x, int z) {
  
  int n = x.size();
  
  NumericMatrix y(n,2);
  
  y(_,1) = x;
  
  if (z == 0){
    std::cout<< "hello"<< std::endl;
  } else {
    
    std::cout<< "bvlah"<< std::endl;
    
  }
  
  return y;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(c(1,2,3),1)
*/

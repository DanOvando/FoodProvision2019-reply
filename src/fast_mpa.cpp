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
double fast_mpa(NumericVector MPAselect, 
                int celltoiterate,
                double HBAU,
                NumericVector MPAselectPrev, 
                NumericVector KprotectedPerCell,
                NumericVector ER,
                NumericVector m, 
                NumericVector R,
                NumericVector r,
                NumericVector K) {
  
  // MPAselect[celltoiterate] = 1;
  
  R = MPAselectPrev+KprotectedPerCell;
  
  NumericVector ER_redistribute(ER.size());
  
  for (int i = 0; i < ER.size(); i++){
    
     ER_redistribute(i) = 1 - pow(1-ER(i),1/(1-R(i)));
    
  }
  
  NumericVector hmpa = (ER_redistribute*((m*K*(1-R))/((ER_redistribute*R)+m))*(1-((ER_redistribute*(1-R)*m)/(((ER_redistribute*R)+m)*r))));
  
  for (int i = 0; i < hmpa.size(); i++){

    hmpa(i) = hmpa(i) * (hmpa(i) > 0);

  }
  
  double HMPA = sum(hmpa);
  
  
  double out = HMPA - HBAU;

  
  return(out);
}



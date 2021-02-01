// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
List simp_lin_cpp(arma::vec x, arma::vec y){
  int n = x.n_elem;
  arma::vec center_x = x-mean(x);
  arma::vec center_y = y-mean(y);
  double Sxx = arma::as_scalar(center_x.t() * center_x);

  double beta1 = arma::as_scalar((center_x.t() * center_y) / Sxx);
  double beta0 = mean(y) - beta1*mean(x);
  arma::colvec yhat = beta1*x+beta0; // predictions
  arma::colvec residuals = y-yhat; //residuals

  double MSres = sum(square(residuals))/ (n-2);

  double se_beta1 = sqrt(MSres/Sxx); //standard error of beta1
  double se_beta0 = sqrt(MSres*(1/n+ pow(mean(x),2)/Sxx)); //standard error of beta0
  double quan_t = R::qt(0.975,n-2,true,false);
  arma::rowvec ci_beta1 = {beta1-quan_t*se_beta1,beta1+quan_t*se_beta1}; //CI of Beta1
  arma::rowvec ci_beta0 = {beta0-quan_t*se_beta1,beta0+quan_t*se_beta1}; //CI of Beta0

  return List::create(Named("Coefficients (Beta0,Beta1)") = arma::colvec {beta0,beta1},
                      Named("Residuals") = residuals,
                      Named("Predictions") = yhat,
                      Named("Standard Errors (Beta0,Beta1)") = arma::colvec {se_beta0,se_beta1},
                      Named("CI of Beta0") = ci_beta0,
                      Named("CI of Beta1") = ci_beta1);
}









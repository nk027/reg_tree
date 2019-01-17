#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
NumericVector get_var_stat(arma::rowvec & y, arma::mat & X, arma::vec & Z, double min_obs){
  int num_vals = Z.size();
  NumericVector var_stat(num_vals);
  int i;
  int nn = X.n_cols - 1;
  for(i = 0; i<num_vals; i++){
    arma::colvec coef_gre, coef_leq, resid_gre, resid_leq;
    arma::colvec diff;
    arma::mat vcov_gre, vcov_leq;
    arma::mat covsum;
    double sig2_gre, sig2_leq, statistic;
    double val = Z(i);
    arma::uvec leq_i = find(Z <= val);
    arma::uvec gre_i = find(Z > val);
    
    arma::mat X_gre = X.rows(gre_i);
    arma::colvec y_gre = y(gre_i);
    arma::mat X_leq = X.rows(leq_i);
    arma::colvec y_leq = y(leq_i);
    
    int n_gre = X_gre.n_rows, k_gre = X_gre.n_cols;
    int n_leq = X_leq.n_rows, k_leq = X_leq.n_cols;
    
    if(n_gre > min_obs && n_leq > min_obs){
      coef_gre = arma::solve(X_gre, y_gre);
      coef_leq = arma::solve(X_leq, y_leq);
      
      resid_gre = y_gre - X_gre*coef_gre; 
      resid_leq = y_leq - X_leq*coef_leq;
      
      sig2_gre = arma::as_scalar(resid_gre.t() * resid_gre)/(n_gre-k_gre);
      sig2_leq = arma::as_scalar(resid_leq.t() * resid_leq)/(n_leq-k_leq);
      
      vcov_gre = sig2_gre * arma::inv(X_gre.t() * X_gre);
      vcov_leq = sig2_leq * arma::inv(X_leq.t() * X_leq);
      diff = coef_leq - coef_gre;
      covsum = vcov_leq + vcov_gre;
      statistic = arma::as_scalar(diff.t() * covsum.i() * diff);
      var_stat[i] = statistic;
    }else{
      var_stat[i] = NumericVector::get_na();
    }
  }
  return var_stat;
}


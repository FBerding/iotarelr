// This file is part of the R package "iotarelr".
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License version 3 as published by
// the Free Software Foundation.
//
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>

#include <Rcpp.h>
using namespace Rcpp;

//'Generating randomly chosen probabilities for Assignment Error Matrix
//'
//'Function written in \code{C++} for generating a set of randomly chosen
//'probabilities for the Assignment Error Matrix.
//'@param n_categories Integer for the number of categories in the data. Must be
//'at least 2.
//'@return Returns a matrix for Assignment Error Matrix (AEM) with randomly
//'generated probabilities. The generated probabilities are in line with
//'the assumption of weak superiority.
//'@importFrom Rcpp sourceCpp
//'@useDynLib iotarelr
// [[Rcpp::export]]
Rcpp::NumericMatrix get_random_start_values_p(int n_categories){
  Rcpp::NumericMatrix random_aem(n_categories,n_categories);
  Rcpp::NumericVector p_range(100);
  Rcpp::NumericVector sample_vec;
  Rcpp::NumericVector diagonal_vec;
  Rcpp::NumericVector start_range;

  int i=0,j=0;
  double rowSum=0.0;

  start_range=seq(50,100);

  for(i=0; i<n_categories; i++){
    diagonal_vec=sample(start_range,
                        n_categories,
                        true,
                        R_NilValue);
     for(j=0; j<n_categories; j++){
      p_range=Rcpp::seq(1,diagonal_vec(0));
      sample_vec=Rcpp::sample(p_range,2,true,R_NilValue);
      if(i==j){
        random_aem(i,j)=diagonal_vec(0);
      } else {
        random_aem(i,j)=sample_vec[0];
      }
    }
    rowSum=Rcpp::sum(random_aem(i,_));

    for(j=0; j<n_categories; j++){
      random_aem(i,j)=random_aem(i,j)/rowSum;
    }

  }
  return random_aem;
}


//'Generating randomly chosen probabilities for categorical sizes
//'
//'Function written in \code{C++} for generating a set of randomly chosen
//'probabilities describing the size of the different classes. The
//'probabilities describe the relative frequencies of the categories in the data.
//'@param n_categories Integer for the number of categories in the data. Must be
//'at least 2.
//'@return Returns a vector of randomly chosen categorical sizes.
//'@importFrom Rcpp sourceCpp
//'@useDynLib iotarelr
// [[Rcpp::export]]
Rcpp::NumericVector get_random_start_values_class_sizes(int n_categories){
  Rcpp::NumericVector random_class_sizes(n_categories);
  Rcpp::NumericVector p_range(100);
  Rcpp::NumericVector sample_vec;
  int i=0;
  double rowSum=0.0;

  p_range=Rcpp::seq(1,100);

  for(i=0; i<n_categories; i++){
    sample_vec=Rcpp::sample(p_range,2,true,R_NilValue);
    random_class_sizes(i)=sample_vec[0];
    }
  rowSum=Rcpp::sum(random_class_sizes);
  for(i=0; i<n_categories; i++){
      random_class_sizes(i)=random_class_sizes(i)/rowSum;

    }

  return random_class_sizes;
}

//' Estimating log-likelihood
//'
//' Function written in \code{C++} estimating the log likelihood of a given
//' parameter set.
//'
//' @param categorial_sizes \code{Vector} containing the sizes of the different
//' categories. That is amount of a category on all cases.
//' @param aem \code{Matrix} in aem form. This matrix reports the true category
//' in the rows and the assigned categories in the columns. The cells represent
//' the probabilities that a coding unit of category i is assigned to category j.
//' @param obs_pattern_shape \code{Matrix} containing the unique patterns found
//' in the data. Ideally this matrix is generated by the function
//' \code{get_patterns()}.
//' @param obs_pattern_frq \code{Vector} containing the frequencies of the
//' patterns. Ideally it is generated by the the function
//' \code{get_patterns()}.
//' @param categorical_levels \code{Vector} containing all possible categories of
//' the content analysis.
//' @return Returns the log likelihood as a single numeric value.
//'@references  Berding, Florian, and Pargmann, Julia (2022).Iota Reliability Concept
//'of the Second Generation.Measures for Content Analysis Done by
//'Humans or Artificial Intelligences. Berlin: Logos.
//'https://doi.org/10.30819/5581
//'@importFrom Rcpp sourceCpp
//'@useDynLib iotarelr
// [[Rcpp::export]]
double fct_log_likelihood_c(Rcpp::NumericVector categorial_sizes,
                              Rcpp::NumericMatrix aem,
                              Rcpp::CharacterMatrix obs_pattern_shape,
                              Rcpp::NumericVector obs_pattern_frq,
                              Rcpp::CharacterVector categorical_levels){

 double log_ll=0.0;
 double tmp_pattern_p=0.0;
 double tmp_k_p=1.0;
 int i=0;
 int j=0;
 int k=0;
 String string_k;
 String string_j;

 CharacterVector aem_colnames=colnames(aem);
 CharacterVector aem_rownames=rownames(aem);

  for(i=0;i<obs_pattern_shape.nrow();i++){
    tmp_pattern_p=0.0;
    for(k=0; k<categorical_levels.length();k++){
      string_k=categorical_levels(k);
      tmp_k_p=1.0;

      for(j=0;j<obs_pattern_shape.ncol();j++){
        string_j=obs_pattern_shape(i,j);
        tmp_k_p=tmp_k_p*aem(categorial_sizes.findName(string_k),categorial_sizes.findName(string_j));
      }

      tmp_k_p=tmp_k_p*categorial_sizes(categorial_sizes.findName(string_k));
      tmp_pattern_p=tmp_pattern_p+tmp_k_p;
    }
    log_ll=log_ll+log(tmp_pattern_p)*obs_pattern_frq(i);
  }
  return(-log_ll);
}

//' Estimating log-likelihood in Condition Stage
//'
//' Function written in \code{C++} estimating the log likelihood of a given
//' parameter set during the condition stage.
//'
//' @param probabilities \code{NumericVector} containing the probabilities of
//' a multinominal distribution. In the context of Iota Reliability this refers
//' to a specific row of the Assignment Error Matrix.
//' @param observations \code{NumericVector} containing the number of
//' observations for each category of the multinominal distribution.
//' @return Returns the log likelihood as a single numeric value.
//'@importFrom Rcpp sourceCpp
//'@useDynLib iotarelr
//'@references  Berding, Florian, and Pargmann, Julia (2022).Iota Reliability Concept
//'of the Second Generation.Measures for Content Analysis Done by
//'Humans or Artificial Intelligences. Berlin: Logos.
//'https://doi.org/10.30819/5581
// [[Rcpp::export]]
double log_likelihood_multi_c(Rcpp::NumericVector probabilities,
                              Rcpp::NumericVector observations){
  double log_likelihood=0.0;
  int n_categories=probabilities.length();
  int i=0;

  for(i=0;i<n_categories;i++){
    if(observations(i)>0){
      log_likelihood=log_likelihood+observations(i)*log(probabilities(i));
    } //else {
      //log_likelihood=log_likelihood;
    //}
  }
  log_likelihood=-log_likelihood;
    return(log_likelihood);
}

//' Gradient for Log Likelihood in Condition Stage
//'
//' Function written in \code{C++} estimating the gradient of the log likelihood
//' function for a given parameter set and given observations.
//'
//' @param param_values \code{NumericVector} containing the probabilities of
//' a multinominal distribution. The length of this factor is the number of
//' categories - 1 since it contains only the parameters to be estimated.
//' @param observations \code{NumericVector} containing the number of
//' observations for each category of the multinominal distribution. The length
//' of this vector equals the number of categories.
//' @return Returns the gradient as a \code{NumericVector}.
//'@importFrom Rcpp sourceCpp
//'@useDynLib iotarelr
//'@references  Berding, Florian, and Pargmann, Julia (2022).Iota Reliability Concept
//'of the Second Generation.Measures for Content Analysis Done by
//'Humans or Artificial Intelligences. Berlin: Logos.
//'https://doi.org/10.30819/5581
// [[Rcpp::export]]
Rcpp::NumericVector grad_ll(Rcpp::NumericVector param_values,
                            Rcpp::NumericVector observations){
  int n_parameters=param_values.length();
  int n_categories=n_parameters+1;
  Rcpp::NumericVector grad(n_parameters);
  int i=0;

  for(i=0;i<n_parameters;i++){
    grad(i)=-(observations(i)/param_values(i)-observations(n_categories-1)/(1-sum(param_values)));
  }
  return(grad);
}

//' Estimating log likelihood in Condition Stage
//'
//' Function written in \code{C++} estimating the log likelihood of a given
//' parameter set during the condition stage.
//'
//' @param observations \code{NumericVector} containing the frequency of the
//' categories.
//' @param anchor \code{Integer} ranging between 1 and the number of categories.
//' Anchor defines the reference category. That is the category with the highest
//' probability according to the assumption of weak superiority.
//' @param max_iter \code{Integer} specifying the maximal number of iterations
//' for each random start.
//' @param n_random_starts \code{Integer} for the number of random start.
//' @param step_size \code{Double} for specifying the size for increasing or
//' decreasing the probabilities during the estimation. This value should not
//' be less than 1e-3.
//' @param cr_rel_change \code{Double} for defining when the estimation should
//' stop. That is, if the change in log-likelihood is smaller as this value the
//' estimation stops.
//' @param fast \code{Bool} If \code{TRUE} a fast estimation is applied. This
//' option ignored all other parameters. If
//' \code{FALSE} the estimation described in Berding and Pargmann (2022) is used.
//' Default is \code{TRUE}.
//' @param trace \code{Bool} \code{TRUE} if information about the progress of
//' estimation should be printed to the console. \code{FALSE} if not desired.
//' @return Returns the log likelihood as a single numeric value.
//' @importFrom Rcpp sourceCpp
//' @useDynLib iotarelr
//'@references  Berding, Florian, and Pargmann, Julia (2022).Iota Reliability Concept
//'of the Second Generation.Measures for Content Analysis Done by
//'Humans or Artificial Intelligences. Berlin: Logos.
//'https://doi.org/10.30819/5581
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector est_con_multinominal_c(Rcpp::NumericVector observations,
                                         int anchor,
                                         int max_iter=500000,
                                         double step_size=1e-4,
                                         double cr_rel_change=1e-12,
                                         int n_random_starts=10,
                                         bool fast=true,
                                         bool trace=false){

  int n_categories=observations.length();
  Rcpp::NumericVector delta=(n_categories-1);
  Rcpp::NumericVector tmp_old(n_categories);
  Rcpp::NumericVector tmp_new(n_categories);
  Rcpp::NumericVector tmp_observations(n_categories);
  Rcpp::NumericMatrix estimates(n_random_starts,(n_categories+1));
  int i=0;
  int j=0;
  int start_iter=0;
  int iter=0;
  double rel_change=0.0;
  double ll_old=0.0;
  double ll_new=0.0;
  double p_anchor_min=1.0/double(n_categories);
  int index_min=0;
  int index_observation_min=0;
  int index_observation_max=0;
  double switch_1=0.0;
  double switch_2=0.0;
  Rcpp::NumericVector grad(n_categories-1);

  Rcpp::NumericVector tmp_result(n_categories);
  Rcpp::NumericVector results(n_categories);
  Rcpp::NumericMatrix tmp_aem;
  Rcpp::NumericVector tmp_p_vector(n_categories-1);
  Rcpp::NumericVector tmp_obs_vector(n_categories-1);

  if(fast==false){
    //Sorting data into the same order for applying the algorithm
    //setting anchor at the first position
    for(i=0;i<n_categories;i++){
      if(i==(anchor-1)){
        tmp_observations(0)=observations(anchor-1);
      } else if(i>(anchor-1)) {
        tmp_observations(i)=observations(i);
      } else if(i<(anchor-1)) {
        tmp_observations(1+i)=observations(i);
      }
    }

    //setting smallest observation to the end
    for(j=1;j<n_categories;j++){
      tmp_obs_vector(j-1)=tmp_observations(j);
    }

    index_observation_min=which_min(tmp_obs_vector)+1;

    switch_1=tmp_observations(index_observation_min);
    switch_2=tmp_observations(n_categories-1);

    tmp_observations(index_observation_min)=switch_2;
    tmp_observations(n_categories-1)=switch_1;

    //Starting parameter estimation
    for(start_iter=0;start_iter<n_random_starts;start_iter++){
      tmp_aem=get_random_start_values_p(n_categories);
      tmp_new=tmp_aem(0,_);

      ll_new=log_likelihood_multi_c(tmp_new,
                                    tmp_observations);
      iter=0;
      rel_change=99999;

      while((rel_change>cr_rel_change) &
            (iter<(max_iter-1)) &
            (ll_new>0)
              ){
        tmp_old=clone(tmp_new);
        ll_old=ll_new;
        for(j=0;j<n_categories-1;j++){
          tmp_p_vector(j)=tmp_old(j);
        }
        grad=grad_ll(tmp_p_vector,
                     tmp_observations);

        for(i=0;i<(n_categories-1);i++){
          if(grad(i)>0){
            delta(i)=-step_size;
          } else if (grad(i)<0){
            delta(i)=step_size;
          } else {
            delta(i)=0;
          }
        }

        for(i=0;i<(n_categories-1);i++){
          if(i==0){
            if((tmp_old(i)+delta(i))<p_anchor_min){
              tmp_new(i)=p_anchor_min;
            } else if ((tmp_old(i)+delta(i))>1){
              tmp_new(i)=1.0;
            } else {
              tmp_new(i)=tmp_old(i)+delta(i);
            }
          } else {
            if((tmp_old(i)+delta(i))<0.0){
              tmp_new(i)=0.0;
            } else if ((tmp_old(i)+delta(i))>tmp_new(0)){
              tmp_new(i)=tmp_new(0);

            } else {
              tmp_new(i)=tmp_old(i)+delta(i);
             }
          }
        }
        for(j=0;j<n_categories-1;j++){
          tmp_p_vector(j)=tmp_new(j);
        }
        tmp_new(n_categories-1)=1-sum(tmp_p_vector);
        ll_new=log_likelihood_multi_c(tmp_new,
                                      tmp_observations);
        rel_change=(ll_old-ll_new)/ll_old;

        if(trace==true){
          Rcout << "Condition Stage Start "<< start_iter+1 <<" Iteration " << iter <<" Log_Likelihood "
                << ll_new << " relative change "<< rel_change << "\n";
        }

        if(ll_new==-2147483648){
          tmp_new=tmp_old;
          ll_new=log_likelihood_multi_c(tmp_new,
                                        tmp_observations);
        }

        if(min(tmp_new)<0){
          tmp_new=tmp_old;
          ll_new=log_likelihood_multi_c(tmp_new,
                                        tmp_observations);
        }

        if((rel_change<0)){
          tmp_new=tmp_old;
          ll_new=log_likelihood_multi_c(tmp_new,
                                        tmp_observations);
        }
        iter=iter+1;
      }


      for(i=0;i<n_categories;i++){
        estimates(start_iter,i)=tmp_new(i);
      }
      estimates(start_iter,n_categories+0)=ll_new;
    }

    index_min=which_min(estimates(_,n_categories));
    tmp_result=estimates(index_min,_);

    //Re-Sorting the data
    switch_1=tmp_result(index_observation_min);
    switch_2=tmp_result(n_categories-1);

    tmp_result(index_observation_min)=switch_2;
    tmp_result(n_categories-1)=switch_1;

    for(i=0;i<n_categories;i++){
      if(i==0){
        results(anchor-1)=tmp_result(i);
      } else if ((i>0) & (i<=(anchor-1))) {
        results(i-1)=tmp_result(i);
      } else if ((i>0) & (i>(anchor-1))){
        results(i)=tmp_result(i);
      }
    }
    return(results);
  } else {
    tmp_observations=clone(observations);
    index_observation_max=Rcpp::which_max(observations);
    if(index_observation_max!=(anchor-1)){
      tmp_observations[(anchor-1)]=observations[index_observation_max];
    }
    results=tmp_observations/Rcpp::sum(tmp_observations);
    return results;
  }
}

//'Check assumptions of weak superiority
//'
//'This function tests if the probabilities within the Assignment Error Matrix
//'are in line with the assumption of weak superiority.
//'
//'@param aem matrix of probabilities
//'@return Returns the number of violations of the assumption of weak superiority.
//'0 if the assumptions are fulfilled.
//'@importFrom Rcpp sourceCpp
//'@useDynLib iotarelr
//'@references  Berding, Florian, and Pargmann, Julia (2022).Iota Reliability Concept
//'of the Second Generation.Measures for Content Analysis Done by
//'Humans or Artificial Intelligences. Berlin: Logos.
//'https://doi.org/10.30819/5581
//'@export
// [[Rcpp::export]]
int check_conformity_c(Rcpp::NumericMatrix aem){
  int n_violations=0;
  int i=0;

  for(i=0;i<aem.nrow();i++){
    if(aem(i,i)<max(aem(i,_))){
      n_violations++;
    }
  }
  return(n_violations);
}

//'Parameter estimation via EM Algorithm with Condition Stage
//'
//'Function written in \code{C++} for estimating the parameters of the model
//'via Expectation Maximization (EM Algorithm).
//'
//' @param obs_pattern_shape \code{Matrix} containing the unique patterns found
//' in the data. Ideally this matrix is generated by the function
//' \code{get_patterns()}.
//' @param obs_pattern_frq \code{Vector} containing the frequencies of the
//' patterns. Ideally it is generated by the the function
//' \code{get_patterns()}.
//' @param obs_internal_count \code{Matrix} containing the relative frequencies
//' of each category within each pattern. Ideally this matrix is generated by
//' the function \code{get_patterns()}.
//' @param categorical_levels \code{Vector} containing all possible categories of
//' the content analysis.
//'@param random_starts \code{Integer} for determining how often the algorithm
//'should restart with randomly chosen values for the Assignment Error Matrix
//'and the categorical sizes.
//'@param max_iterations \code{Integer} for determining the maximum number of iterations
//'for each random start.
//'@param rel_convergence \code{Double} for determining the convergence criterion. The
//'algorithm stops if the relative change is smaller than this criterion.
//' @param con_step_size \code{Double} for specifying the size for increasing or
//' decreasing the probabilities during the condition stage of estimation.
//' This value should not be less than 1e-3.
//' @param con_random_starts \code{Integer} for the number of random starts
//' within the condition stage.
//' @param con_max_iterations \code{Integer} for the maximum number of iterations
//' during the condition stage.
//' @param con_rel_convergence \code{Double} for determining the convergence
//' criterion during condition stage. The algorithm stops if the relative change
//' is smaller than this criterion.
//' @param fast \code{Bool} If \code{TRUE} a fast estimation is applied during the
//' condition stage. This option ignores all parameters beginning with "con_".
//' If \code{FALSE} the estimation described in Berding and
//' Pargmann (2022) is used. Default is \code{TRUE}.
//'@param trace \code{TRUE} for printing progress information on the console.
//'\code{FALSE} if this information should not be printed.
//'@param con_trace \code{TRUE} for printing progress information on the console
//'during estimations in the condition stage. \code{FALSE} if this information
//'should not be printed.
//'@return Function returns a \code{list} with the estimated parameter sets for
//'every random start. Every parameter set contains the following components:
//'\item{log_likelihood}{Log likelihood of the estimated solution.}
//'\item{aem}{Estimated Assignment Error Matrix (aem). The rows represent the
//'true categories while the columns stand for the assigned categories. The cells
//'describe the probability that a coding unit of category i is assigned to
//'category j.}
//'\item{categorial_sizes}{\code{Vector} of estimated sizes for each
//'category.}
//'\item{convergence}{If the algorithm converged within the iteration limit
//'\code{TRUE}. \code{FALSE} in every other case.}
//'\item{iteration}{Number of iterations when the algorithm was terminated.}
//'@export
//'@importFrom Rcpp sourceCpp
//'@useDynLib iotarelr
//'@references  Berding, Florian, and Pargmann, Julia (2022).Iota Reliability Concept
//'of the Second Generation.Measures for Content Analysis Done by
//'Humans or Artificial Intelligences. Berlin: Logos.
//'https://doi.org/10.30819/5581
// [[Rcpp::export]]
Rcpp::List EM_algo_c (Rcpp::CharacterMatrix obs_pattern_shape,
                      Rcpp::NumericVector obs_pattern_frq,
                      Rcpp::NumericMatrix obs_internal_count,
                      Rcpp::CharacterVector categorical_levels,
                      int random_starts,
                      int max_iterations,
                      double rel_convergence,
                      double con_step_size,
                      int con_random_starts,
                      int con_max_iterations,
                      double con_rel_convergence,
                      bool fast,
                      bool trace,
                      bool con_trace)
{
  Rcpp::List Estimates_collection;
  Rcpp::LogicalMatrix cons_matrix;

  double rel_change=0.0;
  double pattern_p=0.0;
  double tmp_p=0.0;
  double tmp=0.0;

  int n_categories=categorical_levels.length();
  int n_pattern=obs_pattern_shape.nrow();
  int n_rater=obs_pattern_shape.ncol();
  int N=Rcpp::sum(obs_pattern_frq);

  Rcpp::NumericMatrix aem_cons(n_categories,n_categories);
  Rcpp::NumericVector tmp_row_aem(n_categories);
  Rcpp::NumericVector tmp_row_aem_1(n_categories);
  Rcpp::NumericMatrix old_aem(n_categories,n_categories);
  Rcpp::NumericVector old_cat_sizes(n_categories);


  int start=0;
  int i=0;
  int j=0;
  int iter=0;
  int p=0;
  int c=0;
  int r=0;
  int a=0;

  double new_ll=0.0;
  double old_ll=0.0;

  int index_c=0;
  int index_pr=0;
  String string_pr;

  Rcpp::NumericMatrix aem(n_categories,n_categories);
  Rcpp::NumericVector categorial_sizes(n_categories);

  Rcpp::NumericVector uncondioned_pattern_p(n_pattern);
  Rcpp::NumericMatrix condioned_pattern_p(n_pattern,n_categories);
  Rcpp::NumericMatrix expected_frequencies(n_pattern,n_categories);

  bool convergence;

  for(start=1;start<=random_starts;start++){
    aem=get_random_start_values_p(n_categories);
    categorial_sizes=get_random_start_values_class_sizes(n_categories);



    colnames(aem)=categorical_levels;
    rownames(aem)=categorical_levels;

    colnames(aem_cons)=categorical_levels;
    rownames(aem_cons)=categorical_levels;

    categorial_sizes.names()=categorical_levels;

    new_ll=fct_log_likelihood_c(categorial_sizes,
                                aem,
                                obs_pattern_shape,
                                obs_pattern_frq,
                                categorical_levels);
    rel_change=99.99;

    if(trace==true){
      Rcout << "Initial Log-Likelihood: " << new_ll << "\n";
    }


    iter=1;
    while(iter<=max_iterations && rel_change>rel_convergence){
      old_aem=clone(aem);
      old_cat_sizes=clone(categorial_sizes);

      for(p=0;p<n_pattern;p++){
        pattern_p=0.0;
        for(c=0;c<n_categories;c++){
          index_c=c;
          tmp_p=1.0;
          for(r=0;r<n_rater;r++){
            string_pr=obs_pattern_shape(p,r);
            index_pr=categorial_sizes.findName(string_pr);
            tmp_p=tmp_p*aem(index_c,index_pr);
          }
          tmp_p=tmp_p*categorial_sizes(c);

          condioned_pattern_p(p,index_c)=tmp_p;

          pattern_p=pattern_p+tmp_p;
        }
        uncondioned_pattern_p(p)=pattern_p;
      }


      for(p=0;p<n_pattern;p++){
        for(c=0;c<n_categories;c++){
          expected_frequencies(p,c)=obs_pattern_frq(p)*
            condioned_pattern_p(p,c)/uncondioned_pattern_p(p);
        }
      }


      for(c=0;c<n_categories;c++){
        categorial_sizes(c)=Rcpp::sum(expected_frequencies(_,c));
      }
      categorial_sizes=categorial_sizes/N;


        for(c=0; c<n_categories;c++){
          for(a=0;a<n_categories;a++){
            tmp=0.0;
            for(p=0; p<n_pattern;p++){
              // Test
              tmp=tmp+expected_frequencies(p,c)*obs_internal_count(p,a);
            }

            aem(c,a)=tmp/(N*categorial_sizes(c));
          }
        }

        //Condition Stage
        if(check_conformity_c(aem)!=0){
          //Transforming aem in order to fit weak superiority
          for(i=0;i<n_categories;i++){
            tmp_row_aem_1=aem(i,_);
            tmp_row_aem=est_con_multinominal_c(tmp_row_aem_1,
                                               i+1,
                                               max_iterations,
                                               con_step_size,
                                               con_rel_convergence,
                                               con_random_starts,
                                               fast,
                                               con_trace);
            for(j=0;j<n_categories;j++){
              aem_cons(i,j)=tmp_row_aem(j);
            }

          }
          aem=aem_cons;
          //Transforming categorical sizes
          for(p=0;p<n_pattern;p++){
            pattern_p=0.0;
            for(c=0;c<n_categories;c++){
              index_c=c;
              tmp_p=1.0;
              for(r=0;r<n_rater;r++){
                string_pr=obs_pattern_shape(p,r);
                index_pr=categorial_sizes.findName(string_pr);
                tmp_p=tmp_p*aem(index_c,index_pr);
              }
              tmp_p=tmp_p*categorial_sizes(c);

              condioned_pattern_p(p,index_c)=tmp_p;

              pattern_p=pattern_p+tmp_p;
            }
            uncondioned_pattern_p(p)=pattern_p;
          }


          for(p=0;p<n_pattern;p++){
            for(c=0;c<n_categories;c++){
              expected_frequencies(p,c)=obs_pattern_frq(p)*
                condioned_pattern_p(p,c)/uncondioned_pattern_p(p);
            }
          }


          for(c=0;c<n_categories;c++){
            categorial_sizes(c)=Rcpp::sum(expected_frequencies(_,c));
          }
          categorial_sizes=categorial_sizes/N;
        }

        old_ll=new_ll;

        new_ll=fct_log_likelihood_c(categorial_sizes,
                                      aem,
                                      obs_pattern_shape,
                                      obs_pattern_frq,
                                      categorical_levels
                                      );

          rel_change=(old_ll-new_ll)/old_ll;
          if(rel_change<0.0){
            aem=old_aem;
            categorial_sizes=old_cat_sizes;
            new_ll=fct_log_likelihood_c(categorial_sizes,
                                        aem,
                                        obs_pattern_shape,
                                        obs_pattern_frq,
                                        categorical_levels
                                        );
            rel_change=(old_ll-new_ll)/old_ll;
          }

          if(trace==true){
            Rcout << "Start "<< start <<" Iteration " << iter <<" Log_Likelihood "
                  << new_ll << " relative change "<< rel_change << "\n";
          }

          iter=iter+1;
    }

    if(rel_change<=rel_convergence && iter<=max_iterations){
      convergence=true;
    } else {
      convergence=false;
    }


    Estimates_collection.push_back(Rcpp::List::create(Named("log_likelihood")=new_ll,
                                        Named("aem")=aem,
                                        Named("categorial_sizes")=categorial_sizes,
                                        Named("convergence")=convergence,
                                        Named("iteration")=iter-1)
                                     );

  }
  return Estimates_collection;
}


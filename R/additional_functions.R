
#'Check new rater
#'
#'Function for estimating the reliability of codings for a new rater based on
#'Iota 2
#'
#'@param true_values \code{Vector} containing the true categories of the coding
#'units. Vector must have the same length as \code{assigned_values}.
#'@param assigned_values \code{Vector} containing the assigned
#'categories of the coding units. Missing values are currently not supported and
#'have to be omitted from the vector. Vector must have the same length as
#'\code{true_values}.
#' @param con_step_size \code{Double} for specifying the size for increasing or
#' decreasing the probabilities during the conditioning stage of estimation.
#' This value should not be less than 1e-3.
#' @param con_random_starts \code{Integer} for the number of random starts
#' within the condition stage.
#' @param con_max_iterations \code{Integer} for the maximum number of iterations
#' during the conditioning stage.
#' @param con_rel_convergence \code{Double} for determining the convergence
#' criterion during the conditioning stage. The algorithm stops if the relative change
#' is smaller than this criterion.
#'@param con_trace \code{TRUE} for printing progress information on the console
#'during estimations in the conditioning stage. \code{FALSE} if you do not want to have
#'this information printed.
#' @param fast \code{Bool} If \code{TRUE} a fast estimation is applied during the
#' condition stage. This option ignores all parameters beginning with "con_".
#' If \code{FALSE} the estimation described in Berding and
#' Pargmann (2022) is used. Default is \code{TRUE}.
#' @param free_aem \code{Bool} If \code{TRUE} the Assignment Error Matrix is
#' estimated in a way ensuring conformity with the assumption of weak superiority.
#' if \code{FALSE} the Assignment Error Matrix is freely estimated. \code{TRUE}
#' is default.
#' @return Returns a \code{list} with the following three components:
#' The first component \code{estimates_categorical_level} comprises all
#' elements that describe the ratings on a categorical level. The elements are
#' sub-divided into raw estimates and chance-corrected estimates.
#'
#' \code{raw_estimates}
#' \describe{
#' \item{\code{alpha_reliability:}}{A vector containing the Alpha
#' Reliabilities for each category. These values represent probabilities.}
#' \item{\code{beta_reliability:}}{A vector containing the Beta Reliabilities for each
#' category. These values represent probabilities.}
#' \item{\code{assignment_error_matrix:}}{An Assignment Error Matrix containing the conditional
#' probabilities for assigning a unit of category i to categories 1 to n.}
#' \item{\code{iota:}}{A vector containing the Iota values for each category.}
#' }
#' \code{elements_chance_corrected}
#' \describe{
#' \item{\code{alpha_reliability:}}{A vector containing the chance-corrected Alpha Reliabilities for each category.}
#' \item{\code{beta_reliability:}}{A vector containing the chance-corrected Beta Reliabilities for each category.}
#' }
#'
#' The second component \code{estimates_scale_level} contains elements to
#' describe the quality of the ratings on a scale level. It contains the
#' following elements:
#' \describe{
#' \item{\code{iota_index:}}{The Iota Index representing the reliability on a scale level.}
#' \item{\code{iota_index_d4:}}{The Static Iota Index, which is a transformation of the original Iota Index,
#' in order to consider the uncertainty of estimation.}
#' \item{\code{iota_index_dyn2:}}{The Dynamic Iota Index, which is a transformation of the original Iota Index,
#' in order to consider the uncertainty of estimation.}
#' }
#' The third component \code{information} contains important information
#' regarding the parameter estimation. It comprises the following elements:
#'\describe{
#' \item{\code{log_likelihood:}}{Log-likelihood of the best solution.}
#' \item{\code{convergence:}}{If estimation converged 0, otherwise 1.}
#' \item{\code{est_true_cat_sizes:}}{Estimated categorical sizes. This is the estimated amount of the categories.}
#' \item{\code{conformity:}}{\code{0} if the solution is in line with assumptions of weak superiority.
#'  A number greater 0 indicates the number of violations of the assumption
#'  of weak superiority.}
#'  \item{\code{random_starts:}}{Numer of random starts for the EM algorithm.}
#' \item{\code{boundaries:}}{\code{False} if the best solution does not contain boundary values.
#' \code{True} if the best solution does contain boundary values}
#' \item{\code{p_boundaries:}}{Percentage of solutions with boundary values during estimation.}
#' \item{\code{call:}}{Name of the function that created the object.}
#' \item{\code{n_rater:}}{Number of raters.}
#' \item{\code{n_cunits:}}{Number of coding units.}
#' }
#'@note The returned object contains further slots since the returned object is
#'of class \code{iotarelr_iota2}. These slots are empty because they are not part of the
#'estimation within this function.
#'@note Please do not use the measures on the scale level if the Assignment Error Matrix
#'was freely estimated since this kind of matrix is not conceptualized for comparing
#'the coding process with random guessing.
#'@references  Florian Berding and Julia Pargmann (2022). Iota Reliability Concept
#'of the Second Generation. Measures for Content Analysis Done by
#'Humans or Artificial Intelligences. Berlin:Logos.
#'https://doi.org/10.30819/5581
#'@export
check_new_rater<-function(true_values,
                          assigned_values,
                          con_step_size=1e-4,
                          con_random_starts=5,
                          con_max_iterations=5000,
                          con_rel_convergence=1e-12,
                          con_trace=FALSE,
                          fast=TRUE,
                          free_aem=FALSE){

  #Checking format of true values and assigned values
  if((is.data.frame(true_values)==TRUE |
     is.factor(true_values)==TRUE |
     is.vector(true_values)==TRUE)==FALSE){
    stop("true_values must be a vector, factor or data.frame")
  }
  if((is.data.frame(assigned_values)==TRUE |
      is.factor(assigned_values)==TRUE |
      is.vector(assigned_values)==TRUE)==FALSE){
    stop("assigned_values must be a vector, factor or data.frame")
  }

  #Standardizing input data of the true values
  true_values<-unname(true_values)
  if(is.data.frame(true_values)==TRUE){
    true_values<-as.matrix(true_values)
  }
  true_values<-as.character(true_values)

  #Standardizing input data of the assigned values
  if(is.data.frame(assigned_values)==TRUE){
    assigned_values<-as.matrix(assigned_values)
  }
  assigned_values<-as.character(assigned_values)
  assigned_values<-unname(assigned_values)

  #Gathering information on the categorical levels
  categorical_levels<-names(table(true_values))
  categorical_levels<-sort(categorical_levels,decreasing = FALSE)

  categorical_levels_assigned_data<-names(table(assigned_values))
  categorical_levels_assigned_data<-sort(categorical_levels_assigned_data,decreasing = FALSE)

  #Checking if the categorical levels are compatible
  if(sum(categorical_levels_assigned_data%in%categorical_levels)!=length(categorical_levels_assigned_data)){
    stop("The categorical levels of the assigned values exceed the range of
         possible categorical levels of the true values. Please check your
         assigned data for levels that do not occur in the true data.")
  }

  #Start of the calculations
  obs<-cbind(true_values,assigned_values)
  colnames(obs)<-c("true_values","assigned_values")
  obs<-as.data.frame(obs)

  n_categories=length(categorical_levels)
  freq_matrix<-matrix(data=NA,
                      nrow = n_categories,
                      ncol = n_categories)

  colnames(freq_matrix)<-categorical_levels
  rownames(freq_matrix)<-categorical_levels
  for(i in categorical_levels){
    for(j in categorical_levels){
      tmp_subset<-subset(obs,obs$true==i)
      freq_matrix[i,j]<-sum(tmp_subset$assigned_values==j)
    }
  }

  categorical_sizes=rowSums(freq_matrix)/sum(freq_matrix)
  aem<-NULL
  if(free_aem==FALSE){
    for(i in 1:n_categories){
      tmp<-est_con_multinominal_c(observations=freq_matrix[i,],
                             anchor = i,
                             max_iter = con_max_iterations,
                             step_size = con_step_size,
                             cr_rel_change = con_rel_convergence,
                             n_random_starts = con_random_starts,
                             fast=fast,
                             trace = con_trace)
      aem<-rbind(aem,tmp)
    }
  } else {
      aem<-freq_matrix/rowSums(freq_matrix)
  }
  colnames(aem)<-categorical_levels
  rownames(aem)<-categorical_levels

  measures<-get_iota2_measures(aem=aem,
                               categorical_sizes = categorical_sizes,
                               categorical_levels= categorical_levels)

  Esimtates_Information<-NULL
  Esimtates_Information["log_likelihood"]<-list(NA)
  Esimtates_Information["iteration"]<-list(NA)
  Esimtates_Information["convergence"]<-list(NA)
  Esimtates_Information["est_true_cat_sizes"]<-list(categorical_sizes)
  Esimtates_Information["conformity"]<-list(check_conformity_c(aem=aem))
  Esimtates_Information["boundaries"]<-list(NA)
  Esimtates_Information["p_boundaries"]<-list(NA)

  Esimtates_Information["n_rater"]<-list(1)
  Esimtates_Information["n_cunits"]<-list(length(true_values))
  Esimtates_Information["call"]<-list("check_new_rater")

  Esimtates_Information["random_starts"]<-list(NA)
  Esimtates_Information["estimates_list"]<-list(NA)

  results<-NULL
  results["categorical_level"]<-list(measures$categorical_level)
  results["scale_level"]<-list(measures$scale_level)
  results["information"]<-list(Esimtates_Information)

  class(results)<-"iotarelr_iota2"

  return(results)
}



#'Check for Different Guidance Functioning (DGF)
#'
#'Function for checking if the coding scheme is the same for different sub-groups.
#'
#' @param data Data for which the elements should be estimated. Data must be
#' an object of type \code{data.frame} or \code{matrix} with cases in the rows and
#' raters in the columns. Please note that no additional variables are allowed
#' in this object.
#' @param splitcr \code{Vector} containing the assignments of coding units to
#' groups. The vector must have the same length as the number of rows of object
#' \code{data}.
#' @param random_starts An integer for the number of random starts for the
#' EM algorithm.
#' @param max_iterations An integer for the maximum number of iterations within
#' the EM algorithm.
#' @param cr_rel_change Positive numeric value for defining the convergence of the
#' EM algorithm.
#' @param con_step_size \code{Double} for specifying the size for increasing or
#' decreasing the probabilities during the conditioning stage of estimation.
#' This value should not be less than 1e-3.
#' @param con_random_starts \code{Integer} for the number of random starts
#' within the condition stage.
#' @param con_max_iterations \code{Integer} for the maximum number of iterations
#' during the condition stage.
#' @param con_rel_convergence \code{Double} for determining the convergence
#' criterion during condition stage. The algorithm stops if the relative change
#' is smaller than this criterion.
#'@param trace \code{TRUE} for printing progress information on the console.
#'\code{FALSE} if this information is not to be printed.
#'@param con_trace \code{TRUE} for printing progress information on the console
#'during estimations in the condition stage. \code{FALSE} if this information
#'is not to be printed.
#' @param fast \code{Bool} If \code{TRUE} a fast estimation is applied during the
#' condition stage. This option ignores all parameters beginning with "con_".
#' If \code{FALSE} the estimation described in Berding and
#' Pargmann (2022) is used. Default is \code{TRUE}.
#'@param b_min Value ranging between 0 and 1 determining the minimal size of
#'the categories for checking if boundary values occurred. The algorithm tries
#'to select solutions that are not considered to be boundary values.
#'@return Returns an object of class \code{iotarelr_iota2_dif}. For each group,
#'the results of the estimation are saved separately. The structure within each
#'group is similar to the results from \code{compute_iota2()}. Please check
#'that documentation.
#'@references  Florian Berding and Julia Pargmann (2022).Iota Reliability Concept
#'of the Second Generation. Measures for Content Analysis Done by
#'Humans or Artificial Intelligences. Berlin:Logos.
#'https://doi.org/10.30819/5581
#'@export

check_dgf<-function(data,
                    splitcr,
                    random_starts = 300,
                    max_iterations = 5000,
                    cr_rel_change = 1e-12,
                    con_step_size = 1e-4,
                    con_random_starts = 10,
                    con_max_iterations = 5000,
                    con_rel_convergence = 1e-12,
                    b_min = 0.01,
                    trace = FALSE,
                    con_trace = FALSE,
                    fast=TRUE){

  tmp<-table(splitcr)
  n_group=length(tmp)
  group_names<-names(tmp)

  splitcr<-factor(x=lapply(splitcr,FUN=as.character),levels = group_names)

  sub_group_data<-NULL
  results<-NULL
  for(i in 1:n_group){
    sub_group_data<-subset(data,splitcr==group_names[i])
    if (trace==TRUE){
      print(paste(date(),"Estimating Parameters for subgroup",group_names[i]))
    }
    results[paste0("group_",group_names[i])]<-list(
      compute_iota2(data=sub_group_data,
                    random_starts = random_starts,
                    max_iterations = max_iterations,
                    cr_rel_change = cr_rel_change,
                    con_step_size = con_step_size,
                    con_rel_convergence = con_rel_convergence,
                    con_max_iterations = con_max_iterations,
                    con_random_starts = con_random_starts,
                    b_min = b_min,
                    trace = trace,
                    con_trace = con_trace,
                    fast=fast))
      }
  class(results)<-"iotarelr_iota2_dgf"
  return(results)
}

#'Estimate Expected Categories
#'
#'Function for estimating the expected category of coding units.
#'
#' @param data \code{Matrix} which contains the codings for every coding unit. The
#' coding units must be in the rows and the raters must be in the columns. At
#' least two raters are necessary.
#' @param aem Assignment Error Matrix based on the second generation of the Iota Concept (Iota2).
#' @return Returns a \code{matrix} with the original data, the conditioned
#' probability of each true category, and the expected category for every coding unit.
#'@references  Florian Berding and Julia Pargmann (2022).Iota Reliability Concept
#'of the Second Generation. Measures for Content Analysis Done by
#'Humans or Artificial Intelligences. Berlin:Logos.
#'https://doi.org/10.30819/5581
#' @export

est_expected_categories<-function(data,
                                  aem){
  if(ncol(data)<2){
    stop("Data table must have at least two columns.")
  }

  prob_matrix<-matrix(data=NA,
                      nrow=nrow(data),
                      ncol=ncol(aem))
  colnames(prob_matrix)<-colnames(aem)

  exp_categories<-vector(length = nrow(data))

  for(i in 1:nrow(data)){
    for(j in 1:nrow(aem)){
      tmp_p<-1
      for(r in 1:ncol(data)){
        assigned_category<-data[i,r]
        index_category<-match(x=assigned_category,
                              table=colnames(aem))
        tmp_p=tmp_p*aem[j,index_category]
      }
      prob_matrix[i,j]<-tmp_p
    }
    prob_matrix[i,]<-prob_matrix[i,]/sum(prob_matrix[i,])
    index_max<-match(x=max(prob_matrix[i,]),
                     table=prob_matrix[i,])
    exp_categories[i]=colnames(aem)[index_max]
  }
  results<-cbind(data,prob_matrix,exp_categories)
  colnames(results)<-c(
    colnames(data),
    paste0("prob_",colnames(prob_matrix)),
    "expected_category")
  return(results)
}

#'Get Summary
#'
#'Function for creating a short summary of the estimated Iota components.
#'
#' @param object An object of class \code{iotarelr_iota2} created by
#' \code{\link{compute_iota2}}, \code{\link{check_new_rater}}, or
#' \code{\link{check_dgf}}.
#' @return Prints central statistics of the estimated model.
#' @importFrom methods is
#' @export
get_summary<-function(object){
  if(methods::is(object,"iotarelr_iota2")==FALSE){
    stop("Class of object is not supported by this function.")
  }

  call=object$information$call

  n_starts<-object$information$random_starts
  n_categories<-length(object$information$est_true_cat_sizes)
  n_cunits<-object$information$n_cunits
  category_levels<-names(object$information$est_true_cat_sizes)

  log_likelihood<-object$information$log_likelihood
  convergence<-object$information$convergence
  n_rater<-object$information$n_rater

  if(call!="check_new_rater"){
    est_log_likelihood<-vector(length = n_starts)
    for(i in 1:n_starts){
      est_log_likelihood[i]<-object$information$estimates_list[[i]]$log_likelihood
    }
    replications<-sum(round(est_log_likelihood,digits = 4)==round(log_likelihood,digits = 4))
    if(replications>1){
      string_rep="The best log-likelihood has been replicated."
    } else {
      string_rep="The best log-likelihood has not been replicated. Increase the
      number of random stars and/or inspect the Assignment Error Matrices and
      categorical sizes."
    }
  } else {
    string_rep=""
  }

  dimensions<-c("Alpha",
                "Beta",
                "Iota",
                "Iota Error Type I",
                "Iota Error Type II")
  summary_table<-NULL
  summary_table<-rbind(summary_table,object$categorical_level$raw_estimates$alpha_reliability)
  summary_table<-rbind(summary_table,object$categorical_level$raw_estimates$beta_reliability)
  summary_table<-rbind(summary_table,object$categorical_level$raw_estimates$iota)
  summary_table<-rbind(summary_table,object$categorical_level$raw_estimates$iota_error_1)
  summary_table<-rbind(summary_table,object$categorical_level$raw_estimates$iota_error_2)
  summary_table<-round(summary_table,digits=4)
  summary_table<-cbind(dimensions,summary_table)
  summary_table<-as.data.frame(summary_table)

  iota_index<-round(object$scale_level$iota_index,digits = 3)
  iota_index_d4<-round(object$scale_level$iota_index_d4,digits = 3)
  iota_index_dyn2<-round(object$scale_level$iota_index_dyn2, digits = 3)

  cat(sep="\n",
      "Summary\n",
      paste("Call:",call),
      "",
      paste("Number of Raters:",n_rater),
      paste("Number of Categories:",n_categories),
      paste("Categories:",paste(category_levels,collapse = ",")),
      paste("Number of Coding Units:",n_cunits),
      "",
      paste("Random Start:",n_starts),
      paste("Log-Likelihood:",log_likelihood),
      string_rep,
      "",
      "Primary Parameters",
      "Assignment Error Matrix")
  print(format(as.data.frame(round(object$categorical_level$raw_estimates$assignment_error_matrix,
                             digits=3)),
               scientific=FALSE))
  cat(sep="\n",
      "",
      "Categorical Sizes")
  print(round(object$information$est_true_cat_sizes,digits=3))
  cat(sep="\n",
      "",
      "Categorical Level")
  print(format(summary_table,digits=3))
  cat(sep="\n",
      "",
      "Scale Level",
      paste("Iota Index:",iota_index),
      paste("Static Iota Index:",iota_index_d4),
      paste("Dynamic Iota Index:",iota_index_dyn2)
      )
}
summary.iotarelr_iota2<-get_summary

#'Get Consequences
#'
#'Function estimating the consequences of reliability for subsequent analysis.
#'
#' @param measure_typ Type of measure used for estimation. Set "iota_index" for
#' the original Iota Index, "static_iota_index" for the static transformation
#' of the Iota Index with d=4 or "dynamic_iota_index" for the dynamic transformation
#' of the Iota Index with d=2.
#' @param measure_1_val Reliability value for the independent variable.
#' @param measure_2_val Reliability value for the dependent variable. If not
#' set, the function uses the same value as for the independent variable.
#' @param level Level of certainty for calculating the prediction intervals.
#' @param strength True strength of the relationship between the independent and
#' dependent variable. Possible values are "no", "weak", "medium" and "strong". If
#' no value is supplied, a strong relationship is assumed for deviation and a weak
#' relationship for all others. They represent the most demanding situations for the
#' reliability.
#' @param data_type Type of data. Possible values are "nominal" or "ordinal".
#' @param sample_size Size of the sample in the study.
#' @return Returns a \code{data.frame} which contains the prediction intervals
#' for the deviation between  true and estimated sample association/correlation,
#' risk of Type I errors and chance to correctly classify the effect size.
#' Additionally, the probability is estimated so that the statistics of the sample
#' deviate from an error free sample with no or only a weak effect .
#' @note The classification of effect sizes uses the work of Cohen (1988),
#' who differentiates effect sizes by their relevance for practice.
#'
#' For nominal data, all statistics refer to Cramer's V. For ordinal data, all
#' statistics refer to Kendall's Tau.
#'
#' The models for calculating the consequences are taken from Berding and
#' Pargmann (2022).
#' @references Cohen, J. (1988). Statistical Power Analysis for the Behavioral
#' Sciences (2nd Ed.). Taylor & Francis.
#' @references  Berding, Florian, and Pargmann, Julia (2022).Iota Reliability Concept
#'of the Second Generation.Measures for Content Analysis Done by
#'Humans or Artificial Intelligences. Berlin:Logos.
#'https://doi.org/10.30819/5581
#' @export
get_consequences<-function(measure_typ="dynamic_iota_index",
                           measure_1_val,
                           measure_2_val=NULL,
                           level=0.95,
                           strength=NULL,
                           data_type,
                           sample_size){

  if(measure_typ=="iota_index"){
    measure_index="Index"
  } else if (measure_typ=="static_iota_index"){
    measure_index="Iota_Index_400"
  } else if (measure_typ=="dynamic_iota_index"){
    measure_index="Index_dyn_200"
  } else {
    stop("Measure Type not supported.")
  }

  if(is.null(strength)==TRUE){
    strength_deviation="strong"
    model_deviation=pred_models[["deviation"]][[data_type]][[measure_index]][[strength_deviation]]
    strength_type_i_err="weak"
    model_type_i=pred_models[["type_i_errors"]][[data_type]][[measure_index]][[strength_type_i_err]]
    strength_effect_size="weak"
    model_effect_size=pred_models[["effect_sizes"]][[data_type]][[measure_index]][[strength_effect_size]]
  } else {
    strength_deviation=strength
    model_deviation=pred_models[["deviation"]][[data_type]][[measure_index]][[strength_deviation]]
    strength_type_i_err=strength
    model_type_i=pred_models[["type_i_errors"]][[data_type]][[measure_index]][[strength_type_i_err]]
    strength_effect_size=strength
    model_effect_size=pred_models[["effect_sizes"]][[data_type]][[measure_index]][[strength_effect_size]]
  }

  if(is.null(measure_2_val)==TRUE){
    measure_2_val=measure_1_val
  }
  values<-c(1,measure_1_val,measure_2_val,sample_size)
  values_probit<-c(-1,measure_1_val,measure_2_val,sample_size)

  expected_deviation=values%*%model_deviation$parameters
  dev_n=model_deviation$n
  dev_p=model_deviation$p
  dev_se=model_deviation$standard_error
  dev_cov=model_deviation$covmatrix
  dev_inner_sqrt=1+(values%*%dev_cov)%*%((values))
  dev_lwr<-expected_deviation-abs(stats::qt(p=(1-level)/2,df=(dev_n-dev_p)))*dev_se*sqrt(dev_inner_sqrt)
  dev_upr<-expected_deviation+abs(stats::qt(p=(1-level)/2,df=(dev_n-dev_p)))*dev_se*sqrt(dev_inner_sqrt)

  expected_error_rate_raw=values_probit%*%model_type_i$parameters
  i_rate_n=model_type_i$n
  i_rate_p=model_type_i$p
  i_rate_se=model_type_i$standard_error
  i_rate_cov=model_type_i$covmatrix
  i_rate_inner_sqrt=1+(values_probit%*%i_rate_cov)%*%((values_probit))
  i_rate_lwr_raw<-expected_error_rate_raw-abs(stats::qt(p=(1-level)/2,df=(i_rate_n-i_rate_p)))*i_rate_se*sqrt(i_rate_inner_sqrt)
  i_rate_upr_raw<-expected_error_rate_raw+abs(stats::qt(p=(1-level)/2,df=(i_rate_n-i_rate_p)))*i_rate_se*sqrt(i_rate_inner_sqrt)

  expected_error_rate=stats::pnorm(q=expected_error_rate_raw)
  i_rate_lwr=stats::pnorm(q=i_rate_lwr_raw)
  i_rate_upr=stats::pnorm(q=i_rate_upr_raw)

  expected_cassification_rate_raw=values_probit%*%model_effect_size$parameters
  c_rate_n=model_effect_size$n
  c_rate_p=model_effect_size$p
  c_rate_se=model_effect_size$standard_error
  c_rate_cov=model_effect_size$covmatrix
  c_rate_inner_sqrt=1+(values_probit%*%c_rate_cov)%*%((values_probit))
  c_rate_lwr_raw<-expected_cassification_rate_raw-abs(stats::qt(p=(1-level)/2,df=(c_rate_n-c_rate_p)))*c_rate_se*sqrt(c_rate_inner_sqrt)
  c_rate_upr_raw<-expected_cassification_rate_raw+abs(stats::qt(p=(1-level)/2,df=(c_rate_n-c_rate_p)))*c_rate_se*sqrt(c_rate_inner_sqrt)

  expected_cassification_rate=stats::pnorm(q=expected_cassification_rate_raw)
  c_rate_lwr=stats::pnorm(q=c_rate_lwr_raw)
  c_rate_upr=stats::pnorm(q=c_rate_upr_raw)

  #Estimating chances for specific values
  #Deviation
  steps=seq(from=0.5, to=1, by=0.001)
  dev_tmp_results<-matrix(data=NA,
                          nrow=length(steps),
                          ncol = 4)
  for(i in 1:length(steps)){
    tmp_dev_lwr<-expected_deviation-abs(stats::qt(p=(1-steps[i]),df=(dev_n-dev_p)))*dev_se*sqrt(dev_inner_sqrt)
    tmp_dev_upr<-expected_deviation+abs(stats::qt(p=(1-steps[i]),df=(dev_n-dev_p)))*dev_se*sqrt(dev_inner_sqrt)

    target_val_no=0.10
    target_val_weak=0.30

    if(target_val_no>=expected_deviation){
      tmp_val<-tmp_dev_upr
      dev_tmp_results[i,1]<-steps[i]
      dev_tmp_results[i,2]<-abs(target_val_no-tmp_val)
    } else {
      tmp_val<-tmp_dev_lwr
      dev_tmp_results[i,1]<-1-(steps[i])
      dev_tmp_results[i,2]<-abs(target_val_no-tmp_val)
    }

    if(target_val_weak>=expected_deviation){
      tmp_val<-tmp_dev_upr
      dev_tmp_results[i,3]<-steps[i]
      dev_tmp_results[i,4]<-abs(target_val_weak-tmp_val)
    } else {
      tmp_val<-tmp_dev_lwr
      dev_tmp_results[i,3]<-1-(steps[i])
      dev_tmp_results[i,4]<-abs(target_val_weak-tmp_val)
    }
  }
  dev_p_01<-dev_tmp_results[match(x=min(dev_tmp_results[,2]),
                                  table=dev_tmp_results[,2])
                            ,1]
  dev_p_03<-dev_tmp_results[match(x=min(dev_tmp_results[,4]),
                                  table=dev_tmp_results[,4])
                            ,3]

  #Type I error rates
  i_rate_tmp_results<-matrix(data=NA,
                          nrow=length(steps),
                          ncol = 4)
  for(i in 1:length(steps)){
    tmp_i_rate_lwr<-stats::pnorm(q=expected_error_rate_raw-abs(stats::qt(p=(1-steps[i]),df=(i_rate_n-i_rate_p)))*i_rate_se*sqrt(i_rate_inner_sqrt))
    tmp_i_rate_upr<-stats::pnorm(q=expected_error_rate_raw+abs(stats::qt(p=(1-steps[i]),df=(i_rate_n-i_rate_p)))*i_rate_se*sqrt(i_rate_inner_sqrt))

    target_val_no=0.05
    target_val_weak=0.10

    if(target_val_no>=expected_error_rate){
      tmp_val<-tmp_i_rate_upr
      i_rate_tmp_results[i,1]<-steps[i]
      i_rate_tmp_results[i,2]<-abs(target_val_no-tmp_val)
    } else {
      tmp_val<-tmp_i_rate_lwr
      i_rate_tmp_results[i,1]<-1-(steps[i])
      i_rate_tmp_results[i,2]<-abs(target_val_no-tmp_val)
    }
    if(target_val_weak>=expected_error_rate){
      tmp_val<-tmp_i_rate_upr
      i_rate_tmp_results[i,3]<-steps[i]
      i_rate_tmp_results[i,4]<-abs(target_val_weak-tmp_val)
    } else {
      tmp_val<-tmp_i_rate_lwr
      i_rate_tmp_results[i,3]<-1-(steps[i])
      i_rate_tmp_results[i,4]<-abs(target_val_weak-tmp_val)
    }
  }
  i_rate_p_05<-i_rate_tmp_results[match(x=min(i_rate_tmp_results[,2]),
                                  table=i_rate_tmp_results[,2])
                            ,1]
  i_rate_p_10<-i_rate_tmp_results[match(x=min(i_rate_tmp_results[,4]),
                                  table=i_rate_tmp_results[,4])
                            ,3]

  c_rate_tmp_results<-matrix(data=NA,
                             nrow=length(steps),
                             ncol = 4)
  for(i in 1:length(steps)){
    tmp_c_rate_lwr<-stats::pnorm(q=expected_cassification_rate_raw-abs(stats::qt(p=(1-steps[i]),df=(c_rate_n-c_rate_p)))*c_rate_se*sqrt(c_rate_inner_sqrt))
    tmp_c_rate_upr<-stats::pnorm(q=expected_cassification_rate_raw+abs(stats::qt(p=(1-steps[i]),df=(c_rate_n-c_rate_p)))*c_rate_se*sqrt(c_rate_inner_sqrt))

    target_val_no=0.95
    target_val_weak=0.90

    if(target_val_no>=expected_cassification_rate){
      tmp_val<-tmp_c_rate_upr
      c_rate_tmp_results[i,1]<-steps[i]
      c_rate_tmp_results[i,2]<-abs(target_val_no-tmp_val)
    } else {
      tmp_val<-tmp_c_rate_lwr
      c_rate_tmp_results[i,1]<-1-(steps[i])
      c_rate_tmp_results[i,2]<-abs(target_val_no-tmp_val)
    }
    if(target_val_weak>=expected_cassification_rate){
      tmp_val<-tmp_c_rate_upr
      c_rate_tmp_results[i,3]<-steps[i]
      c_rate_tmp_results[i,4]<-abs(target_val_weak-tmp_val)
    } else {
      tmp_val<-tmp_c_rate_lwr
      c_rate_tmp_results[i,3]<-1-(steps[i])
      c_rate_tmp_results[i,4]<-abs(target_val_weak-tmp_val)
    }
  }


    c_rate_p_95<-1-c_rate_tmp_results[match(x=min(c_rate_tmp_results[,2]),
                                          table=c_rate_tmp_results[,2])
                                    ,1]
    c_rate_p_90<-1-c_rate_tmp_results[match(x=min(c_rate_tmp_results[,4]),
                                          table=c_rate_tmp_results[,4])
                                    ,3]

    res_dev<-c(dev_lwr,expected_deviation,dev_upr,dev_p_01,dev_p_03)
    res_i_rate<-c(i_rate_lwr,expected_error_rate,i_rate_upr,i_rate_p_05,i_rate_p_10)
    res_c_rate<-c(c_rate_lwr,expected_cassification_rate,c_rate_upr,c_rate_p_95,c_rate_p_90)
    results<-rbind(res_dev,res_c_rate,res_i_rate)
    rownames(results)<-c("deviation","classification rate","risk of Type I errors")
    colnames(results)<-c(paste("lower",level,"%"),
                         "mean",
                         paste("upper",level,"%"),
                         "practically no effect",
                         "practically weak effect")
    results<-as.data.frame(results)
    results<-round(results,digits=3)
    format(results, scientific =FALSE)
    return(results)
}

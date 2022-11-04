#' Computes Iota and its elements in version 1
#'
#' Computes all elements of the Iota Reliability Concept
#'
#' @param data Data for which the elements should be estimated. Data must be
#' an object of type \code{data.frame} or \code{matrix} with  cases in the rows and
#' raters in the columns.
#' @return A list with the following components
#' \item{alpha}{A vector containing the chance-corrected Alpha Reliabilities for
#' every category.}
#' \item{beta}{A vector containing the chance-corrected Beta Reliabilities for
#' every category.}
#' \item{iota}{A vector containing the Iota values for every category.}
#' \item{assignment_error_matrix}{A matrix with the conditional probabilities
#' for every category. The rows refer to the true categories and the columns
#' refer to the assigned categories. The elements on the diagonal represent the
#' alpha errors of that category. The other elements in each row represent the
#' conditioned probabilities that a coding unit is wrongly assigned to another
#' category.}
#' \item{average_iota}{A numeric value ranging between 0 and 1, representing the
#' Average Iota values on a categorical level. It describes the reliability of
#' the whole scale.}
#' @references -   Berding, Florian, Elisabeth Riebenbauer, Simone Stuetz, Heike
#'Jahncke, Andreas Slopinski, and Karin Rebmann. 2022. Performance and
#'Configuration of Artificial Intelligence in Educational
#'Settings.Introducing a New Reliability Concept Based on Content
#'Analysis. Frontiers in Education.
#'https://doi.org/10.3389/feduc.2022.818365
#' @export

compute_iota1<-function(data){
  data<-as.data.frame(data)
  categorical_levels<-names(table(data[1]))
  for(i in 2:ncol(data)){
    tmp<-names(table(data[i]))
    for(j in tmp)
      if(j %in% categorical_levels==FALSE){
        categorical_levels<-c(categorical_levels,j)
      }
  }

  for (i in 1:ncol(data)){
    data[[i]]<-factor(x=lapply(data[[i]],as.character),levels=categorical_levels)
  }
  categorical_levels<-sort(categorical_levels,decreasing = FALSE)
  n_rater=ncol(data)
  n_categories<-length(categorical_levels)
  N=nrow(data)
  n_rater=ncol(data)
  p_cell<-1/(n_categories^n_rater)

  N_Aggrement_Table<-matrix(nrow=1,ncol=n_categories,data=NA)
  colnames(N_Aggrement_Table)<-categorical_levels
  for (k in categorical_levels){
    aggreement<-data[,1:n_rater]==k
    aggreement<-replace(x=aggreement,aggreement==TRUE,1)
    aggreement<-rowSums(aggreement)
    aggreement<-subset(aggreement,aggreement==n_rater)
    aggreement<-length(aggreement)
    N_Aggrement_Table[1,k]<-aggreement
  }

  P_Condition<-matrix(nrow=1,ncol=n_categories,data=NA)
  colnames(P_Condition)<-categorical_levels

  N_Condition<-matrix(nrow=1,ncol=n_categories,data=0)
  colnames(N_Condition)<-categorical_levels

  P_Marginal<-matrix(nrow=n_categories,ncol = n_rater,data=NA)
  rownames(P_Marginal)<-categorical_levels

  for (k in categorical_levels){
    for (r in 1:n_rater){
      #Count the frequencies of k for every rater and sum
      N_Condition_Category<-nrow(subset(data,data[r]==k))
      N_Condition[1,k]<-N_Condition[1,k]+N_Condition_Category
      P_Marginal[k,r]<-nrow(subset(data[r],data[r]==k))/N
    }
    #Correction for over-counting the diagonal cells
    N_Condition[1,k]<-N_Condition[1,k]-(n_rater-1)*N_Aggrement_Table[1,k]
  }
  P_Condition<-N_Condition/N
  P_Alpha_Reliability<-(N_Aggrement_Table/N)/P_Condition
  P_Alpha_Error<-1-P_Alpha_Reliability

  #Estimation of beta-Error
  P_Condition_Beta<-matrix(nrow=1,ncol=n_categories,data=NA)
  colnames(P_Condition_Beta)<-categorical_levels

  N_Condition_Beta<-matrix(nrow=1,ncol=n_categories,data=NA)
  colnames(N_Condition_Beta)<-categorical_levels

  N_Target_Beta<-matrix(nrow=1,ncol=n_categories,data=NA)
  colnames(N_Target_Beta)<-categorical_levels

  for (k in categorical_levels){
    categories<-categorical_levels
    categories<-subset(categories,categories!=k)
    N_Condition_Beta[1,k]<-N-sum(N_Aggrement_Table[1,])

    N_Combination<-data[,1:n_rater]!=k
    N_Combination<-replace(x=N_Combination,N_Combination==TRUE,1)
    N_Combination<-rowSums(N_Combination)
    N_Combination<-subset(N_Combination,N_Combination==n_rater)
    N_Combination<-length(N_Combination)

    N_Target_Beta[1,k]<-N-N_Combination-N_Aggrement_Table[1,k]
  }
  P_Condition_Beta<-N_Condition_Beta/N
  P_Target_Beta<-N_Target_Beta/N
  P_Beta_Error<-P_Target_Beta/P_Condition_Beta
  P_Beta_Reliability<-1-P_Beta_Error

  #Estimating the Assignment-Error-Matrix
  assignment_error_matrix<-matrix(nrow=n_categories,ncol=n_categories,data=NA)
  rownames(assignment_error_matrix)<-categorical_levels
  colnames(assignment_error_matrix)<-categorical_levels

  for(k in categorical_levels){
    assignment_error_matrix[k,k]<-P_Alpha_Error[1,k]
    categories<-categorical_levels
    categories<-subset(categories,categories!=k)
    for(k2 in categories){
      assignment_error_matrix[k,k2]<-(P_Alpha_Error[1,k]*P_Beta_Error[1,k2])/(P_Alpha_Error[1,k]*sum(P_Beta_Error[1,categories]))
    }
  }


  #Estimating normalized and chance-corrected alpha values for iota
  P_No_K<-p_cell*(n_categories-1)^n_rater
  P_Diagonal<-n_categories*p_cell

  Alpha_Rel_Chance<-p_cell/(1-P_No_K)
  Alpha_Error_Chance<-1-Alpha_Rel_Chance
  alpha<-abs((P_Alpha_Reliability-Alpha_Rel_Chance)/(1-Alpha_Rel_Chance))

  #Estimating normalized and chance-corrected beta values for iota
  P_Beta_Error_Chance_Target<-1-P_No_K-p_cell
  Beta_Error_Chance<-P_Beta_Error_Chance_Target/(1-P_Diagonal)
  Beta_Rel_Chance<-1-Beta_Error_Chance

  Realized_Beta_Error<-P_Condition_Beta*P_Beta_Error
  Realized_Beta_Error_Chance<-(1-P_Diagonal)*Beta_Error_Chance

  Realized_Beta_Reliability<-1-Realized_Beta_Error
  Realized_Beta_Reliability_Chance<-1-Realized_Beta_Error_Chance


  beta<-abs((Realized_Beta_Reliability-Realized_Beta_Reliability_Chance)/(1-Realized_Beta_Reliability_Chance))

  beta<-replace(x=beta,beta=="NaN",values = 1)
  beta<-replace(x=beta,beta==NaN,values=1)
  beta<-replace(x=beta,beta=="Inf",values=1)
  beta<-replace(x=beta,beta==Inf,values=1)
  beta<-replace(x=beta,beta=="-Inf",values=1)
  beta<-replace(x=beta,beta==-Inf,values=1)
  beta<-replace(x=beta,beta=="NA",values=1)
  beta<-replace(x=beta,beta==NA,values=1)
  #test_abc<-((1-n_categories*p_cell)*Beta_Error_Chance-P_Condition_Beta*P_Beta_Error)/((1-n_categories*p_cell)*Beta_Error_Chance)
  #print(round(test_abc,3)==round(Beta_Rel,3))



  reliability<-(alpha+beta)/2

  results<-NULL
  results["alpha"]<-list(alpha)
  results["beta"]<-list(beta)
  results["iota"]<-list(reliability)
  results["assignment_error_matrix"]<-list(assignment_error_matrix)
  results["average_iota"]<-mean(reliability)

  class(results)<-"iotarelr_iota1"

  return(results)
}

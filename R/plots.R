
#'Plot Iota2
#'
#'Function for creating a plot object that can be plotted via \link{ggplot2}.
#'
#'@param object Estimates of Iota 2 created with \code{compute_iota2()},
#'\code{check_dgf()} or \code{check_new_rater()}.
#'@param xlab \code{Character} passed to xlab() from scale_fill_manual(). Label
#'of the x-axis.
#'@param ylab \code{Character} passed to ylab() from scale_fill_manual(). Label
#'of the y-axis.
#'@param liota \code{Character} passed to labels() from scale_fill_manual().
#'Label for Iota.Amount of cases that are assigned to the correct category.
#'@param lcase2 \code{Character} passed to labels() from scale_fill_manual().
#'Label for the amount of cases that are assigned to a false category.
#'@param lcase3 \code{Character} passed to labels() from scale_fill_manual().
#'Label for the amount of cases that are assigned from a false category.
#'@param lscale_quality \code{character} passed to scale_fill_manual() determining
#'the title for the quality of a scale. Only used in conjunction with
#'\code{scale}.
#'@param lscale_cat Vector of strings with length 5. This vector contains the
#'labels for each category of quality for the scale.
#'@param number_size \code{Double} passed to geom_text() determining the size
#'of the numbers within the plot.
#'@param key_size \code{Double} passed to theme() determining the size of the
#'legend keys.
#'@param text_size \code{Double} passed to theme() determining the size of the
#'text within the legend.
#'@param scale \code{String} for requesting an additional plot of reliability
#'on the scale level. If \code{scale="dynamic_iota_index"} Dynamic Iota Index
#'is used.  If \code{scale="static_iota_index"} Static Iota Index
#'is used. If \code{scale="none"} no additional plot is created.
#'@return Function returns an object of class \code{gg, ggplot} illustrating how
#'the data of the different categories influence each other.
#'@note An example for interpreting the plot can be found in the vignette
#'\href{../iotarelr.html}{Get started} or via
#'\code{vignette("iotarelr", package = "iotarelr")}.
#'@importFrom rlang .data
#'@importFrom methods is
#'@references  Florian Berding and Julia Pargmann (2022).Iota Reliability Concept
#'of the Second Generation. Measures for Content Analysis Done by
#'Humans or Artificial Intelligences. Berlin: Logos.
#'https://doi.org/10.30819/5581
#'@export
plot_iota<-function(object,
                    xlab="Amount on all cases",
                    ylab="Categories",
                    liota="Assignment of the true category (Iota)",
                    lcase2="Assignment to the false category",
                    lcase3="Assignment from the false true category",
                    lscale_quality="Scale Quality",
                    lscale_cat=c("insufficent",
                                 "minimum",
                                 "satisfactory",
                                 "good",
                                 "excellent"),
                    number_size=6,
                    key_size=0.5,
                    text_size=10,
                    scale="none"){

  if((methods::is(object,"iotarelr_iota2") | methods::is(object,"iotarelr_iota2_dgf"))==FALSE){
    stop("Class of object for iota is not supported by this function.")
  }

  image_data<-NULL
  if(methods::is(object,"iotarelr_iota2")){
    n_group=1
  } else if(methods::is(object,"iotarelr_iota2_dgf")){
    n_group<-length(object)
  }

  for(g in 1:n_group){
    if(methods::is(object,"iotarelr_iota2")){
      tmp_object<-object
    } else if(methods::is(object,"iotarelr_iota2_dgf")){
      tmp_object<-object[[g]]
    }

    p_classes=tmp_object$information$est_true_cat_sizes
    p_alpha_reliability=tmp_object$categorical_level$raw_estimates$alpha_reliability
    p_alpha_error=1-p_alpha_reliability
    n_categories=length(p_classes)
    p_beta_error<-1-tmp_object$categorical_level$raw_estimates$beta_reliability

    beta_error_sum<-vector(length = n_categories)
    for(i in 1:n_categories){
      beta_error_sum[i]<-sum(p_classes*p_alpha_error)-
        p_classes[i]*p_alpha_error[i]
    }
    iota_numerator<-p_classes*p_alpha_reliability
    iota_denominator<-p_classes*p_alpha_reliability+
      p_classes*p_alpha_error+
      p_beta_error*beta_error_sum

    iota<-iota_numerator/iota_denominator
    case_2<-p_alpha_error*p_classes/iota_denominator
    case_3<-p_beta_error*beta_error_sum/iota_denominator

    names(iota)<-names(p_classes)
    names(case_2)<-names(p_classes)
    names(case_3)<-names(p_classes)

    tmp<-t(rbind(iota,case_2,case_3))

    tmp_image_data<-NULL
    for(i in 1:nrow(tmp)){
      for (j in 1:3){
        if(methods::is(object,"iotarelr_iota2")){
          tmp_category=rownames(tmp)[i]
          tmp_group<-NA
        } else if(methods::is(object,"iotarelr_iota2_dgf")) {
          tmp_category=rownames(tmp)[i]
          tmp_group<-names(object)[g]
        }

        tmp_image_data<-rbind(tmp_image_data,c(tmp_category,
                                               tmp[i,j],
                                               tmp[i,j]/sum(tmp[i,]),
                                               sum(tmp[i,0:(j-1)])/sum(tmp[i,]),
                                               sum(tmp[i,1:j])/sum(tmp[i,]),
                                               colnames(tmp)[j],
                                               tmp_group)
        )
      }
    }
    tmp_image_data<-as.data.frame(tmp_image_data)
    colnames(tmp_image_data)<-c("category","value", "amount","position_l","position_r","case","group")
    tmp_image_data$amount<-as.numeric(tmp_image_data$amount)
    tmp_image_data$value<-as.numeric(tmp_image_data$value)
    tmp_image_data$position_l<-as.numeric(tmp_image_data$position_l)
    tmp_image_data$position_r<-as.numeric(tmp_image_data$position_r)
    tmp_image_data$case<-factor(as.character(tmp_image_data$case),
                                levels=c("case_3",
                                         "case_2",
                                         "iota"))
    image_data<-rbind(image_data,tmp_image_data)
  }


  image_plot<-ggplot2::ggplot(data=image_data)
  if(methods::is(object,"iotarelr_iota2")){
    image_plot<-image_plot+
      ggplot2::geom_col(position="stack",
                        ggplot2::aes(y=.data$category,
                                     x=.data$amount,
                                     fill=.data$case))+
      ggplot2::geom_text(ggplot2::aes(x=(.data$position_l+.data$position_r)*0.5,
                                      y=.data$category,
                                      label = round(.data$amount,digits=3)),
                         size = number_size, hjust = 0.5, vjust = 0, check_overlap=TRUE)+
      ggplot2::ylab(ylab)
  } else if(methods::is(object,"iotarelr_iota2_dgf")){
    image_plot<-image_plot+
      ggplot2::geom_col(position="stack",
                        ggplot2::aes(y=.data$group,
                                     x=.data$amount,
                                     fill=.data$case))+
      ggplot2::facet_grid(category ~ .)+
      ggplot2::geom_text(ggplot2::aes(x=(.data$position_l+.data$position_r)*0.5,
                                      y=.data$group,
                                      label = round(.data$amount,digits=3)),
                         size = number_size, hjust = 0.5, vjust = 0, check_overlap=TRUE)+
      ggplot2::ylab(ylab)
  }
  image_plot<-image_plot+
    ggplot2::scale_fill_manual(
      values=c("iota"="darkgreen",
               "case_2"="orange",
               "case_3"="red"),
      labels=c("iota"=liota,
               "case_2"=lcase2,
               "case_3"=lcase3),
      name="")+
    ggplot2::xlab(xlab)+
    ggplot2::theme_classic()+
    ggplot2::theme(legend.position="bottom",
                   legend.justification = "left",
                   legend.key.size = ggplot2::unit(key_size, "cm"),
                   legend.text = ggplot2::element_text(size=text_size),
                   legend.direction="vertical")

  ##-------------------------------Image for the scale level
  cut_off_values<-matrix(c(0.829, 0.961, 0.985, 1,
                           0.686, 0.853, 0.898, 1),
                         byrow=TRUE,
                         nrow = 2)
  rownames(cut_off_values)<-c("dynamic_iota_index",
                              "static_iota_index")

  if(scale!="none"){
    scale_cat_colors<-c("red",
                        "orange",
                        "yellow",
                        "green",
                        "darkgreen")
    names(scale_cat_colors)<-lscale_cat
    image_scale<-ggplot2::ggplot()+
      ggplot2::geom_rect(ggplot2::aes(xmin = 0,
                                      xmax = cut_off_values[scale,1],
                                      ymin = 0,
                                      ymax = 1,
                                      fill=lscale_cat[1]),
                         color="black")+
      ggplot2::geom_rect(ggplot2::aes(xmin = cut_off_values[scale,1],
                                      xmax = cut_off_values[scale,2],
                                      ymin = 0,
                                      ymax = 1,
                                      fill=lscale_cat[2]),
                         color="black")+
      ggplot2::geom_rect(ggplot2::aes(xmin = cut_off_values[scale,2],
                                      xmax = cut_off_values[scale,3],
                                      ymin = 0,
                                      ymax = 1,
                                      fill=lscale_cat[3]),
                         color="black")+
      ggplot2::geom_rect(ggplot2::aes(xmin = cut_off_values[scale,3],
                                      xmax = cut_off_values[scale,4],
                                      ymin = 0,
                                      ymax = 1,
                                      fill=lscale_cat[4]),
                         color="black")+
      ggplot2::geom_rect(ggplot2::aes(xmin = cut_off_values[scale,4],
                                      xmax = 1,
                                      ymin = 0,
                                      ymax = 1,
                                      fill=lscale_cat[5]),
                         color="black")+
      ggplot2::scale_fill_manual(values=scale_cat_colors,
                                 name=lscale_quality)+
      ggplot2::theme_classic()+
      ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank())
    if(methods::is(object,"iotarelr_iota2")){
      if(scale=="dynamic_iota_index"){
        image_scale=image_scale+
          ggplot2::geom_vline(xintercept = object$scale_level$iota_index_dyn2,
                              size=2)+
          ggplot2::labs(x="Dynamic Iota Index",
                        y="")
      } else if(scale=="static_iota_index") {
        image_scale=image_scale+
          ggplot2::geom_vline(xintercept = object$scale_level$iota_index_d4,
                              size=2)+
          ggplot2::labs(x="Static Iota Index",
                        y="")
      }
    } else if(methods::is(object,"iotarelr_iota2_dgf")){
      for(g in 1:n_group){
        if(scale=="dynamic_iota_index"){
          image_scale=image_scale+
            ggplot2::geom_vline(xintercept = object[[g]]$scale_level$iota_index_dyn2,
                                size=1.5)+
            ggplot2::annotate(geom = "text",
                              x = object[[g]]$scale_level$iota_index_dyn2-.02,
                              y = 0.5,
                              label = names(object)[g],
                              angle = 90,
                              check_overlap = TRUE)+
            ggplot2::labs(x="Dynamic Iota Index",
                          y="")
        } else if(scale=="static_iota_index") {
          image_scale=image_scale+
            ggplot2::geom_vline(xintercept = object[[g]]$scale_level$iota_index_d4,
                                size=1.5)+
            ggplot2::annotate(geom = "text",
                              x = object[[g]]$scale_level$iota_index_dyn2-.02,
                              y = 0.5,
                              label = names(object)[g],
                              angle = 90,
                              check_overlap = TRUE)+
            ggplot2::labs(x="Static Iota Index",
                          y="")
        }
      }
    }

  }

  if(scale=="none"){
    return(image_plot)
  } else {
    image_complete<-gridExtra::arrangeGrob(image_plot,
                                           image_scale,
                                           layout_matrix=matrix(c(1,1,1,2),
                                                                ncol = 1))
    return(image_complete)
  }
}

#'Plot of the Coding Stream
#'
#'Function for creating an alluvial plot that can be plotted via \link{ggplot2}.
#'
#'@param object Estimates of Iota 2 created with \code{compute_iota2()},
#'\code{check_new_rater()} or with \code{check_dgf()}. Please note that the object
#'created by \code{check_dgf()} cannot be passed directly. Only the elements of
#'the corresponding list are compatible.
#'@param label_titel \code{Character} containing the title of the plot.
#'@param label_prefix_true \code{Character} representing the prefix for tagging
#'the true categories. Character is applied to every category.
#'@param label_prefix_assigned \code{Character} representing the prefix for tagging
#'the assigned categories. Character is applied to every category.
#'@param label_legend_title \code{Character} containing the title of the legend.
#'@param label_true_category \code{Character} describing the stratum of true
#'categories.
#'@param label_assigned_category \code{Character} describing the stratum of
#'assigned categories.
#'@param label_y_axis \code{Character}. Label of the y-axis.
#'@param label_categories_size \code{double} determining the size of the
#'label for each true and assigned category within the plot.
#'@param key_size \code{double} determining the size of the legend.
#'@param text_size \code{double} determining the size of the text within the legend.
#'@return Returns an object of class \code{gg} and \code{ggplot} which can be
#'shown with \code{plot()}.
#'@note An example for interpreting the plot can be found in the vignette
#'\href{../iotarelr.html}{Get started} or via
#'\code{vignette("iotarelr", package = "iotarelr")}.
#'@importFrom rlang .data
#'@import ggplot2
#'@import ggalluvial
#'@export
plot_iota2_alluvial<-function(object,
                              label_titel="Coding Stream from True to Assigned Categories",
                              label_prefix_true="true",
                              label_prefix_assigned="labeled as",
                              label_legend_title="True Categories",
                              label_true_category="True Category",
                              label_assigned_category="Assigned Category",
                              label_y_axis="Relative Frequencies",
                              label_categories_size=3,
                              key_size=0.5,
                              text_size=10){
  if(methods::is(object,"iotarelr_iota2")==FALSE){
    stop("Class of object for iota is not supported by this function.
         Object must be of class iotarelr_iota2.")
  }

  n_categories=ncol(object$categorical_level$raw_estimates$assignment_error_matrix)
  categorical_levels<-colnames(object$categorical_level$raw_estimates$assignment_error_matrix)
  alluvial_data<-matrix(data=NA,
                         nrow=n_categories*n_categories,
                         ncol=3)
  colnames(alluvial_data)<-c("true_category","assigned_category","freq")
  index=1
  for(i in 1:n_categories){
    for(j in 1:n_categories){
      alluvial_data[index,1]<-paste(label_prefix_true,categorical_levels[i])
      alluvial_data[index,2]<-paste(label_prefix_assigned,categorical_levels[j])
      alluvial_data[index,3]<-object$information$est_true_cat_sizes[i]*
        object$categorical_level$raw_estimates$assignment_error_matrix[i,j]
      index=index+1
    }
  }
  alluvial_data<-as.data.frame(alluvial_data)
  alluvial_data$freq<-as.numeric(alluvial_data$freq)
  gg_alluvival_plot<-ggplot2::ggplot(
    alluvial_data,
    ggplot2::aes(
      axis1 = .data$true_category,
      axis2 = .data$assigned_category,
      y = .data$freq)) +
    ggalluvial::geom_alluvium(ggplot2::aes(fill=.data$true_category),
                              width = 0,
                              knot.pos = 0,
                              curve_type = "sigmoid",
                              reverse = FALSE)+
    ggplot2::scale_fill_discrete(name=label_legend_title)+
    ggalluvial::geom_stratum(width = 1/8,
                             reverse = FALSE)+
    ggplot2::geom_text(stat = "stratum",
                       ggplot2::aes(label = ggplot2::after_stat(.data$stratum)),
                       reverse = FALSE,
                       size = label_categories_size) +
    ggplot2::scale_x_continuous(breaks = 1:2,
                                labels = c(label_true_category, label_assigned_category))+
    ggplot2::theme_classic()+
    ggtitle(label_titel)+
    ggplot2::ylab(label_y_axis)+
    ggplot2::theme(legend.position="right",
                   legend.justification = "left",
                   legend.key.size = ggplot2::unit(key_size, "cm"),
                   legend.text = ggplot2::element_text(size=text_size),
                   legend.direction="vertical")
  return(gg_alluvival_plot)
}

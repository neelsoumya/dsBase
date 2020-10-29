#' 
#' @title Performs plotting of survival analysis curves.
#' @description returns a privacy preserving survival curves.
#' @details Serverside aggregate function {plotsurvfitDS} called by clientside function.
#' {ds.plotsurvfit}.
#' returns a privacy preserving survival curve from the server side environment.
#' This request is not disclosive as it is randomized.
#' For further details see help for {ds.coxphSLMA} function.
#' @param formula either NULL or a character string (potentially including '*'
#' wildcards) specifying a formula.
#' @param dataName character string of name of data frame
#' @param weights vector of case weights
#' @param init vector of initial values of the iteration
#' @param ties character string specifying the method for tie handling.
#'          The Efron approximation is used as the default. Other options are
#'          'breslow' and 'exact'.
#' @param singular.ok Logical value indicating how to handle collinearity in the model matrix.
#'        Default is TRUE. If TRUE, the program will automatically skip over columns of the 
#'        X matrix that are linear combinations of earlier columns. In this case the coefficients
#'        of such columns will be NA and the variance matrix will contain zeros.
#' @param model logical value. If TRUE, the model frame is returned in component model. 
#' @param x logical value. If TRUE, the x matrix is returned in component x.
#' @param y logical value. If TRUE, the response vector is returned in component y.
#' @param control object of type survival::coxph.control() specifying iteration limit and other
#'        control options. Default is survival::coxph.control()
#' @return a summary of the Cox proportional hazards from the server side environment from the server side environment.
#' @author Soumya Banerjee and Tom Bishop (2020).
#' @export
plotsurvfitDS<-function(formula = NULL,
                      dataName = NULL,
                      weights = NULL,
                      init = NULL,
                      ties = 'efron',
                      singular.ok = TRUE,
                      model = FALSE,
                      x = FALSE,
                      y = TRUE,
                      control = NULL
                     )
{
      
      errorMessage <- "No errors"
      
      #########################################################################
      # DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS                       #
      thr <- listDisclosureSettingsDS()                                       #
      #nfilter.tab<-as.numeric(thr$nfilter.tab)                               #
      #nfilter.glm<-as.numeric(thr$nfilter.glm)                               #
      #nfilter.subset<-as.numeric(thr$nfilter.subset)                         #
      nfilter.string <- as.numeric(thr$nfilter.string)                        #
      nfilter.tab    <- as.numeric(thr$nfilter.tab)                           #
      nfilter.glm    <- as.numeric(thr$nfilter.glm)                           #
      #nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)               #
      #nfilter.kNN<-as.numeric(thr$nfilter.kNN)                               #
      #datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)       #
      #########################################################################
      
      # get the value of the 'data' and 'weights' parameters provided as character on the client side
      if(is.null(dataName))
      {
         dataTable <- NULL 
      }
      else
      {
         dataTable <- eval(parse(text=dataName), envir = parent.frame())
      }
      
      # check if formula is set
      if (is.null(formula))
      {
         stop("The formula must be set for use in survival::coxph()", call.=FALSE)
      } 	
	
      
      ###########################
      # disclosure checks
      ###########################
	
      # get survfit model
      survfit_model_variable = eval(parse(text=formula), envir = parent.frame())
      
      # TODO: modify by adding random noise to time, n.risk, n.event, conf.int, etc.
      # call plot.survival()
      # return that	
      # OR just return modified survfit object
      # add random noise
 
      # TODO: use datashield function for this	
      set.seed(100)	
      survfit_model_variable$surv    <- abs(stats::rnorm(n = length(survfit_model_variable$surv), mean = survfit_model_variable$surv, sd = 0.1))
      survfit_model_variable$n.event <- abs(stats::rnorm(n = length(survfit_model_variable$n.event), mean = survfit_model_variable$n.event, sd = 0.1))
      survfit_model_variable$n.risk  <- abs(stats::rnorm(n = length(survfit_model_variable$n.risk), mean = survfit_model_variable$n.risk, sd = 0.1))
      survfit_model_variable$lower   <- abs(stats::rnorm(n = length(survfit_model_variable$lower), mean = survfit_model_variable$lower, sd = 0.1))
      survfit_model_variable$upper   <- abs(stats::rnorm(n = length(survfit_model_variable$upper), mean = survfit_model_variable$upper, sd = 0.1))
	
      # TODO: modify conf.int	
      
      return(survfit_model_variable)
}
#AGGREGATE FUNCTION
# plotsurvfitDS

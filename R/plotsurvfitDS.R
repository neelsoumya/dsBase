#' 
#' @title Performs plotting of survival analysis curves.
#' @description returns a privacy preserving survival curve.
#' @details Serverside aggregate function {plotsurvfitDS} called by clientside function.
#' {ds.plotsurvfit}.
#' returns a privacy preserving survival curve from the server side environment.
#' This request is not disclosive as it is randomized.
#' For further details see help for {ds.coxphSLMA} function.
#' @param formula a character string which has the name of server-side survfit() object.
#'		This should be created using ds.survfit()
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
      nfilter.noise  <- as.numeric(thr$nfilter.noise)                         #
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
      
      # call plot.survival()
      # return that	
      # OR just return modified survfit object
      # add random noise
 
      # TODO: use datashield function for this	
      #	   https://rdrr.io/github/datashield/dsBase/man/setSeedDS.html
      #    dsBase::setSeedDS(100)	
      # set.seed(100)	
      
      # set method to probabilistic
      # TODO: make arguments
      method_anonymization = 2
      noise = 0.003 # 0.03 0.26
	
      # TODO: make monotonic
      # TODO: determinsitic algorithm
      # TODO: rgrenrate CIs based on noisy added data
      # TODO: check for strata at least 10 in each
      # TODO: combine across different studies (do this in ds.plotsurvfit last line combine OR not) 	
      # TODO: cloglog diagnostic plots	

      # if probabilistic anonymization then generate and add noise	
      if (method_anonymization == 2)
      {	      
	      
	      # set study specific seed
	      seed <- getOption("datashield.seed")
	      if (is.null(seed))
	      {
		      stop("plotsurvfitDS requires a seed to be set and requires 'datashield.seed' R option to operate", call.=FALSE)
	      }

	      # if there is a seed, then set it
	      set.seed(seed)	

	      # Check if the percentage of the variance that is specified in the argument 'noise'
	      # and is used as the variance of the embedded noise is a greater
	      # than the minimum threshold specified in the filter 'nfilter.noise'

	      # if(noise < nfilter.noise)
	      # {
	      #	  stop(paste0("'noise' must be greater than or equal to ", nfilter.noise), call.=FALSE)
	      # }
	      # else
	      # {
	      #	  percentage <- noise
	      # }

	      percentage <- noise

	      
	      # for loop and only subtract to build a monotonically decreasing function 
	      # add noise to all components of survfit object	
	      # NOTE: start from 2nd index
	      for ( i_temp_counter_inner in c(2:length(survfit_model_variable$surv)) )
	      {
		      # current value at this index
		      value_temp <- survfit_model_variable$surv[i_temp_counter_inner]
		      # previous value
		      prev_value_temp <- survfit_model_variable$surv[i_temp_counter_inner - 1]
		      
		      # add some noise 
		      # TODO: make noise a percentage of previous OR current value
		      # delta_noise <- abs(stats::rnorm(n = 1, mean = value_temp, sd = percentage * value_temp))
		      delta_noise <- abs(stats::rnorm(n = 1, mean = 0, sd = percentage))
					 
		      # SUBTRACT this noise from the PREVIOUS VALUE			 
		      survfit_model_variable$surv[i_temp_counter_inner] <- prev_value_temp - delta_noise
	      }	      
		   
	      # survfit_model_variable$surv    <- abs(stats::rnorm(n = length(survfit_model_variable$surv), mean = survfit_model_variable$surv, sd = percentage * survfit_model_variable$surv ))
	      
	      # TODO: now do this for n.event	      
	      survfit_model_variable$n.event <- abs(stats::rnorm(n = length(survfit_model_variable$n.event), mean = survfit_model_variable$n.event, sd = percentage * survfit_model_variable$n.event ))
	      
	      # do this for n.risk
	      for ( i_temp_counter_inner in c(2:length(survfit_model_variable$n.risk)) )
	      {
		      # current value at this index
		      value_temp <- survfit_model_variable$n.risk[i_temp_counter_inner]
		      # previous value
		      prev_value_temp <- survfit_model_variable$n.risk[i_temp_counter_inner - 1]
		      
		      # add some noise 
		      # TODO: make noise a percentage of previous OR current value
		      # delta_noise <- abs(stats::rnorm(n = 1, mean = value_temp, sd = percentage * value_temp))
		      delta_noise <- abs(stats::rnorm(n = 1, mean = 0, sd = percentage))
					 
		      # SUBTRACT this noise from the PREVIOUS VALUE			 
		      survfit_model_variable$n.risk[i_temp_counter_inner] <- prev_value_temp - delta_noise
	      }
	      
	      # survfit_model_variable$n.risk  <- abs(stats::rnorm(n = length(survfit_model_variable$n.risk), mean = survfit_model_variable$n.risk, sd = percentage * survfit_model_variable$n.risk ))
	      
	      # do this for lower
	      for ( i_temp_counter_inner in c(2:length(survfit_model_variable$lower)) )
	      {
		      # current value at this index
		      value_temp <- survfit_model_variable$lower[i_temp_counter_inner]
		      # previous value
		      prev_value_temp <- survfit_model_variable$lower[i_temp_counter_inner - 1]
		      
		      # add some noise 
		      # TODO: make noise a percentage of previous OR current value
		      # delta_noise <- abs(stats::rnorm(n = 1, mean = value_temp, sd = percentage * value_temp))
		      delta_noise <- abs(stats::rnorm(n = 1, mean = 0, sd = percentage))
					 
		      # SUBTRACT this noise from the PREVIOUS VALUE			 
		      survfit_model_variable$lower[i_temp_counter_inner] <- prev_value_temp - delta_noise
	      }
	      
	      # survfit_model_variable$lower   <- abs(stats::rnorm(n = length(survfit_model_variable$lower), mean = survfit_model_variable$lower, sd = percentage * survfit_model_variable$lower ))
	      
	      # do this for upper
	      for ( i_temp_counter_inner in c(2:length(survfit_model_variable$upper)) )
	      {
		      # current value at this index
		      value_temp <- survfit_model_variable$upper[i_temp_counter_inner]
		      # previous value
		      prev_value_temp <- survfit_model_variable$upper[i_temp_counter_inner - 1]
		      
		      # add some noise 
		      # TODO: make noise a percentage of previous OR current value
		      # delta_noise <- abs(stats::rnorm(n = 1, mean = value_temp, sd = percentage * value_temp))
		      delta_noise <- abs(stats::rnorm(n = 1, mean = 0, sd = percentage))
					 
		      # SUBTRACT this noise from the PREVIOUS VALUE			 
		      survfit_model_variable$upper[i_temp_counter_inner] <- prev_value_temp - delta_noise
	      }
	      
	      # survfit_model_variable$upper   <- abs(stats::rnorm(n = length(survfit_model_variable$upper), mean = survfit_model_variable$upper, sd = percentage * survfit_model_variable$upper ))
	      
	      # survfit_model_variable$conf.int <- abs(stats::rnorm(n = length(survfit_model_variable$conf.int), mean = survfit_model_variable$conf.int, sd = percentage * survfit_model_variable$conf.int ))
	      
	      # TODO: survfit_model_variable$std.err
	      
	      # TODO: create a Surv() server side object, add noise and then recalculate CIs
	      #  do this for # survfit_model_variable$std.err
	      # create a new object
	      survfit_model_variable_modified <- NULL
	      survfit_model_variable_modified$surv <- survfit_model_variable$surv
	      survfit_model_variable_modified$n.event <- survfit_model_variable$n.event
	      survfit_model_variable_modified$n.risk <- survfit_model_variable$n.risk
	      survfit_model_variable_modified$lower <- survfit_model_variable$lower
	      survfit_model_variable_modified$upper <- survfit_model_variable$upper
	      survfit_model_variable_modified$conf.int <- survfit_model_variable$conf.int
	      survfit_model_variable_modified$time <- survfit_model_variable$time
	      
      }
	      
      # TODO: create a new object and return that; alternatively delete all other components of survfit object	
      # return(list(x.new, y.new))
	
      return(survfit_model_variable)
      # return(survfit_model_variable_modified)
      # TODO: error fix error in xy.coords(x, y, xlabel, ylabel, log)
      # 	x is a list but does not have components x and y	
	
}
#AGGREGATE FUNCTION
# plotsurvfitDS

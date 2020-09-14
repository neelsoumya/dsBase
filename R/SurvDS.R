#' 
#' @title Creates a survival object for survival analysis using the Cox proportional hazards model at the serverside environment
#' @description returns a summary of the Cox proportional hazards from the server side environment.
#' @details Serverside assign function {SurvDS} called by clientside function.
#' {ds.Surv}.
#' returns a Survival object for use in Cox proportional hazards from the server side environment from the server side environment.
#' This request is not disclosive as it only returns a string.
#' For further details see help for {ds.Surv} function.
#' @param start name of start time parameter to be passed to Surv(). 
#'      Should be a character string.
#' @param stop name of stop time parameter to be passed to Surv(). 
#'      Should be a character string.
#' @param event name of event parameter to be passed to Surv()
#'      Should be character string.
#' @return a survival::Surv() object from the server side environment.
#' @author Soumya Banerjee and Tom Bishop (2020).
#' @export
SurvDS<-function(start=NULL, stop=NULL, event=NULL)
{
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
      
    
      #################################
      # check type of all parameters
      #################################
      # check for start parameter
      class_start <- dsBase::classDS(x=start)
      if ( !('numeric' %in% class_start) & !('integer' %in% class_start) )
      {
            stop('Start time parameter (start) must be numeric or integer.', call.=FALSE)
      }
      
      # check for stop parameter
      class_stop <- dsBase::classDS(x=stop)
      if( !('numeric' %in% class_stop) & !('integer' %in% class_stop) )
      {
            stop('Stop time parameter (stop) must be numeric or integer.', call.=FALSE)
      }
      
      # construct a call to Surv function with these parameters
      # surv_object <- survival::Surv(time = SURVTIME, event = EVENT)
      # str_command = paste0('survival::Surv(time = ', time)
      str_command = paste0('survival::Surv(time = ', start)
      str_command = paste0(str_command, ', time2 = ') 
      str_command = paste0(str_command, stop)
      str_command = paste0(str_command, ', event = ') 
      str_command = paste0(str_command, event)
      str_command = paste0(str_command, ')')
      
      # evaluate this
      surv_object <- eval(parse(text=str_command), envir = parent.frame())
      
      # surv_object <- eval(parse(text='survival::Surv(time = SURVTIME, event = EVENT)'), envir = parent.frame())
      
      return(surv_object)
}
#ASSIGN FUNCTION
# SurvDS

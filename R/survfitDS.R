#' 
#' @title Creates a survival survfit object for survival analysis at the serverside environment. 
#'   This is to be used for eventually plotting survival models.
#' @description creates a survfit survival object in the server side environment.
#' @details Serverside assign function {survfitDS} called by clientside function.
#' {ds.survfit}.
#' creates a survfit survival object in the server side environment
#' This request is not disclosive.
#' For further details see help for {ds.survfit} function.
#' @param time name of time parameter to be passed to Surv(). 
#'      Should be a character string.
#' @param event name of event parameter to be passed to Surv()
#'      Should be character string.
#' @return creates a survfit survival object in the server side environment.
#' @author Soumya Banerjee (2020).
#' @export
survfitDS<-function(time=NULL, event=NULL)
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
      
    
      # construct a call to Surv function with these parameters
      
      time  = "SURVTIME"
      event = "EVENT"
      
      # surv_object <- survival::Surv(time = SURVTIME, event = EVENT)
      str_command = paste0('survival::Surv(time = ', time)
      str_command = paste0(str_command, ', event = ') 
      str_command = paste0(str_command, event)
      str_command = paste0(str_command, ')')
      
      # evaluate this
      surv_object <- eval(parse(text=str_command), envir = parent.frame())
      
      survfit_object <- survival::survfit(surv_object ~ 1)
      
      # surv_object <- eval(parse(text='survival::Surv(time = SURVTIME, event = EVENT)'), envir = parent.frame())
      
      # surv_object <- "HellofromSurvDS"
      
      # cat('\n Hello World from server-side function coxphSLMADS() in dsBase \n')
      # temp_str <- 'Hello World from server-side dsBase::coxphSLMADS()'
      # outlist <- paste0(search.filter, temp_str)
      # return(outlist)
      
      return(survfit_object)
}
#ASSIGN FUNCTION
# survfitDS

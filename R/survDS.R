#' 
#' @title Creates a survival object for survival analysis using the Cox proportional hazards model at the serverside environment
#' @description returns a summary of the Cox proportional hazards from the server side environment.
#' @details Serverside aggregate function {coxphSLMADS} called by clientside function.
#' {ds.coxphSLMA}.
#' returns a summary of the Cox proportional hazards from the server side environment from the server side environment.
#' This request is not disclosive as it only returns a string.
#' For further details see help for {ds.coxphSLMA} function.
#' @param formula either NULL or a character string (potentially including '*'
#' wildcards) specifying a formula.
#' @param dataName character string of name of data frame
#' @return a summary of the Cox proportional hazards from the server side environment from the server side environment.
#' @author Soumya Banerjee (2020).
#' @export
SurvDS<-function(formula=NULL, dataName=NULL)
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
      
    
      
      #surv_object <- survival::Surv(time = SURVTIME, event = EVENT)
      
      surv_object <- "Hello from SurvDS"
      
      # cat('\n Hello World from server-side function coxphSLMADS() in dsBase \n')
      # temp_str <- 'Hello World from server-side dsBase::coxphSLMADS()'
      # outlist <- paste0(search.filter, temp_str)
      # return(outlist)
      return(surv_object)
}
#ASSIGN FUNCTION
# SurvDS

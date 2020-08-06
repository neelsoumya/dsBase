#' 
#' @title returns a string from a serverside environment
#' @description returns a string from the server side environment afetr appending it to a string passed from the client side
#' @details Serverside aggregate function {retStrDS} called by clientside function.
#' {ds.retStr}.
#' returns a string from the server side environment afetr appending it to a string passed from the client side. 
#' This request is not disclosive as it only returns a string.
#' For further details see help for {ds.retStr} function.
#' @param search.filter either NULL or a character string (potentially including '*'
#' wildcards) specifying required search criteria. This argument is
#' fully specified by its corresponding argument in the clientside function.
#' @return a string containing a Hello World from server-side appended to whatever string was passed from client-side
#' @author Soumya Banerjee (2020).
#' @export
coxphSLMADS<-function(search.filter=NULL)
{
      #########################################################################
      # DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS                       #
      thr <- listDisclosureSettingsDS()                                       #
      #nfilter.tab<-as.numeric(thr$nfilter.tab)                               #
      #nfilter.glm<-as.numeric(thr$nfilter.glm)                               #
      #nfilter.subset<-as.numeric(thr$nfilter.subset)                         #
      nfilter.string<-as.numeric(thr$nfilter.string)                          #
      #nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)               #
      #nfilter.kNN<-as.numeric(thr$nfilter.kNN)                               #
      #datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)       #
      #########################################################################

      cat('\n Hello World from server-side function retStrDS() in dsBase \n')
      temp_str <- 'Hello World from server-side dsBase::retStrDS()'
      outlist <- paste0(search.filter, temp_str)
      return(outlist)
}
#AGGREGATE FUNCTION
# coxphSLMADS

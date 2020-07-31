#' 
#' @title returns a string from a serverside environment
#' @description creates a list of the names of all of the objects in
#' a specified serverside environment
#' @details Serverside aggregate function {retStrDS} called by clientside function
#' {ds.retStr}. When running analyses one may want to know the objects already generated. This 
#' request is not disclosive as it only returns the names of the objects and not their contents. 
#' By default, objects in the current 'active analytic environment' (".GlobalEnv")
#' will be displayed. This
#' is the environment that contains all of the objects that serverside DataSHIELD 
#' is using for the main analysis or has written out to the serverside during the process
#' of managing or undertaking the analysis (variables, scalars, matrices, data.frames etc).
#' For further details see help for {ds.retStr} function and for native R function {ls}
#' @param search.filter either NULL or a character string (potentially including '*'
#' wildcards) specifying required search criteria. This argument is
#' fully specified by its corresponding argument in the clientside function.
#' @param env.to.search integer (e.g. in a format such as '2' or '5L' format) specifying
#' the position in the search path of the environment to be explored. This argument is
#' fully specified by its corresponding argument in the clientside function.
#' @return a list containing: (1) the name/details of the serverside R environment
#' which {ds.ls} has searched; (2) a vector of character strings giving the names of
#' all objects meeting the naming criteria specified by the argument <search.filter> in this
#' specified R serverside environment; (3) the nature of the search filter string as it was
#' actually applied
#' @author Gaye, A (2015). Updated and extended by Paul Burton (2020).
#' @export
retStrDS<-function(search.filter=NULL,env.to.search)
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
# retStrDS

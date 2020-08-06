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
#' @param dataName character string of name of data frame
#' @return a string containing a Hello World from server-side appended to whatever string was passed from client-side
#' @author Soumya Banerjee (2020).
#' @export
coxphSLMADS<-function(search.filter=NULL, dataName=NULL)
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

      
      
      # get the value of the 'data' and 'weights' parameters provided as character on the client side
      if(is.null(dataName)){
         dataTable <- NULL 
      }else{
         dataTable <- eval(parse(text=dataName), envir = parent.frame())
      }
      
      #    formulatext <- Reduce(paste, deparse(formula))
      #    originalFormula <- formulatext
      #   
      # # Convert formula string into separate variable names split by |
      #   formulatext <- gsub(" ", "", formulatext, fixed=TRUE)
      #   formulatext <- gsub("~", "|", formulatext, fixed=TRUE)
      #   formulatext <- gsub("+", "|", formulatext, fixed=TRUE)
      #   formulatext <- gsub("*", "|", formulatext, fixed=TRUE)
      #   formulatext <- gsub("||", "|", formulatext, fixed=TRUE)
      # 
      #    formula2use <- stats::as.formula(paste0(Reduce(paste, deparse(originalFormula))), env = parent.frame()) # here we need the formula as a 'call' object
      
      # formula2use <- formula
      # mod.glm.ds <- stats::glm(formula2use, family=family, x=TRUE, control=stats::glm.control(maxit=1), contrasts=NULL, data=dataTable)
      #cxph_serverside <- survival::coxph(formula = survival::Surv(time = survtime, event = cens) ~  female,
      #                                   data = dataTable)
      cxph_serverside <- survival::coxph(formula = survival::Surv(time = SURVTIME, event = EVENT) ~  1,
                                         data = dataTable)
      cxph_serverside <- survival::coxph(formula = survival::Surv(time = SURVTIME, event = EVENT) ~  D$female,
                                         data = dataTable)
      
      
      
      cat('\n Hello World from server-side function coxphSLMADS() in dsBase \n')
      temp_str <- 'Hello World from server-side dsBase::coxphSLMADS()'
      outlist <- paste0(search.filter, temp_str)
      #return(outlist)
      return(summary(cxph_serverside))
}
#AGGREGATE FUNCTION
# coxphSLMADS

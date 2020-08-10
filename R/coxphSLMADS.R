#' 
#' @title Performs survival analysis using the Cox proportional hazards model at the serverside environment
#' @description returns a summary of the Cox proportional hazards from the server side environment.
#' @details Serverside aggregate function {coxphSLMADS} called by clientside function.
#' {ds.coxphSLMA}.
#' returns a summary of the Cox proportional hazards from the server side environment from the server side environment.
#' This request is not disclosive as it only returns a string.
#' For further details see help for {ds.coxphSLMA} function.
#' @param search.filter either NULL or a character string (potentially including '*'
#' wildcards) specifying a formula.
#' @param dataName character string of name of data frame
#' @return a summary of the Cox proportional hazards from the server side environment from the server side environment.
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
      
      # TODO:fix later
      formula = search.filter
      
      # Put pipes back into formula
      #formula = as.formula(paste(formula,collapse="|"))
      formula <- Reduce(paste, deparse(formula))
      formula <- gsub("sssss", "survival::Surv(", formula, fixed = TRUE)
      formula <- gsub("lll", "=", formula, fixed = TRUE)
      formula <- gsub("xxx", "|", formula, fixed = TRUE)
      formula <- gsub("yyy", "(", formula, fixed = TRUE)
      formula <- gsub("zzz", ")", formula, fixed = TRUE)
      formula <- gsub("ppp", "/", formula, fixed = TRUE)
      formula <- gsub("qqq", ":", formula, fixed = TRUE)
      formula <- gsub("rrr", ",", formula, fixed = TRUE)

      formula <- stats::as.formula(formula)
      
      # TODO: fix later
      search.filter = formula
      
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
      
      #cxph_serverside <- survival::coxph(formula = survival::Surv(time = SURVTIME, event = EVENT) ~  1,
      #                                   data = dataTable)
      
      cxph_serverside <- survival::coxph(formula = survival::Surv(time = SURVTIME, event = EVENT) ~  D$female,
                                         data = dataTable)
      
      cxph_serverside <- survival::coxph(formula = formula,
                                         data = dataTable)
      
      # cat('\n Hello World from server-side function coxphSLMADS() in dsBase \n')
      # temp_str <- 'Hello World from server-side dsBase::coxphSLMADS()'
      # outlist <- paste0(search.filter, temp_str)
      # return(outlist)
      return(summary(cxph_serverside))
}
#AGGREGATE FUNCTION
# coxphSLMADS

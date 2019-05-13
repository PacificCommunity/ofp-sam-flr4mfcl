# A bunch of HCR functions

#' hcr_threshold
#'
#' Typical HCR function with the parameters sbsbf0_min, sbsbf0_max, out_min, out_max passed as a vector.
#' The default HCR for the MFCLMSEControl class.
#'
#' @param sbsbf0 Numeric vector of SBSBF0.
#' @param params Numeric vector of HCR parameters.
#'
#' @rdname hcr_funcs
#' @return Numberic vector of the HCR output
hcr_threshold <- function(sbsbf0, params){
  # The standard threshold type of HCR
  if (!all(names(params) %in% c('sbsbf0_min', 'sbsbf0_max', 'out_min', 'out_max'))){
    stop("HCR parameter names do not match those in the HCR function\n")
  }
  sbsbf0_min <- params['sbsbf0_min']
  sbsbf0_max <- params['sbsbf0_max']
  out_min <- params['out_min']
  out_max <- params['out_max']
  # Do in steps to prevent further cockups
  # Get the slope
  grad <- (out_max - out_min) / (sbsbf0_max - sbsbf0_min)
  intercept <- out_min - (grad * sbsbf0_min)
  out <- sbsbf0 * grad + intercept
  # Apply limits
  out <- unname(pmin(pmax(out_min, out), out_max))
  #out <- unname(pmin(pmax(sbsbf0*((1-out_min)/(sbsbf0_max-sbsbf0_min))-((1-out_min)/(sbsbf0_max-sbsbf0_min)*sbsbf0_max-1), out_min), out_max))
  return(out)
}

#' hcr_constant
#'
#' An HCR function that just returns the same output irrespective of the SBSBF0 input
#'
#' @rdname hcr_funcs
hcr_constant <- function(sbsbf0, params){
  # A constant output HCR
  if (length(params) > 1){
    stop("Too many parameters in the HCR parameters. There should be only one\n")
  }
  return(rep(unname(params[1]), length(sbsbf0)))
}


#' hcr_threshold_constrained
#'
#' Like the typical HCR function with the parameters but with a constraint on how much the output is allowed to change from a reference output.
#' Parameters are 'sbsbf0_min', 'sbsbf0_max', 'out_min', 'out_max', 'max_change_up', 'max_change_down'.
#' There is an additional argument 'reference_out' that is the reference output, i.e. the value to which to the new output is compared.
#'
#' @param reference_out The reference level that the constraint is applied to.
#'
#' @rdname hcr_funcs
#' @example
#'
#' Without constraint
#'msectrl <- MFCLMSEControl()
#'hcr(msectrl) <- "hcr_threshold"
#'hcr_params(msectrl) <- c(sbsbf0_min=0.1, sbsbf0_max=0.5, out_min=0.0, out_max=1.2)
#'eval_hcr(msectrl, sbsbf0=0.3) # 0.5
#'
#' With a constraint of 15% each way
#'msectrl2 <- MFCLMSEControl()
#'hcr(msectrl2) <- "hcr_threshold_constrained"
#'hcr_params(msectrl2) <- c(sbsbf0_min=0.1, sbsbf0_max=0.5, out_min=0.0, out_max=1.2, max_change_up=1.15, max_change_down=0.85)
#'eval_hcr(msectrl2, sbsbf0=0.3, reference_out=1.0)
#'# So that next out cannot change by more 15%
#'eval_hcr(msectrl2, sbsbf0=0.3, reference_out=0.1)
hcr_threshold_constrained <- function(sbsbf0, params, reference_out){
  # The standard threshold type of HCR
  if (!all(names(params) %in% c('sbsbf0_min', 'sbsbf0_max', 'out_min', 'out_max', 'max_change_up', 'max_change_down'))){
    stop("HCR parameter names do not match those in the HCR function\n")
  }
  sbsbf0_min <- params['sbsbf0_min']
  sbsbf0_max <- params['sbsbf0_max']
  out_min <- params['out_min']
  out_max <- params['out_max']
  out <- hcr_threshold(sbsbf0, params[c('sbsbf0_min', 'sbsbf0_max', 'out_min', 'out_max')])
  # Apply constraint
  out <- pmax(pmin(out, reference_out * params['max_change_up']), reference_out * params['max_change_down'])
  return(out)
}


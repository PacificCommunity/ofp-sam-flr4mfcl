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
  if (!all(c('sbsbf0_min', 'sbsbf0_max', 'out_min', 'out_max') %in% names(params))){
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


#' hcr_asymptotic
#'
#' Instead of a threshold shape this HCR has a lovely curvy shape based on an asymptotic function.
#'
#' @param sbsbf0 Numeric vector of SBSBF0.
#' @param params Numeric vector of HCR parameters.
#'
#' @rdname hcr_funcs
#' @return Numberic vector of the HCR output
hcr_asymptotic <- function(sbsbf0, params){
  # The standard threshold type of HCR
  if (!all(c('sbsbf0_min', 'sbsbf0_max', 'out_min', 'out_max', 'curve') %in% names(params))){
    stop("HCR parameter names do not match those in the HCR function\n")
  }
  sbsbf0_min <- params['sbsbf0_min']
  sbsbf0_max <- params['sbsbf0_max']
  out_min <- params['out_min']
  out_max <- params['out_max']
  curve <- params['curve']

  # y = a - (a-b) * exp(-c * x)
  # a is maximum attainable y (sbsbf0_max)
  # b is y at x = 0 (sbsbf0_min)
  # c is the curve
  # The following finds a and b for a given curve parameter that passes through
  # (out_min, sbsbf0_min) and (out_max, sbsbsf0_max)
  # Could probably be neater but...

  z1 <- exp(-curve * sbsbf0_min)
  z2 <- exp(-curve * sbsbf0_max)
  p <- (out_max / z2) - (out_min / z1)
  q <- (1 - z2) / z2
  r <- (1 - z1) / z1
  a <- p / (q-r)
  b <- (out_min - a * (1 - z1)) / z1
  out <- a - (a-b)*exp(-curve * sbsbf0)
  out <- pmax(pmin(out_max, out), out_min)
  # Apply limits
  out <- unname(pmin(pmax(out_min, out), out_max))
  return(out)
}

# Test
# msectrl <- MFCLMSEControl()
# hcr(msectrl) <- "hcr_asymptotic"
# hcr_params(msectrl) <- c(sbsbf0_min=0.1, sbsbf0_max=0.5, out_min=0.15, out_max=1.2, curve=10)
# sbsbf0 <- seq(from=0, to=1, length=101)
# eval_hcr(msectrl, sbsbf0=sbsbf0)
# plot(x=sbsbf0, y=eval_hcr(msectrl, sbsbf0=sbsbf0), typ="l", ylim=c(0,1.5), xlim=c(0,1))

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


#' hcr_hillary_step
#'
#' Hillary step is basically two thresholds on top of each other, side by side
#' Parameters are 'sbsbf0_min', 'sbsbf0_max', 'sbsbf0_step_min', 'sbsbf0_step_max','out_min', 'out_max', 'step_height'.
#' 
#'
#' @param reference_out The reference level that the constraint is applied to.
#'
#' @rdname hcr_funcs

# Add in extra HCR function - actually in FLR4MFCL but needs to be reinstalled into Condor R
hcr_hillary_step <- function(sbsbf0, params){
  if (!all(c('sbsbf0_min', 'sbsbf0_max', 'sbsbf0_step_min', 'sbsbf0_step_max','out_min', 'out_max', 'step_height') %in% names(params))){
    stop("HCR parameter names do not match those in the HCR function\n")
  }
  # Need to unname them - kind of annoying
  sbsbf0_min <- unname(params['sbsbf0_min'])
  sbsbf0_max <- unname(params['sbsbf0_max'])
  sbsbf0_step_min <- unname(params['sbsbf0_step_min'])
  sbsbf0_step_max <- unname(params['sbsbf0_step_max'])
  out_min <- unname(params['out_min'])
  out_max <- unname(params['out_max'])
  step_height <- unname(params['step_height'])
  
  # Hillary step is basically two thresholds on top of each other, side by side
  out_left_params <- c(sbsbf0_min = sbsbf0_min, sbsbf0_max = sbsbf0_step_min, out_min = out_min, out_max = step_height)
  out_left <- hcr_threshold(sbsbf0, params=out_left_params)
  out_right_params <- c(sbsbf0_min = sbsbf0_step_max, sbsbf0_max = sbsbf0_max, out_min = step_height, out_max = out_max)
  out_right <- hcr_threshold(sbsbf0, params=out_right_params)
  # out is out_left, 
  out <- out_left
  # If we're on the right hand side threshold, use that
  out[sbsbf0 > sbsbf0_step_min] <- out_right[sbsbf0 > sbsbf0_step_min]
  return(out)
}



#' Generic constrained HCR function
#' 
#' Takes any HCR function as a character string and applies constraint if necessary
#' @param hcr_func Character string of the function name
#' @param sbsbf0 Numeric vector of SBSBF0.
#' @param params Numeric vector of HCR parameters.
#' @param reference_out The previous output value to compare against
#' @rdname hcr_funcs
#' @return Numberic vector of the HCR output
hcr_constrained <- function(sbsbf0, params, reference_out, hcr_func){
  # Check we have the change parameters
  if (!all(c('max_change_up', 'max_change_down') %in% names(params))){
    stop("Calling constrained HCR but missing 'max_change_up' and 'max_change_down' parameters\n")
  }
  out <- eval(call(hcr_func, sbsbf0=sbsbf0, params=params))
  # Apply constraint
  out <- pmax(pmin(out, reference_out * params['max_change_up']), reference_out * params['max_change_down'])
  return(out)
}
#params <- c(sbsbf0_min=0.1, sbsbf0_max=0.5, out_min=0.15, out_max=1.2, curve=10, max_change_up=1.1, max_change_down=0.9)
#hcr_asymptotic(sbsbf0=0.42, params=params)
#hcr_constrained(sbsbf0=0.42, params=params, reference_out=1.0, hcr_func="hcr_asymptotic")

# Should fail with parameter names
#params <- c(sbsbf0_min=0.1, sbsbf0_max=0.5, out_min=0.15, out_max=1.2, max_change_up=1.1, max_change_down=0.9)
#hcr_asymptotic(sbsbf0=0.42, params=params)
#hcr_constrained(sbsbf0=0.42, params=params, reference_out=1.0, hcr_func="hcr_asymptotic")

# Should fail with parameter names
#params <- c(sbsbf0_min=0.1, sbsbf0_max=0.5, out_min=0.15, out_max=1.2, curve=10, max_change_up=1.1)
#hcr_asymptotic(sbsbf0=0.42, params=params)
#hcr_constrained(sbsbf0=0.42, params=params, reference_out=1.0, hcr_func="hcr_asymptotic")

#' hcr_asymptotic_constrained
#'
#' Instead of a threshold shape this HCR has a lovely curvy shape based on an asymptotic function.
#' Also has a maximum constraint change up or down from previous value
#' @param sbsbf0 Numeric vector of SBSBF0.
#' @param params Numeric vector of HCR parameters.
#' @param reference_out The previous output value to compare against
#'
#' @rdname hcr_funcs
#' @return Numberic vector of the HCR output
hcr_asymptotic_constrained <- function(sbsbf0, params, reference_out){
  out <- hcr_constrained(sbsbf0=sbsbf0, params=params, reference_out=reference_out, hcr_func="hcr_asymptotic")
  return(out)
}

#params <- c(sbsbf0_min=0.1, sbsbf0_max=0.5, out_min=0.15, out_max=1.2, curve=10, max_change_up=1.1, max_change_down=0.9)
#hcr_asymptotic(sbsbf0=0.42, params=params)
#hcr_asymptotic_constrained(sbsbf0=0.42, params=params, reference_out=1.0)


#' hcr_threshold_constrained
#'
#' Like the typical HCR function with the parameters but with a constraint on how much the output is allowed to change from a reference output.
#' Parameters are 'sbsbf0_min', 'sbsbf0_max', 'out_min', 'out_max', 'max_change_up', 'max_change_down'.
#' There is an additional argument 'reference_out' that is the reference output, i.e. the value to which to the new output is compared.
#'
#' @param reference_out The reference level that the constraint is applied to.
#'
#' @rdname hcr_funcs
#' @examples
#' # Without constraint
#' msectrl <- MFCLMSEControl()
#' hcr(msectrl) <- "hcr_threshold"
#' hcr_params(msectrl) <- c(sbsbf0_min=0.1, sbsbf0_max=0.5, out_min=0.0, out_max=1.2)
#' eval_hcr(msectrl, sbsbf0=0.3) # 0.5
#'
#' # With a constraint of 15% each way
#' msectrl2 <- MFCLMSEControl()
#' hcr(msectrl2) <- "hcr_threshold_constrained"
#' hcr_params(msectrl2) <- c(sbsbf0_min=0.1, sbsbf0_max=0.5, out_min=0.0, out_max=1.2, max_change_up=1.15, max_change_down=0.85)
#' eval_hcr(msectrl2, sbsbf0=0.3, reference_out=1.0)
#' # So that next out cannot change by more 15%
#' eval_hcr(msectrl2, sbsbf0=0.3, reference_out=0.1)

hcr_threshold_constrained <- function(sbsbf0, params, reference_out){
  out <- hcr_constrained(sbsbf0=sbsbf0, params=params, reference_out=reference_out, hcr_func="hcr_threshold")
  return(out)
}


#' hcr_hillary_step_constrained
#'
#' Like the typical HCR function with the parameters but with a constraint on how much the output is allowed to change from a reference output.
#' Parameters are 'sbsbf0_min', 'sbsbf0_max', 'out_min', 'out_max', 'max_change_up', 'max_change_down'.
#' There is an additional argument 'reference_out' that is the reference output, i.e. the value to which to the new output is compared.
#'
#' @param reference_out The reference level that the constraint is applied to.
#'
#' @rdname hcr_funcs

hcr_hillary_step_constrained <- function(sbsbf0, params, reference_out){
  #browser()
  out <- hcr_constrained(sbsbf0 = sbsbf0, params=params, reference_out=reference_out, hcr_func="hcr_hillary_step")
}




#' hcr_asymptotic_hillary_step
#'
#' Hillary step is basically two thresholds on top of each other, side by side
#' Parameters are 'sbsbf0_min', 'sbsbf0_max', 'sbsbf0_step_min', 'sbsbf0_step_max','out_min', 'out_max', 'step_height'.
#' Adding 'curve' to allow for asymptotic out_left
#'
#' @param reference_out The reference level that the constraint is applied to.
#'
#' @rdname hcr_funcs

# Add in extra HCR function - actually in FLR4MFCL but needs to be reinstalled into Condor R
hcr_asymptotic_hillary_step <- function(sbsbf0, params){
  if (!all(c('sbsbf0_min', 'sbsbf0_max', 'sbsbf0_step_min', 'sbsbf0_step_max','out_min', 'out_max', 'step_height', 'curve') %in% names(params))){
    stop("HCR parameter names do not match those in the HCR function\n")
  }
  # Need to unname them - kind of annoying
  sbsbf0_min <- unname(params['sbsbf0_min'])
  sbsbf0_max <- unname(params['sbsbf0_max'])
  sbsbf0_step_min <- unname(params['sbsbf0_step_min'])
  sbsbf0_step_max <- unname(params['sbsbf0_step_max'])
  out_min <- unname(params['out_min'])
  out_max <- unname(params['out_max'])
  step_height <- unname(params['step_height'])
  curve       <- unname(params['curve'])
  
  # Hillary step is basically two thresholds on top of each other, side by side
  out_left_params <- c(sbsbf0_min = sbsbf0_min, sbsbf0_max = sbsbf0_step_min, out_min = out_min, out_max = step_height, curve=curve)
  out_left <- hcr_asymptotic(sbsbf0, params=out_left_params)
  out_right_params <- c(sbsbf0_min = sbsbf0_step_max, sbsbf0_max = sbsbf0_max, out_min = step_height, out_max = out_max)
  out_right <- hcr_threshold(sbsbf0, params=out_right_params)
  # out is out_left, 
  out <- out_left
  # If we're on the right hand side threshold, use that
  out[sbsbf0 > sbsbf0_step_min] <- out_right[sbsbf0 > sbsbf0_step_min]
  return(out)
}


#' hcr_hillary_step_constrained
#'
#' Like the typical HCR function with the parameters but with a constraint on how much the output is allowed to change from a reference output.
#' Parameters are 'sbsbf0_min', 'sbsbf0_max', 'out_min', 'out_max', 'max_change_up', 'max_change_down'.
#' There is an additional argument 'reference_out' that is the reference output, i.e. the value to which to the new output is compared.
#'
#' @param reference_out The reference level that the constraint is applied to.
#'
#' @rdname hcr_funcs

hcr_asymptotic_hillary_step_constrained <- function(sbsbf0, params, reference_out){
  out <- hcr_constrained(sbsbf0 = sbsbf0, params=params, reference_out=reference_out, hcr_func="hcr_asymptotic_hillary_step")
}



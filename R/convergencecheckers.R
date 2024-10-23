# Define the generic function for checking convergence
setGeneric("check_convergence", function(object) {
  standardGeneric("check_convergence")
})


########################################################
#
# Handle lmerMod objects from lme4::lmer
#
########################################################


# Method for lmerMod objects (from lme4 and lmerTest)
setMethod("check_convergence", "lmerMod", function(object) {

  # First check for singularity
  if (lme4::isSingular(object)) {
    return(0)  # Singular fit
  }

  # Check if convergence information exists
  if (is.null(unlist(object@optinfo$conv$lme4))) {
    return(1)  # Model converged
  } else {
    return(-1)  # Failed to converge
  }
})

# Method for glmmTMB objects (from glmmTMB package)

########################################################
#
# Handle glmmTMB objects from glmmTMB::glmmTMB
#
#
########################################################



setMethod("check_convergence", "glmmTMB", function(object) {
  # First check if there's a singular convergence message
  if (!is.null(object$fit$message) && grepl("singular", object$fit$message)) {
    return(0)  # Singular fit based on the message
  }

  # Now check if the model failed to converge (non-zero without singularity message)
  if (object$fit$convergence != 0) {
    return(-1)  # Failed to converge
  }

  # Fallback: Check the Hessian matrix for non-PSD status
  hessian <- object$fit$Hessian
  if (!is.null(hessian) && any(eigen(hessian, symmetric = TRUE)$values <= 0)) {
    return(0)  # Singular fit (non-PSD Hessian)
  }

  # Fallback: Check for near-zero random effect variances
  re_variances <- as.numeric(VarCorr(object)$cond)
  if (any(re_variances < 1e-10)) {  # Adjust threshold as needed
    return(0)  # Singular fit (near-zero variance)
  }

  return(1)  # Model converged successfully and is not singular
})


########################################################
#
# Handle fitted objects from nlme:lme
#
# Note: if the model does not converge, then nothing is
# returned by lme() even when erros are surpressed for
# example by tryCatch - which is markedly different
# behaviour from lme4:lmer and glmmTMB:gmllTMB
#
########################################################

library(nlme)

setMethod("check_convergence", "lme", function(object) {

  # If the model didn't converge, this wouldn't be an lme object, but we check anyway
  if (is.null(object)) {
    stop("No model object found. Likely non-convergence.")
  }

  # Check the variances of the random effects
  variances <- VarCorr(object)

  # Check for very small variances (near-zero), suggestive of singularity
  small_variances <- sapply(variances, function(v) v[1, "Variance"]) < 1e-6

  if (any(small_variances)) {
    warning("Some random effect variances are very close to zero, possibly indicating a near-singular fit.")
    return(0)  # Singular fit
  }

  # If no singularity is detected, consider it converged
  return(1)  # Converged
})

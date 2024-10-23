###########################################################
#
#  Utility functions for the check_convergence
#
###########################################################

# Function to check if a package is installed
check_package_installed <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(paste0("Package '", package, "' is not installed. Please install it ",
    "using install.packages('", package, "')."))
  }
}

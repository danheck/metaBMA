#' @importFrom Rcpp loadModule
.onLoad <- function(libname, pkgname) {
  modules <- paste0("stan_fit4", names(stanmodels), "_mod")
  for (m in modules) loadModule(m, what = TRUE)

  # packageStartupMessage("Welcome to metaBMA. ")
}


.onAttach <- function(...) {
  ver <- utils::packageVersion("metaBMA")
  packageStartupMessage("This is metaBMA version ", ver)

  packageStartupMessage("- Default priors were changed in version 0.6.6.")
  packageStartupMessage("- Since default priors may change again, it is safest to specify priors (even when using the defaults).")
}

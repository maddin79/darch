.onAttach <- function(libname, pkgname)
{  
  # To disable loading of gputools, set the below-mentioned option to false
  if (getOption("darch2.gputools", default=T) && !require("gputools", quietly=T))
  {
    # TODO more informative message
    cat("Warning: gputools package not found, you may want to install it.")
  }
}
.onAttach <- function(libname, pkgname)
{  
  if (require("gputools", quietly=T))
  {
    options(darch2.gputools=T)
  }
  else
  {
    options(darch2.gputools=F)
    cat("Warning: gputools package not found, you may want to install it.")
  }
}
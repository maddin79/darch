# Copyright (C) 2015-2016 Johannes Rueckert
#
# This file is part of darch.
#
# darch is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# darch is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with darch. If not, see <http://www.gnu.org/licenses/>.

# Utility functions

# Convert given function name to actual function or return NULL if not found
# TODO use match.fun and disregard functions not exported from other packages or
# duplicates?
characterToFunction <- function(funcName)
{
  if (is.function(funcName))
  {
    return (funcName)
  }
  
  results <- utils::getAnywhere(funcName)
  
  if (length(results$objs) == 0)
  {
    return (NULL)
  }
  
  func <- NULL
  funcsFound <- 0
  funcChosen <- NULL
  
  for (i in 1:length(results$objs))
  {
    if (is.function(results$objs[[i]]))
    {
      if (funcsFound == 0)
      {
        func <- results$objs[[i]]
        funcChosen <- i
      }
      
      funcsFound <- if (results$dups[i]) funcsFound else funcsFound + 1
    }
  }
  
  if (funcsFound > 1)
  {
    futile.logger::flog.warn(paste("%s different functions matching '%s' were",
      "found, using the one from \"%s\". Pass function directly to use a",
      "different one"), funcsFound, funcName, results$where[funcChosen])
  }
  
  func
}

# Find function in a list of function names by comparing function bodies;
# returns function name if found, NULL otherwise
functionToCharacter <- function(needle, default = "unknown function",
  package = "package:darch")
{
  needleBody <- body(needle)
  needleBodyLength <- length(needleBody)
  
  for (functionName in utils::lsf.str(package))
  {
    functionBody <- body(functionName)
    
    if (needleBodyLength == length(functionBody)
      && length(intersect(as.character(needleBody), as.character(functionBody)))
      == needleBodyLength)
    { 
      return(functionName)
    }
  }
  
  return(default)
}

# TODO solve better
getErrorFunctionName <- function(func)
{
  func(matrix(0), matrix(0))[[1]]
}

# Helper to check whether a variable is exactly FALSE
is.logical.length1 <- function(variable, logical)
{
  return(length(variable) == 1 && is.logical(variable) && variable == logical)
}

# clean deparsing on only one line and without structure() constructs for lists
deparseClean <- function(s)
{
  paste(deparse(s, control = c("keepInteger")), collapse = "")
}
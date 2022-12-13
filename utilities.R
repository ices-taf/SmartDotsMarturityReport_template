
vname <- function(name) paste(name, group, sep = "_")
vsname <- function(name) paste(name, stratum, group, sep = "_")

# This first function to estimate the Mode is created to deal with matrices were the columns are the maturity stage, and the content of the cell the number of readers that decided each maturity stage. This function Mode_I goes with the function cv_I
Mode_I <- function(x) {
  names(sort(x, decreasing = TRUE)[1])
}

# This second function to estimate the Mode is created to deal with matrices were the columns are the readers, and the content of the cell the maturity stage decided by each reader. This function Mode_II goes with the function cv_II
Mode_II <- function(x) {
  names(sort(table(x), decreasing = TRUE)[1])
}

# ape <- function(x) {
#   if (length(x) == 0) {
#     return(numeric(0))
#   }
#   if (Mode(x) == 0) {
#     NA
#   } else {
#     100 * mean(abs((x - mean(x, na.rm = TRUE)) / mean(x, na.rm = TRUE)), na.rm = TRUE)
#   }
# }

# Estimate of variance in categorical variables: Coefficient of unalikeability (see paper of Kader and Perry in https://www.tandfonline.com/doi/full/10.1080/10691898.2007.11889465?scroll=top&needAccess=true. It is estimated with the function unalike, of package Ragree.

# This first function to estimate the coefficient of unalikeability is created to deal with matrices were the columns are the maturity stage, and the content of the cell the number of readers that decided each maturity stage. This function cu_I goes with the function Mode_I, that estimates the mode for matrices equally designed (i.e. columns are the maturity stage, and the content of the cell the number of readers that decided each maturity stage, while rows are the fishID)
cu_I <- function (x) {
  if (length(x) == 0) {
    return(numeric(0))
  }
  if (Mode_I(x) == 0) {
    NA
  } else {
    dat=rep.int(names(x[which(x>0)]),times=x[which(x>0)])
    unalike(dat, na.rm = TRUE)
  }
}


# This second function to estimate the coefficient of unalikeability is created to deal with matrices were the columns are the readers, and the content of the cell the maturity stage decided by each reader. This function cu_II goes with the function Mode_II, that estimates the mode for matrices equally designed (i.e. columns are readers and content is the maturity stage decided by each reader, while rows are the fishID)
cu_II <- function (x) {
  if (length(x) == 0) {
    return(numeric(0))
  }
  if (Mode_II(x) == 0) {
    NA
  } else {
    unalike(x)
  }
}


capFirst <- function(s) {
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}


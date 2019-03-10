### INSTALL ALL LIBRARIES AND DEPENDENCIES #####

# this will only install the packages that are not already installed on your computer i.e. packages won't get updated

libs <- c("traj")

libsInstalled <- installed.packages()[, 1]
libsToInstall <- libs[!libs %in% libsInstalled]

## fix if CRANextra mirror is down and causes problems with installations
#options(repos = getOption("repos")["CRAN"])

if (length(libsToInstall) == 0) {
  stop('No packages queued for installation')
} else{
  install.packages(libsToInstall)
}  
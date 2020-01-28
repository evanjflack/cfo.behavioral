install_update_packages <- function(package_list, package_path, 
                                    update = FALSE) {
  
  # Update all packages already installed
  if (update == T) {
    update.packages(lib.loc = package_path, ask = FALSE)
  }
  
  # Install packages that are uninstalled
  installed <- installed.packages(lib.loc = package_path)[, 1]
  
  uninstalled <- unlist(package_list[!(package_list %in% installed)])
  if (length(uninstalled) >= 1) {
    installed_packages <- lapply(uninstalled, install.packages,
                                 lib = package_path)
    rm(installed_packages)
  }
}

load_packages <- function(package_list, package_path) {
  # load all packages
  loaded_packages <- suppressMessages(lapply(package_list, library,
                                             lib.loc = package_path,
                                             character.only = T,
                                             quietly = T))
}
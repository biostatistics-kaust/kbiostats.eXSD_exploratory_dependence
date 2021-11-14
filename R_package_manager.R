#
# This is a simple script to create, build and manage
# an R package
#

silent.dir.create <- function(dirname, showWarnings=FALSE) if(!file.exists(dirname)) dir.create(dirname, showWarnings=showWarnings) else dirname

create_text_file <- function(packageDir, dirpath, filename, fileContent) {
  #
  silent.dir.create(sprintf("%s/%s/", packageDir, dirpath))
  filePath <- sprintf("%s/%s/%s", packageDir, dirpath, filename)
  fp <- file(filePath, "w+")
  write(fileContent, fp)
  unlink(fp)
  close(fp)
}

# Package checking
check_package <-  function(package_name, github_repo=NULL) {
  #if (!require(package_name, character.only = TRUE)) {
  if (!suppressMessages(suppressWarnings(require(package_name, character.only = TRUE)))) {
    if(is.null(github_repo)){
      install.packages(package_name, dependencies=TRUE)
    }else{
      remotes::install_github(github_repo, dependencies=TRUE)
    }
    #library(package_name, character.only=TRUE)
    suppressMessages(suppressWarnings(library(package_name, character.only=TRUE)))
  }
}
check_minimal_packages <- function(){
  check_package("devtools")
  check_package("pkgdown", github_repo="r-lib/usethis")
  check_package("remotes")
  check_package("roxygen2")
  check_package("mathjaxr", github_repo="wviechtb/mathjaxr")
  check_package("ggplot2")
  check_package("patchwork", github_repo="thomasp85/patchwork")
  check_package("shiny", github_repo="rstudio/shiny")
  check_package("stringr")
  check_package("zip")
  check_package("pkgdown", github_repo="r-lib/pkgdown")
  check_package("rbokeh", github_repo="hafen/rbokeh")
}

create_default_package_structure <- function(packageDir, packageName) {
 defaultAuthors <- 'c(
    person("Pinto", "Marco", email="pinto.marco@live.com", role=c("aut", "cre")),
    person("Ombao", "Hernando", email="hernando.ombao@kaust.edu.sa", role=c("aut"))
  )'
  defaultProperties <- list(
    "Title"=paste(packageName, "Toolbox"),
    #"Authors@R"=list(other, myself),
    "Authors@R"=defaultAuthors,
    #"Maintainer"=defaultAuthors,
    #"LaxyData"=TRUE,
    "License"="MIT/GPL"
  )
  devtools::create(packageDir, defaultProperties)
}

create_path_for_additional_files <- function(packageDir, packageName) {
  silent.dir.create(paste0(packageDir, "/inst/"))
  silent.dir.create(paste0(packageDir, "/inst/extdata"))
  create_text_file(
    packageDir,
    dirpath="inst",
    filename="README-extdata.txt",
    fileContent=sprintf("
External data should be in inst/extdata
and it can be accessed with instructions like
system.file(\"extdata\", \"mydataset.csv\", package=\"%s\")
  ", packageName
))
}

create_sample_data <- function(packageDir, packageName) {
  #https://r-pkgs.org/data.html
  #https://www.davekleinschmidt.com/r-packages/
  silent.dir.create(paste0(packageDir, "/data/"))
  #
  create_text_file(
    packageDir,
    dirpath="inst",
    filename="README-data.txt",
    fileContent=sprintf("
Exported data should be in data/
Remember that R saves the name as well:

sampleDataPath <- sprintf(\"%s/data/sample_data.Rdata\", packageDir)
sample_data <- data.frame(
  var1=c(9,10),
  var2=c(111,122)
)
save(sample_data, file=sampleDataPath)

To document the file, add an entry in /R/data.R
  ", packageName
))
  #
  create_text_file(
    packageDir,
    dirpath="R",
    filename="data.R",
    fileContent=sprintf("#' Sample data
#'
#' This is data from the first experiment ever to try XYZ using Mechanical
#' Turk workers.
#'
#' @format A data frame with NNNN rows and NN variables:
#' \\describe{
#'   \\item{subject}{Anonymized Mechanical Turk Worker ID}
#'   \\item{trial}{Trial number, from 1..NNN}
#'   ...
#' }
\"sample_data\"
  ", packageName
))
  #
  sampleDataPath <- sprintf("%s/data/sample_data.Rdata", packageDir)
  sample_data <- data.frame(
    var1=c(9,10),
    var2=c(111,122)
  )
  save(sample_data, file=sampleDataPath)
}


create_sample_script <- function(packageDir) {
  sampleFunctionPath <- paste0(packageDir, "/R/cat_function.R")
  sampleFunction <- "
#' A Cat Function 
#' 
#' This function allows you to express your love of cats. 
#' @param love Do you love cats? Defaults to TRUE. 
#' @keywords cats 
#' @export 
#' @examples 
#' cat_function()  
cat_function <- function(love=TRUE) {
  if(love==TRUE) {
    print('I love cats!')
  } else {
    print('I am not a cool person.')
  }
}
  "
  fp <- file(sampleFunctionPath, "w+")
  write(sampleFunction, fp)
  unlink(fp)
  close(fp)
}

# https://www.rdocumentation.org/search?q=write_over

#' Use pkgdown
#' Modified from usethis::use_pkgdown
#'
#' @description
#' [pkgdown](https://pkgdown.r-lib.org) makes it easy to turn your package into
#' a beautiful website. usethis provides two functions help you use pkgdown:
#'
#' * `use_pkgdown()`: creates a pkgdown config file, adds relevant files or
#'   directories to `.Rbuildignore` and `.gitignore`, and builds favicons if
#'   your package has a logo.
#'
#' * `use_pkgdown_github_pages()`: implements the GitHub setup needed to
#'   automatically publish your pkgdown site to GitHub pages:
#'
#'   - [use_github_pages()] prepares to publish the pkgdown site from the
#'     `github-pages` branch
#'   - [`use_github_action("pkgdown")`][use_github_action()] configures a
#'     GitHub Action to automatically build the pkgdown site and deploy it via
#'     GitHub Pages
#'   - The pkgdown site's URL is added to the pkgdown configuration file,
#'     to the URL field of DESCRIPTION, and to the GitHub repo.
#'
#' `use_pkgdown_travis()` is deprecated; we no longer recommend that you use
#' Travis-CI.
#'
#' @seealso <https://pkgdown.r-lib.org/articles/pkgdown.html#configuration>
#' @param config_file Path to the pkgdown yaml config file
#' @param destdir Target directory for pkgdown docs
use_pkgdown <- function(config_file = "_pkgdown.yml", destdir = "docs") {
  has_logo <- function() {
    fs::file_exists(proj_path("man", "figures", "logo.png")) ||
      fs::file_exists(proj_path("man", "figures", "logo.svg"))
  }
  #
  usethis::use_build_ignore(c(config_file, destdir))
  usethis::use_build_ignore("pkgdown")
  usethis::use_git_ignore(destdir)

  if (has_logo()) {
    #build_favicons(overwrite=TRUE)
    #build_favicons(overwrite=FALSE)
  }

  config <- usethis::proj_path(config_file)
  if (!base::identical(destdir, "docs")) {
    usethis::write_over(config, paste("destination:", destdir))
  }
  usethis::edit_file(config)

  base::invisible(TRUE)
}

build_package <- function(){
  use_pkgdown(config_file = "_pkgdown.yml", destdir = "docs")
  pkgdown::clean_site()
  ####devtools::document(roclets = c('rd', 'collate', 'namespace'), quiet=TRUE)
  suppressMessages(suppressWarnings(devtools::document(roclets = c('rd', 'collate', 'namespace'), quiet=TRUE)))
  pkgdown::build_site(pkg=".", examples=FALSE, devel=TRUE, lazy=TRUE)
  ####devtools::build(quiet=TRUE)
  #devtools::install(quiet=TRUE, upgrade="never")
  ####devtools::install(quiet=TRUE, upgrade="never", dependencies=FALSE)
  suppressMessages(suppressWarnings(devtools::build(quiet=TRUE)))
  suppressMessages(suppressWarnings(devtools::install(quiet=TRUE, upgrade="never", dependencies=FALSE)))
}

# By default, ALL files in an R package
# should be in the R directory. However,
# that's HORRIBLY difficult to maintain (for me).
# So, this function reads an structured R folder
# (R-tree by default), rename all files and copy
# them in the R dir
unfold_R_subpackages <- function(packageDir, subpackagesFolder="scripts") {
  commonRPath <- file.path(paste0(packageDir, "/R"))
  subpackagesPath <- file.path(paste0(packageDir, "/", subpackagesFolder))
  if(file.exists(subpackagesPath)){
    print(sprintf("-- Unfolding subpackage directory: %s", subpackagesPath))
    filenames <- list.files(subpackagesPath, pattern="\\.R$", recursive=TRUE)
    zip_filename <- format(Sys.time(), "Rbackup.%d.%m.%Y.%H.%M.zip")
    zip(zip_filename, c("R"))
    for(filename in filenames){
      unfolded_filename = str_replace_all(filename, "/", "_")
      folded_path <- paste0(subpackagesPath, "/", filename)
      unfolded_path <- paste0(commonRPath, "/", unfolded_filename)
      file.copy(from=folded_path, to=unfolded_path, overwrite=TRUE)
    }
  } else {
    print(sprintf("-- NO subpackage directory: %s", subpackagesPath))
  }
}


manage_package <- function(packageName){
  startDirectory <- getwd()
  check_minimal_packages()
  packageDir <- file.path(paste0(startDirectory, "/", packageName))
  print(sprintf("-- Current directory: %s", startDirectory))
  print(sprintf("-- R package directory: %s", packageDir))
  if(!file.exists(packageDir)){
    print("-- Creating default structure")
    create_default_package_structure(packageDir, packageName)
    create_sample_script(packageDir)
    create_sample_data(packageDir, packageName)
    create_path_for_additional_files(packageDir, packageName)
    print(sprintf("-- Note: Add package info & dependencies into %s/DESCRIPTION", packageName))
    print(sprintf("-- Note: %s/NAMESPACE is automatically updated. NO need to change.", packageName))
  }
  tryCatch({
    setwd(packageDir)
    unfold_R_subpackages(packageDir)
    build_package()
  }, finally = {
    setwd(startDirectory)
  })
}


manage_package(packageName="eXSD")


# Additional resources for handling R packages
# hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
# https://www.rdocumentation.org/packages/devtools/versions/1.13.6/topics/create
# https://r-pkgs.org/man.html

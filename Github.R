library(usethis)
library(tidyverse)
library(lme4)
library(rmarkdown)


#== Set-up ==#
#configure your name and email associated with your GitHub account
use_git_config(
  user.name = "sakurakomiyama", 
  user.email = "sakura.komiyama@uib.no"
)

#== Connect RStudio and GitHub ==#
usethis::create_github_token()
gitcreds::gitcreds_set() #copy token from Github page
git_vaccinate() #add various files to your global .gitignore file to reduce the chance of you leaking passwords, making git safer to use

#== Making a repo ==#
usethis::use_git()
git_default_branch_rename()
git_default_branch_configure(name = "main")
use_github()

#create_from_github("sakurakomiyama/BIO302") #To clone repository (No need to do when you create new project)

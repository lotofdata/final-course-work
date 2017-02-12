
install.packages("Rserve")
library(Rserve)
run.Rserve()

install.packages("devtools")
library(devtools)


install_github('sentiment140', 'okugami79')
library(sentiment)

sentiment('I LOVE #Apple')
sentiment('I hate #Apple')

git config --global user.email "s1mpass@gmail.com"
git config --global user.name "lotofdata"


git remote add origin https://github.com/lotofdata/TM.git

git config remote.origin.url git@github.com:lotofdata/TM.git

git pull -u origin master
git push -u origin master
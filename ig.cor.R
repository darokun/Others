install.packages("instaR")
library(instaR)

setwd("/Users/daro/Documents/GitHub/Others")

app_id <- "efb67c70fda34195b648ea5e951d4155"
secret <- "88bbf28c63ca44acbb15b91a085a20f4"

my_oauth <- instaOAuth(app_id = app_id, app_secret = secret)
save(my_oauth, file="my_oauth")

load("my_oauth")
dk <- getFollowers(username = "darokun", token = my_oauth) #error

remove.packages("instaR") #remove CRAN package

#---
# Trying from a forked repo on GitHub

devtools::install_github("darokun/instaR/instaR")
library(instaR)

setwd("/Users/daro/Documents/GitHub/Others")

app_id <- "efb67c70fda34195b648ea5e951d4155"
secret <- "88bbf28c63ca44acbb15b91a085a20f4"

my_oauth <- instaOAuth(app_id = app_id, app_secret = secret)
save(my_oauth, file="my_oauth")

load("my_oauth")
dk <- getFollowers(username = "darokun", token = my_oauth) # still error

#---
# from: https://bigdataenthusiast.wordpress.com/2016/03/22/exploring-instagram-api-using-r/

my_oauth
my.access.token <- my_oauth$credentials$access_token
my.access.token

ownerInfoEndPoint <- paste("https://api.instagram.com/v1/users/self/?access_token=", my.access.token, sep = "")
require(RCurl)
data <- getURL(ownerInfoEndPoint)
json <- fromJSON(data)
json

n.posts <- json$data$counts$media
n.followers <- json$data$counts$followed_by
n.posts
n.followers

# however, this method doesn't work because it's meant to extract information only on the self-account.









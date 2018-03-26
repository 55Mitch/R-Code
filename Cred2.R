###Loading my twitter credentials
api_key <- "0XkQ9vgX4pKUhtj1Ej4uO59f3"
api_secret <- "Az0Br6rXLIsKsQD96McEe9Jl1NhP9KFR3Tj9TyXXMb0MDnhR5g"
access_token <- "3980633294-FvqkFMRneOuP0FnuSXrdcVPJ1IWCy2Rdetjk6ay"
access_token_secret <- "MCQhS8MuMBK5VRAKih6FjWFlpnZQ39Z1IgHeXz7NNkCJh"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

### fetching tweets ###
download.file(url = "http://curl.haxx.se/ca/cacert.pem",
              destfile = "cacert.pem")
###### Load Plotly Credentials
Sys.setenv("plotly_username"="55mitch")
Sys.setenv("plotly_api_key"="ktOOlYRAf2sxMF3nahed")
###Loading my twitter credentials
api_key <- "NUS7NG4UW8jgUPKyzo2zfGjZj"
api_secret <- "etmI4Pd3QAHfTX6YalYAqyakCtlPIfXztlz8yuMjkErWpBlkCQ"

create_token(app = "mytwitterapp", api_key, api_secret,set_renv = TRUE)
### fetching tweets ###
download.file(url = "http://curl.haxx.se/ca/cacert.pem",
              destfile = "cacert.pem")
###### Load Plotly Credentials
Sys.setenv("plotly_username"="55mitch")
Sys.setenv("plotly_api_key"="ktOOlYRAf2sxMF3nahed")
# AWS functions


write.to.aws <- function(bucket, location, file.name, object){
  require(aws.s3)
  Sys.setenv("AWS_ACCESS_KEY_ID" = "xxxxx",
             "AWS_SECRET_ACCESS_KEY" = "xxxxxxx",
             "AWS_DEFAULT_REGION" = "us-east-1")
  temp.file <- paste0(location, file.name)
  # temporary connection
  tmp <- rawConnection(raw(0), "r+")
  # writes obj "test" to temporary connection "tmp"
  write.csv(object, tmp, row.names=F)
   aws.s3::put_object(
    file = rawConnectionValue(tmp), 
    bucket = bucket, 
    object = temp.file)
}


read.fun <- function(x, bucket ...){
  read.csv(text = rawToChar(get_object(object = x, bucket = bucket)), stringsAsFactors=FALSE)
}

read.from.aws <- function(bucket, location){
  require(aws.s3)
  Sys.setenv("AWS_ACCESS_KEY_ID" = "xxxxxxxx",
             "AWS_SECRET_ACCESS_KEY" = "xxxxxx",
             "AWS_DEFAULT_REGION" = "us-east-1")
  cat("\ngetting bucket contents\n")
  cont <- get_bucket_df(bucket, max= Inf)
  getfiles <- cont[grep(location, cont[,1]), ]
  getfiles <- getfiles[getfiles$Size > 1, 1]
  cat("\nloading csvs\n")
  cat("\n", gsub(".*\\/(.*)\\..*", "\\1", getfiles), "\n")
  list2env(
    lapply(setNames(getfiles, make.names(gsub(".*\\/(.*)\\..*", "\\1", getfiles))), 
           read.fun), envir = .GlobalEnv)
}

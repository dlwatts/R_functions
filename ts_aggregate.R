
# 'x' must include datetime (POSIXct object & numeric data)

cut_5min_aggregate <- function(x, data.col){

x$cut5min <- xts::align.time(x$datetime, 5 * 60)

	x1 <- aggregate(
					x[,data.col],
					by = list(x$cut5min),
					FUN = sum,
					na.rm = T)
	col.names <- c("datetime", names(x1)[2:length(names(x1))])
	names(x1) <- col.names
	return(x1)
}

cut_1hour_aggregate <- function(x, data.col){

x$cut1hour <- xts::align.time(x$datetime, 60 * 60)

	x1 <- aggregate(
					x[,data.col],
					by = list(x$cut1hour),
					FUN = sum,
					na.rm = T)
	col.names <- c("datetime", names(x1)[2:length(names(x1))])
	names(x1) <- col.names
	return(x1)
}

cut_24hour_aggregate <- function(x, data.col){

x$cut24hour <- xts::align.time(x$datetime, 24 * 60 * 60)

	x1 <- aggregate(
					x[,data.col],
					by = list(x$cut24hour),
					FUN = sum,
					na.rm = T)
	col.names <- c("datetime", names(x1)[2:length(names(x1))])
	names(x1) <- col.names
	return(x1)
}

# align.time.down = function(x,n) {
#     index(x) = index(x) - n
#     align.time(x,n)
# }



#cut_custom_aggregate function
cut_custom_aggregate <- function(x, data.col, cut.list){
  col.names <- c("datetime", names(x)[data.col])
  first <- max(min(x$datetime), min(cut.list))
  last <- min(max(x$datetime), max(cut.list))
  x <- x[x$datetime >= first & x$datetime <= last,]
  cut.list <- cut.list[cut.list >= first & cut.list <= last]
  x$cut <- cut(x[,"datetime"], breaks=cut.list)
  
  x1 <- aggregate(
    x[,data.col],
    by = list(x$cut),
    FUN = mean,
    na.rm = T)
  names(x1) <- col.names
  x1$datetime <- as.POSIXct(x1$datetime, tz = attr(cut.list, "tzone"))
  time.mean <- 1.1 * mean(diff(x1$datetime), na.rm=T)
  units(time.mean) <- "mins"
  diff.time <- diff(x1$datetime)
  units(diff.time) <- "mins"
  diff.time <- c(0, diff.time)
  x1 <- x1[-(which(diff.time > time.mean)),]
  return(x1)   
}
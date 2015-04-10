decondata <- function(data){
	class(data) <- "decon"
	data
}
searchdata <- function(data){
	class(data) <- "search"
	data
}
fitdata <- function(data){
	class(data) <- "fit"
	data
}

#' @export
qqplt_list<-function(a,data,list){
  for (x in seq_along(list)){
    RVAideMemoire::byf.qqnorm(formula=a~list[[x]],data=data)
  }}

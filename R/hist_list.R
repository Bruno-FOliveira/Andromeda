#'@export
hist_list<-function(a,data,list){
  for (x in seq_along(list)){
    RVAideMemoire::byf.hist(formula=a~list[[x]],data=data,sep = T, density=F)
  }}

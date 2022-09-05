#' @export
kt_list<-function(a,data,list){
  c<-for (x in seq_along(list)){
    kt(formula=a~list[[x]],data=data)
    print(as.list(c))
    print(names(list))
  }}

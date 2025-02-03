# matrix = matrix(c(
#   1,2,3,4,
#   1,1,1,1,
#   3,2,1,0
# ),nrow=3,byrow=TRUE)
# x = 1:20
# test = .filter(matrix,x)
# test[[4]] == sum(matrix[1,]*1:4)
# test[[5]] == sum(matrix[2,]*2:5)
# test[[6]] == sum(matrix[3,]*3:6) this doesn;t work
# .filter = function(matrix, x, pad=NA) {
#   which = 1:nrow(matrix)
#   # reverse the filter row wise
#   matrix = matrix[,ncol(matrix):1]
#   tmp = sapply(seq_along(utils::head(x,-ncol(matrix)+1)), function(i) matrix %*% x[i:(i+ncol(matrix)-1)])
#   index=rep(1:nrow(matrix),length.out=ncol(tmp))
#   out = sapply(seq_along(index),FUN=function(i) tmp[index[[i]],i])
#   return(c(rep(pad,ncol(matrix)-1),out))
# }

## Put comments here that give an overall description of what your
## functions do

## 缓存读写方法

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {#创建矩阵
        x <<- y
        i <<- NULL
    }
    get <- function() x#获取矩阵
    setinv <- function(inv) #创建缓存
        i <<- inv
    getinv <- function() i#获取缓存
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## 读写缓存

cacheSolve <- function(x, ...) {
    i<-x$getinv() #首先把getinv赋值给i，如果i不是NULL的话弹出下列提示
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    #如果代码执行到这里，说明之前的if语句被跳过，也就说明i为空
    data<-x$get()#把最开始的matrix()向量赋值给data
    #判断矩阵是否可逆
    if(det(data)==0)
        message("data is unable to solve")#不可逆时给出提示
    else{
        i <- solve(data)
        x$setinv(i)#可逆时缓存逆矩阵 
    }
    i#读取逆矩阵
}

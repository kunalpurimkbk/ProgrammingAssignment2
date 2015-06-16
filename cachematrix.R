## Caching the Inverse of the Matrix:
## ----------------------------------

## Author: Kunal Puri
## Date: June 16,2015

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a
## matrix rather than compute it repeatedly

## Here, it is shown how it can be implemeted in R.

## The file consists of 2 functions:
## 1) makeCacheMatrix:

## This function returns a list that has functions
## 1. set: sets matrix whose inverse is required to be evaluated
## 2. get: returns matrix whose inverse is required to be evaluated
## 3. setInv : sets the value of matrix that stores inverse
## 4. getInv : gets the special matrix that stores inverse

## 2) cacheSolve:

## This function saves a lot of computation by storing the previously computed inverse of the matrix.
## It returns the matrix that is inverse of matrix x


## Code: 
## -----


## The following function creates a special "matrix" object that can cache its inverse.

## Pre-Condition: x must be of matrix type and its values are either numeric or integer
## Post-Condition: Returned List must be stored so that it can be used further for cacheSolve function
 
makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL			##inv will store the inverse of the matrix passed through argument

	set <- function (y)	##assigning values to functions
	{
		x <<- y
		inv <<-NULL
	}
	get <- function() x
	setInv <- function(y) inv <<- y
	getInv <- function() inv

	list(get=get,set=set,getInv=getInv,setInv=setInv)	##return the list of functions
}


## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then the
## cacheSolve should retrieve the inverse from the cache.

## Pre-condition: x is a list as returned by the makeCacheMatrix function
## Post-condition: None

cacheSolve <- function(x, ...) {

	inv <- x$getInv()
	if (is.null(inv))	#if inv not computed
	{
		mat <- x$get()	#compute
		inv <- solve(mat,...)
		x$setInv(inv)
	}
	else
		message('Cached Inverse: ')

	inv		## Return a matrix that is the inverse of 'x'
}

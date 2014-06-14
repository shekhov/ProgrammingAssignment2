## Hey you! Welcome to my code, feel free to read it fully and evaluate :)
## My name is Anton and I am from Russia. Right now I am getting PhD in 
## Max Plank Institute for Chemical Ecology. I am a chemist, and my passion 
## is programming. Recently I decided to merge my work and hobby, and because 
## R is the most usable language in science, I decided to learn it. Well, enough
## about me.. Explore the code! ))
##
## To make sure that everything is working you probably need to create 
## a matrix (oh, really?!) 
##
## Then pass this matrix to the makeCacheMatix function, which will 
## create something like cash table with 4 method inside. At this point
## there is no reverse version inside until you will specify to do it.
## Actually I don't understand why we can not specify it already this 
## constructor... It will be much easy and more convenient. But this is not 
## our concern 


## Created cashed table with matrix as a main object. Has several function
## to work with it outside. Is it looked like OOP paradigm? So, yes, 
## we construct class Matrix and will have instances of it.
makeCacheMatrix <- function(x = matrix()) {
    inversed_matrix <- NULL # here I would already created inversed matrix
    # something like this:
    # inverse_x <- function () inversed_matrix <<- solve(x)
    # inverse_x()
    set <- function (y) {
        x <<- y
        inversed_matrix <<- NULL # And here another call
        # inverse_x()
    }
    
    get <- function () x
    
    set_inverse <- function (new_matrix) {
        inversed_matrix <<- new_matrix
    }
    
    get_inverse <- function () inversed_matrix
    
    # Compare to OOP it is like setting which methods are public, and which are 
    # private
    list (set = set, get = get, 
          set_inverse = set_inverse, get_inverse = get_inverse)
}


## Hmmm.. This function accept at least one argument x, which is an instance of 
## class Matrix (let me explain like this, it is easy for me). But if to speak
## in R language, it accept function makeCacheMatrix and try to set 
## inversed matrix to the origianl matrix. If it was already done, then return it 
## back. Oh.., to many words, no? What, short description? Have not idea what 
## are you talking about. Stop, I am talking with my new friend..
## Sorry, we were interrapted. Where we were? Oh yeh, here the function :)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Oh, here the short description! )) Why did they ask us to repeat it?
    
    # We need it to make sure that is cashed function already exists or not     
    inversed_x <- x$get_inverse() 
    if (!is.null(inversed_x)) {
        message ("Matrix alredy cashed. Return reverse version")
        return (inversed_x)
    }
    
    # if it has no stil, create one.
    original_matrix <- x$get()
    inversed_x <- solve (original_matrix)
    x$set_inverse (inversed_x)
    
    message ("Cashing matrix...\nDone! Return reverse version")
    return (inversed_x)
}

## P.S. I usually do not do so many comments, but I decided to have a little 
## bit of fun :) 

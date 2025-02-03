################################################################################
# Week 2 HW R CODE
# Danny Molyneux
# MATH 240 - SPRING 2025
################################################################################

################################################################################
# NOTES
################################################################################
#################################
# Function to integrate
#################################
integrand <- function(x){
  f <- 7 - 2 * x^2
  return(f)
}
#################################
# Testing Values
#################################
a <- 0
b <- 2
n.rect <- 100
(delta.x <- (b-a)/n.rect)

#################################
# Left Rule
#################################
left.points <- a + 0:99*(delta.x)
(left.area <- sum(delta.x*(integrand(left.points))))
#################################
# Right Rule
#################################
right.points <- a + 1:100*(delta.x)
(right.area <- sum(delta.x*(integrand(right.points))))
#################################
# Midpoint Rule
#################################
mid.points <- (left.points+right.points)/2
(mid.area <- sum(delta.x*(integrand(mid.points))))
################################################################################
# Question 1(a) Trapezoidial Rule
################################################################################
trap.points <- a + 0:100*(delta.x)
vector.areas <- delta.x*(integrand(trap.points)). #vector that the integrand function returns
(trap.area <- (1/2)*vector.areas[1] + sum(vector.areas[2:100]) + (1/2)*vector.areas[101])
################################################################################
# Question 1(b) Skeleton
################################################################################
riemann.sums <- function(fnct,                        # function to integrate
                         a,                           # lower bound of integral
                         b,                           # upper bound of integral
                         n.rect,                      # number of  bound of integral
                         method = "Trapezoidial"){    # method to use (trap by default)
  ######################################
  # Check Input
  ######################################
  if(!is.numeric(a)){ # if a is not numeric
    stop("The lower bound of the integral (a) must be numeric.")
  }
  if(!is.numeric(b)){ # if b is not numeric
    stop("The lower bound of the integral (a) must be numeric.")
  }
  if(!(is.numeric(n.rect)) | (n.rect%%1!=0)){ # if n.rect is not a whole number
    stop("The number of rectangles must be a positive whole number.")
  }
  ######################################
  # Compute Area
  ######################################
  (delta.x <- (b-a)/n.rect)
  if(method == "Left"){
    left.points <- a + 0:99*(delta.x)
    (area <- sum(delta.x*(integrand(left.points))))
  }else if(method == "Right"){
    right.points <- a + 1:100*(delta.x)
    (area <- sum(delta.x*(integrand(right.points))))
  }else if(method == "Midpoint"){
    mid.points <- (left.points+right.points)/2
    (area <- sum(delta.x*(integrand(mid.points))))
  }else if(method == "Trapezoidial"){
    trap.points <- a + 0:100*(delta.x)
    vector.areas <- delta.x*(integrand(trap.points))
    (area <- (1/2)*vector.areas[1] + sum(vector.areas[2:100]) + (1/2)*vector.areas[101])
  }else{
    stop("Please select a valid method (e.g., 'Left', 'Right', 'Midpoint', 'Trapezoidial')")
  }
  ######################################
  # Return the area
  ######################################
  return(area)
}
######################################
# Test the function
######################################
riemann.sums(fnct = integrand,
             a = 0,
             b = 2,
             n.rect = 100)
######################################
# Compare to numerical integral
######################################
integrate(f = integrand, # integrate() is an R function
          lower = 0,     # that completes numerical
          upper = 2)     # integration

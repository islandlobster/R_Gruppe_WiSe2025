# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser

get_corners <- function(x) {
  n <- length(x$sides)
  if (n < 3) return(invisible(NULL))
  out <- matrix(ncol = 2, nrow = length(x$sides))
  direction <- c(1, 0)
  point <- c(0, 0)
  out[1, ] <- point
  for (i in 1:(n-1)) {
    point <- point + direction * x$sides[i]
    out[i+1, ] <- point
    a <- pi - x$angles[i]
    direction <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), ncol=2) %*% direction
  }
  return(out)
}

plot_polygon <- function(x) {
  corners <- get_corners(x)
  plot(range(corners[,1]), range(corners[,2]), type = 'n', axes=FALSE, ann=FALSE, asp=1)
  polygon(corners[,1], corners[,2], lwd = 3)
}

#DONE 1: overload plot
plot.Polygon <- function(x) plot_polygon(x)

#DONE 2.1: create constructor for Rectangle
rectangle <- function(w=NA, h=NA){
    if(anyNA(as.numeric(c(w, h)))) return(invisible(NULL))
    return(return(structure(list(sides = rep(c(w, h), 2), angles = rep(pi/2, 4)), class = c("Rectangle", "Quadrilateral", "Polygon"))))
}
#DONE 2.2: create constructor for Triangle
triangle <- function(s, a=NA){
    if(anyNA(as.numeric(s)) || (!is.na(c(a)) && length(s)<2) || (is.na(c(a)) && length(s)<3)) return(invisible(NULL))
    sides <- rep(NA, 3)
    angles <- rep(NA, 3)
    sides[1] <- s[1]
    sides[2] <- s[2]
    if(is.na(c(a))){
        sides[3] <- s[3]
        angles[1] <- acos((sides[3]^2-sides[1]^2-sides[2]^2)/(2*sides[1]*sides[2]))
        angles[2] <- asin(sides[1]*sin(angles[1])/sides[3])
        angles[3] <- pi-angles[1]-angles[2]
    } else{
        angles[1] <- a
        sides[3] <- sqrt(sides[1]^2+sides[2]^2+(2*sides[1]*sides[2]*cos(angles[1])))
        angles[2] <- asin(sides[1]*sin(angles[1])/sides[3])
        angles[3] <- pi-angles[1]-angles[2]
    }
    return(structure(list(sides = sides, angles = angles), class = c("Triangle", "Polygon")))
}

#DONE 3: create validate_triangle function
validate_triangle <- function(x){
    if(!is(x, c("Polygon"))){
        if(!is(x, c("Triangle"))){
            stop("Not a Polygon")
        }else{
            stop("Since when are triangles not polygons?")
        }
    }
    if(!is(x, c("Triangle"))) stop("Not a Triangle")
    if(length(x$sides)!=3) stop("Number of sides is not 3")
    if(length(x$angles)!=3) stop("Number of angles is not 3")
    temp <- c(x$sides[1]/sin(x$angles[2]), x$sides[2]/sin(x$angles[3]), x$sides[3]/sin(x$angles[1]))
    if((temp[1]!=temp[2]) || (temp[2]!=temp[3])) stop("Sides and angles do not fit")
}

#DONE 4: create circumf function
circumf <- function(x) UseMethod("circumf")
circumf.default <- function(x) stop("function not defined")
circumf.Polygon <- function(x) return(sum(x$sides))

#DONE 5: create area function for triangle and quadrilateral
area <- function(x) UseMethod("area")
area.default <- function(x) stop("function not defined")
area.Triangle <- function(x) return((x$sides[1]*x$sides[2]*sin(x$angles[1]))/2)
area.Quadrilateral <- function(x){
    if(any(x$angles[c(2,4)]>=pi)){
        one <- (x$sides[1]*x$sides[2]*sin(x$angles[1]))/2
        two <- (x$sides[3]*x$sides[4]*sin(x$angles[3]))/2
    } else{
        one <- (x$sides[2]*x$sides[3]*sin(x$angles[2]))/2
        two <- (x$sides[4]*x$sides[1]*sin(x$angles[4]))/2
    }
    return(sum(c(one, two)))
}
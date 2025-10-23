# Paul Proft, Lionel Assick

#1)
x <- c(7, -3, -7.2, sin(-2.5), 1/3 )
x #print for subresult

#2)
rev_x <- rev(x)
rev_x #print for subresult

#3)
sum_unev <- sum(x[(seq_along(x) %% 2) == 1])
sum_unev #print for subresult

#4)
y <- -10:10
y #print for subresult

#5)
y[sin(y)<0]<-0
y #print for subresult

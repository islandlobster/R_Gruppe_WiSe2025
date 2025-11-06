# Paul Proft, Lionel Assick, Gina Lilienkamp, Emma Roser 

x <- 7:3
y = seq(15,55, by = 10)
x
y


(y - x)/0.5 #1
(y*x)/5 #2
(x<y/10)[3:4] #3


#1
`/`(`-`(y, x), 0.5)
#2
`/`(`*`(y, x), 5)
#3
`[`(`<`(x,`/`(y, 10)), 3:4)

#Klammern weil das prioritäten setzt/indiziert und : weil macrofunktion für die keine Präfix vom system gestellt wird (könnte aber manuell gschrieben werden)

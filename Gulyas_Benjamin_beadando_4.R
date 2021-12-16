#4. Hozz létre egy fgv-t, amelyik data.frame-t vár bemenetként, illetve két oszlop (változó)
#nevet és kimenetként a két változóra készít egy scatterplot-t, amit kirajzol. Opcionálisan lehessen 
#tetszőleges "title"-t hozzáadni, illetve képként menteni is az eredményt.

library(utils )

graf <- function ( adat , var1, var2 , cím ) {

  if (is.data.frame(adat)) { 
    print("A scatterplot pedig...")
  } else  {  stop("Nem data.frame a bemenet") }

  png(file = "beadandó_scatterplot.png")
  
  plot(x = adat[,var1] , y = adat[, var2] ,
       xlab = var1,
       ylab = var2,
       xlim = c(min(adat[,var1]), max(adat[,var1])),
       ylim = c(min(adat[,var2]), max(adat[,var2])),	
       main = cím,
       col= "red" )
  
  dev.off()
}
 
##ellenőrzés
 
graf(mtcars, "wt", "mpg", "Title")

graf(iris, "Sepal.Length", "Sepal.Width", cím= "NA HAJRÁ")

-------------------------
#GGPLOT-os verzió
  
library(ggplot2)
library(hrbrthemes)

graf2 <- function ( adat , var1, var2 , cím ) {
  
  if (is.data.frame(adat)) { 
    print("A scatterplot pedig...")
  } else  {  stop("Nem data.frame a bemenet") }
  
  png(file = "beadandó_scatterplot.png")
  
  kep2 <- ggplot(iris, aes_string(x=var1 , y=var2  )) + 
    geom_point(size=3) +
    theme_ipsum()
print(kep2)
  
  dev.off()
}

#Ellenőrzés
graf2(mtcars, "mpg", "cyl", "Cím")

graf2(iris, "Sepal.Length", "Sepal.Width", cím= "NA HAJRÁ")

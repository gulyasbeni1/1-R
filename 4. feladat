#4. Hozz létre egy fgv-t, amelyik data.frame-t vár bemenetként, illetve két oszlop (változó)
#nevet és kimenetként a két változóra készít egy scatterplot-t, amit kirajzol. Opcionálisan lehessen 
#tetszőleges "title"-t hozzáadni, illetve képként menteni is az eredményt.


library(utils )
  
graf2 <- function ( adat , var1, var2 , cím , kiírás= "nem") {
    
    if (is.data.frame(adat)) { 
      print("A scatterplot pedig...")  }
    else  {  stop("Nem data.frame a bemenet") }

    if (!any(class(adat[,var1]) == c( "numeric", "integer", "factor"))) {
      stop("Az első változó nem szám formátumú") }
    
    if (!any(class(adat[,var2]) == c( "numeric", "integer", "factor"))) {
      stop("A második változó nem szám formátumú") }

    
    p<-  plot(x = adat[,var1] , y = adat[, var2] ,
              xlab = var1,
              ylab = var2,
              xlim = c(min(adat[,var1]), max(adat[,var1])),
              ylim = c(min(adat[,var2]), max(adat[,var2])),	
              main = cím,
              col= "red" )
    
    print(p)
    
    if (kiírás == "igen")  {
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
  }

################ Ellenőrzés
graf2(mtcars, "wt", "mpg", "Title", "igen")

graf2(iris, "Sepal.Length", "Sepal.Width", cím= "NA HAJRÁ", "igen")

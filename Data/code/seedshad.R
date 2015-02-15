R> ls()
 [1] "R_LIBS"       "dist"       "last.warning" "mmhab"   "pm.pp"   "prugeo"
 [7] "prumap"       "prumap.pp"  "sd9798"       "sd9899"  "sd9900"  "sdl97"
[13] "sdl98"        "sdl99"      "tt"          
R> names(sd9899)
[1] "window" "n"      "x"      "y"      "marks" 
R> data(julliot)
R> names (julliot)
[1] "tab"  "xy"   "area"

R> s.image(julliot$xy, log(julliot$tab[,k]+1), kgrid = 3, span = 0.25)

R> str(julliot$tab)
`data.frame':	160 obs. of  7 variables:
 $ Pouteria_torta             : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Minquartia_guianensis      : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Quiina_obovata             : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Chrysophyllum_lucentifolium: int  0 0 0 0 1 0 1 0 0 0 ...
 $ Parahancornia_fasciculata  : int  0 0 0 0 0 0 0 0 0 0 ...
 $ Virola_michelii            : int  0 1 0 6 1 2 1 0 0 0 ...
 $ Pourouma_spp               : int  0 0 0 0 0 0 0 0 0 0 ...

R> names(sd9899)
[1] "window" "n"      "x"      "y"      "marks" 

R> str(sd9899)
List of 5
 $ window:List of 3
  ..$ type  : chr "rectangle"
  ..$ xrange: num [1:2] 0 470
  ..$ yrange: num [1:2] 0 1150
  ..- attr(*, "class")= chr "owin"
 $ n     : int 615
 $ x     : num [1:615]  57.6 112.2  89.3  70.6  76.3 ...
 $ y     : num [1:615] 126 161 107 154 158 ...
 $ marks : num [1:615] 0 0 4 0 6 0 6 11 2 0 ...
 - attr(*, "class")= chr "ppp"

 R> prunus9899<- new.env()
R> prunus9899$tab<- sd9899$marks
R> names(julliot$xy)
[1] "x" "y"

R> str(julliot$area)
`data.frame':	640 obs. of  3 variables:
 $ plot: Factor w/ 160 levels "1","2","3","4",..: 1 1 1 1 2 2 2 2 3 3 ...
 $ x   : num  1.5 1.5 2.5 2.5 3.5 3.5 4.5 4.5 5.5 5.5 ...
 $ y   : num  14.5 15.5 15.5 14.5 14.5 15.5 15.5 14.5 14.5 15.5 ...

 R> prunus9899$xy<- data.frame(sd9899$x,sd9899$y)
R> str(prunus9899$xy)
`data.frame':	615 obs. of  2 variables:
 $ sd9899.x: num   57.6 112.2  89.3  70.6  76.3 ...
 $ sd9899.y: num  126 161 107 154 158 ...
R> str(julliot$area)
`data.frame':	640 obs. of  3 variables:
 $ plot: Factor w/ 160 levels "1","2","3","4",..: 1 1 1 1 2 2 2 2 3 3 ...
 $ x   : num  1.5 1.5 2.5 2.5 3.5 3.5 4.5 4.5 5.5 5.5 ...
 $ y   : num  14.5 15.5 15.5 14.5 14.5 15.5 15.5 14.5 14.5 15.5 ...
R> str(julliot$area)
`data.frame':	640 obs. of  3 variables:
 $ plot: Factor w/ 160 levels "1","2","3","4",..: 1 1 1 1 2 2 2 2 3 3 ...
 $ x   : num  1.5 1.5 2.5 2.5 3.5 3.5 4.5 4.5 5.5 5.5 ...
 $ y   : num  14.5 15.5 15.5 14.5 14.5 15.5 15.5 14.5 14.5 15.5 ...
R> julliot$area
    plot    x    y
1      1  1.5 14.5
2      1  1.5 15.5
3      1  2.5 15.5
4      1  2.5 14.5
5      2  3.5 14.5
...
R> 

### CODE STARTS HERE
area <-read.table("area.txt",header=TRUE,sep="\t",dec=".",na.strings=".")




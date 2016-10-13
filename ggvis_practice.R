library(dplyr)
library(ggvis)
data(cars)
head(cars)
data(mtcars)
head(mtcars)
p=ggvis(mtcars,x=~wt,y=~mpg)
layer_points(p)
# can also use the pipe function like dplyr to use ggvis

mtcars%>%
  ggvis(x=~wt,y=~mpg)%>%
  layer_points
library(dplyr)

mtcars%>%
  ggvis(x=~mpg,y=~disp)%>%
  mutate(disp=disp/100)%>%
  layer_points()

mtcars%>%
  ggvis(~mpg,~disp,fill=~gear)%>%layer_points()

mtcars%>%
  ggvis(~mpg,~disp,shape=~factor(gear))%>%layer_points()

#making fixed colors using := instead of =

mtcars%>%
  ggvis(~wt,~mpg,fill:="red",stroke:="blue")%>%layer_points()

mtcars%>%
  ggvis(~wt,~mpg,size:=300,opacity:=0.4,shape:="cross")%>%layer_points()

#in ggvis, you can also map the plot to interactive controls

mtcars%>%
  ggvis(~wt,~mpg,
        size:=input_slider(10,100),
        opacity:=input_slider(0,1))%>%
  layer_points()

#add interactive elements to width and center of histogram bins

mtcars%>%
  ggvis(~wt)%>%
  layer_histograms(width=input_slider(0,2,step=0.10,label="width"),
                   center=input_slider(0,2,step=0.05,label="center"))

# using keyboard controls left_right() and up_down() to control the size of points

key_s=left_right(10,1000,step=50)
mtcars%>%
  ggvis(~wt,~mpg,size:=key_s,opacity:=0.5)%>%
  layer_points()

# adding tool tips to mouse movements

mtcars%>%
  ggvis(~wt,~mpg)%>%
  layer_points()%>%
  add_tooltip(function(df)df$wt)

#adding more complex layers
# simple layers
#layer_points() with properties - shape, size, stroke, fill, fillOpacity, opacity,strokeOpacity

#2. paths and polygons- layer_paths()

df=data.frame(x=1:10,y=runif(10))
df

df%>%
  ggvis(~x,~y,fill:="green",opacity:=0.5)%>%layer_paths()

#use layer ribbons to control the extent of the area

df%>%
  ggvis(~x,~y,fill:="green")%>%layer_ribbons()

#layer rectangles

set.seed(1234)
df=data.frame(x1=runif(5),x2=runif(5),y1=runif(5),y2=runif(5))
df%>%
  ggvis(~x1,~y1,x2=~x2,y2=~y2,fillOpacity:=0.4)%>%layer_rects()

#layer_text to control the appearance of text
df=data.frame(x=3:1,y=c(1,3,2),label=c("a","b","c"))
df
df%>%ggvis(~x,~y , text:=~label)%>%layer_text()
df%>%ggvis(~x,~y , text:=~x)%>%layer_text()

df%>%ggvis(~x,~y , text:=~label)%>%layer_text(fontSize:=30)

df%>%ggvis(~x,~y , text:=~label)%>%layer_text(angle:=45,fontSize:=40)

#compound layers

#layer_lines

t=seq(0,2.5*pi,length=20)
df=data.frame(x=sin(t),y=cos(t))
df%>%ggvis(~x,~y)%>%layer_lines()
df%>%ggvis(~x,~y)%>%layer_paths()
#layer_lines = arrange(x)+layer_paths
df%>%ggvis(~x,~y)%>%arrange(x)%>%layer_paths()

mtcars%>% ggvis(~mpg)%>%layer_histograms()
#layer_smooth() is a smooth line to the data and displays the predictions with the line

mtcars%>%ggvis(~wt,~mpg)%>%layer_smooths()

span=input_slider(0.2,1,value=0.75)
mtcars%>%ggvis(~wt,~mpg)%>%layer_smooths(span=span)

#multiple layers

mtcars%>%
  ggvis(~wt,~mpg)%>%
  layer_smooths()%>%
  layer_points()

#separate marke for each group

mtcars%>%
  group_by(cyl)%>%
  ggvis(~mpg,~wt)%>%
  layer_smooths()
  
sliderbox=input_slider(.1,2,value=1,step=0.1,label="Bandwidth adjustment")

selectbox=input_select(c("Gaussian" = "gaussian",
                         "Epanechnikov" = "epanechnikov","Rectangular" = "rectangular",
                         "Triangular" = "triangular","Biweight" = "biweight",
                         "Cosine" = "cosine", "Optcosine" = "optcosine"),label = "Kernel")
mtcars%>%
  ggvis(x=~wt)%>%
  layer_densities(adjust=sliderbox,kernel=selectbox)

# hover functions for displaying all values
all_values=function(x){
  if(is.null(x)) return(NULL)
  paste0(names(x),":",format(x),collapse = "<br />")
}

mtcars%>%
  group_by(factor(gear))%>%
  ggvis(~wt,~mpg,fill=~gear)%>%
  layer_points(fill.hover:="red")%>%
  add_tooltip(all_values,"hover")
  

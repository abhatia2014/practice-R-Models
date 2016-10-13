install.packages("mlmRev")
library(mlmRev)
data("Chem97")
head(Chem97)
#histogram of the gcsescore
library(lattice)
histogram(~gcsescore,data=Chem97)
#getting histogram by score
histogram(~gcsescore|factor(score),data=Chem97)
densityplot(~gcsescore|factor(score),Chem97,groups = gender,
            plot.points=FALSE,auto.key = TRUE)
#following displays are available
#histogram
#densityplot
#qqmath=theoretical quantile plot
#stripplot- stripchart- 1D scatter plots
#bwplot- box and whisker plot
# dotplot- 
#   barchart
# xyplot- scatter plot
# splom- scatter plot matrix
# contourplot= countour plot of surfaces
# levelplot-level plot of surfaces
# wireframe-3dimensional perspective plot(
#   cloud- 3 dimensional scatter plot
#   parallel- parallel coordinates plot
# )

bwplot(gcsescore~gender|factor(score),Chem97,layout=c(6,1))


#Loading required libraries
library(kohonen)
library(dplyr)
library(vtable)

#Reading data
df = read.csv("day5result_day4med.csv")
View(df)

data = df[,c(1:9)]

#standardize data with mean
dfone = round(df %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)), digits = 3)
dfone
View(dfone)

#Descriptive statistics
st(dfone,
   summ = list(
     c('countNA(x)','min(x)','max(x)','mean(x)','sd(x)')
     
   ),
   summ.names = list(
     c('N','Min','Max','Mean','SD')
   )
)

#Scaling data and model building
set.seed(100)

datatwo = as.matrix(dfone, "scale:center")
datatwo 

som_grid = somgrid(xdim = 3, ydim=3, topo="rectangular", neighbourhood.fct = "gaussian", toroidal = FALSE)

supersom = som(datatwo, 
               grid=som_grid, rlen = 100,
               alpha=c(0.05,0.01),
               whatmap = NULL,
               maxNA.fraction = 0L,
               user.weights = (0.0005 -0.0005),
               radius = 1,
               dist.fcts = NULL,
               mode = "online",
               init,
               keep.data = TRUE)


#Data visualization

#Training process
plot(supersom, type="changes")

#Codes plot
suppressWarnings(plot(supersom, type="codes"))

#Counts plot
plot(supersom, type ="counts")

#Variables influence plot
par(mfrow=c(3,3));
for (j in 1:9) { plot(supersom,type='property', property=as.data.frame(supersom$codes)[,j],main=colnames(data)[j],cex=0.8,palette.name=function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]})}


# Creating a dataframe with the assigned unit classes and distances:
res <- as.data.frame(cbind(som_model$unit.classif, som_model$distances))
colnames(res) <- c("Class","Distances")
View(res)

write.csv(res, file = "observation_one.csv", row.names = FALSE)

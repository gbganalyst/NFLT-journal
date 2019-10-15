
# Post processing of predictive ranking of results on classification and regression datasets

# Each code line should be run with `CTRL + ENTER`.

# R Packages ----------------------------------------

source("Various heatmaps/R-script/Packages.R") # Any error? Check `Packages.R` script for instructions.


# Standard deviation ranking on Binary class data --------

 nflt <- read_excel('Table of ranks/Table of SD ranks of learning methods.xlsx', sheet=1, range = 'B22:M37')
 
   nflt <- nflt %>% column_to_rownames(.,var = 'Methods')
   n <- nrow(nflt);n  # Number of methods explored
   p <- ncol(nflt);p  # Number of datasets used
   

   x <- nflt
   
   distance <- 'manhattan'
   linkage  <- 'ward.D2'
   
#  Heatmap

# To save the plot to a file

png(file = 'Various heatmaps/Binary/SD rank/Heatmap.png')

  heatmap(as.matrix(x))

dev.off()
   
   
#  Hierarchical clustering
  
# To save the plot to a file

png(file = 'Various heatmaps/Binary/SD rank/hclust.png')
  
   dist.x <- dist(x, method=distance)
   dist.x <- exp(-0.09*dist.x)
   
   hclust.x <- hclust(dist.x, method=linkage)
   plot(hclust.x)
   rect.hclust(hclust.x, k=5,border ='red')
dev.off()       
   
#  Graph theoretic visualization of groupings/clusterings
  
   pam.x <- pam(x, k=5, metric=distance)
   grouping <- pam.x$clustering
   groups.x <- list(which(grouping==1), which(grouping==2),     which(grouping==3), which(grouping==4), which(grouping==5    ))
  
# To save the plot to a file
   
png(file = 'Various heatmaps/Binary/SD rank/g.theoretic.png')  
    graph.x <- qgraph(dist.x, layout="spring", 
    sampleSize = nrow(dist.x), groups=groups.x, color=2:6,       vsize=7, cut=0, maximum=.45, border.width=1.5)
dev.off()  



# Median ranking on Multi class data --------

nflt <- read_excel('Table of ranks/Table of SD ranks of learning methods.xlsx', sheet=2, range = 'B20:K35')

nflt <- nflt %>% column_to_rownames(.,var = 'Methods')
n <- nrow(nflt);n  # Number of methods explored
p <- ncol(nflt);p  # Number of datasets used


x <- nflt

distance <- 'manhattan'
linkage  <- 'ward.D2'

#  Heatmap

# To save the plot to a file

png(file = 'Various heatmaps/Multi/SD rank/Heatmap.png')

  heatmap(as.matrix(x))

dev.off()


#  Hierarchical clustering

# To save the plot to a file

png(file = 'Various heatmaps/Multi/SD rank/hclust.png')

  dist.x <- dist(x, method=distance)
  dist.x <- exp(-0.09*dist.x)

  hclust.x <- hclust(dist.x, method=linkage)
  plot(hclust.x)
  rect.hclust(hclust.x, k=5,border ='red')
dev.off()       

#  Graph theoretic visualization of groupings/clusterings

pam.x <- pam(x, k=5, metric=distance)
grouping <- pam.x$clustering
groups.x <- list(which(grouping==1), which(grouping==2),     which(grouping==3), which(grouping==4), which(grouping==5    ))

# To save the plot to a file

png(file = 'Various heatmaps/Multi/SD rank/g.theoretic.png')  
  graph.x <- qgraph(dist.x, layout="spring", 
                  sampleSize = nrow(dist.x), groups=groups.x, color=2:6, vsize=7, cut=0, maximum=.45,   border.width=1.5)

dev.off()  



# std ranking on Regression data --------

nflt <- read_excel('Table of ranks/Table of SD ranks of learning methods.xlsx',sheet=3, range = 'B33:X48')

nflt <- nflt %>% column_to_rownames(.,var = 'Methods')
n <- nrow(nflt);n  # Number of methods explored
p <- ncol(nflt);p  # Number of datasets used


x <- nflt

distance <- 'manhattan'
linkage  <- 'ward.D2'

#  Heatmap

# To save the plot to a file

png(file = 'Various heatmaps/Regression/SD rank/Heatmap.png')

heatmap(as.matrix(x))

dev.off()


#  Hierarchical clustering

# To save the plot to a file

png(file = 'Various heatmaps/Regression/SD rank/hclust.png')

dist.x <- dist(x, method=distance)
dist.x <- exp(-0.09*dist.x)

hclust.x <- hclust(dist.x, method=linkage)
plot(hclust.x)
rect.hclust(hclust.x, k=5,border ='red')
dev.off()       

#  Graph theoretic visualization of groupings/clusterings

pam.x <- pam(x, k=5, metric=distance)
grouping <- pam.x$clustering
groups.x <- list(which(grouping==1), which(grouping==2),     which(grouping==3), which(grouping==4), which(grouping==5))

# To save the plot to a file

png(file = 'Various heatmaps/Regression/SD rank/g.theoretic.png')  
graph.x <- qgraph(dist.x, layout="spring", 
                  sampleSize = nrow(dist.x), groups=groups.x, color=2:6, vsize=7, cut=0, maximum=.45,   border.width=1.5)

dev.off()  


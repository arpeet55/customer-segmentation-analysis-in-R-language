


customer_data <- read.csv("D:\\r project\\customer-segmentation-dataset\\Mall_Customers.csv")

#to get structure of data
str(customer_data)

#names of column
names(customer_data)

#display the first six rows of our dataset using the head() function
#and use the summary() function to get quick summary of it.
head(customer_data)
summary(customer_data$Age)

#after that we will take summery and sd of age, and anual income
sd(customer_data$Age)
summary(customer_data$Annual.Income..k..)
sd(customer_data$Annual.Income..k..)
summary(customer_data$Age)
sd(customer_data$Spending.Score..1.100.)

#create a barplot and a piechart to show the gender distribution across our customer_data dataset.
a=table(customer_data$Gender)
barplot(a,main="Using BarPlot to display Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=rainbow(3),
        legend=rownames(a))

#to observe the ratio of male and female we create pie chart
pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(a,labels=lbs,main="Pie Chart Depicting Ratio of Female and Male")

# we will visualiz histogram for more deep knowledge
hist(customer_data$Age,
     col="#5C05FF" ,
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)

boxplot(customer_data$Age,
        col="green ",
        main="Boxplot for Descriptive Analysis of Age")

#from above two graph we conclude that max people are age of 30 to 35
#and the min age is 17 and max is 70



# now we will analyze annual income of customer
summary(customer_data$Annual.Income..k..)
hist(customer_data$Annual.Income..k..,
     col="pink",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)

plot(density(customer_data$Annual.Income..k..),
     col="yellow",
     main="Density Plot for Annual Income",
     xlab="Annual Income Class",
     ylab="Density")
polygon(density(customer_data$Annual.Income..k..),
        col="#ccff66")

#from above visualization we conclude that min anual income is 3.0 and maximum is 150 points


#summer of spending score
summary(customer_data$Spending.Score..1.100.)



boxplot(customer_data$Spending.Score..1.100.,
        horizontal=TRUE,
        col="#990000",
        main="BoxPlot for Descriptive Analysis of Spending Score")


hist(customer_data$Spending.Score..1.100.,
     main="HistoGram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="#6600cc",
     labels=TRUE)

#people with spending score 45 has more

#Elbo method to find the no of cluster
install.packages("purrr")

library(purrr)
set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
        kmeans(customer_data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}

k.values <- 1:10


iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")

#With the help of the average silhouette method, we can measure the quality of our clustering operation. 
#With this, we can determine how well within the cluster is the data object.

install.packages("cluster")
install.packages("gridExtra")
install.packages("grid")

library(cluster) 
library(gridExtra)
library(grid)


k2<-kmeans(customer_data[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")
s2<-plot(silhouette(k2$cluster,dist(customer_data[,3:5],"euclidean")))


k3<-kmeans(customer_data[,3:5],3,iter.max=100,nstart=50,algorithm="Lloyd")
s3<-plot(silhouette(k3$cluster,dist(customer_data[,3:5],"euclidean")))


k4<-kmeans(customer_data[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s4<-plot(silhouette(k4$cluster,dist(customer_data[,3:5],"euclidean")))


k5<-kmeans(customer_data[,3:5],5,iter.max=100,nstart=50,algorithm="Lloyd")
s5<-plot(silhouette(k5$cluster,dist(customer_data[,3:5],"euclidean")))

k6<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
s6<-plot(silhouette(k6$cluster,dist(customer_data[,3:5],"euclidean")))


k7<-kmeans(customer_data[,3:5],7,iter.max=100,nstart=50,algorithm="Lloyd")
s7<-plot(silhouette(k7$cluster,dist(customer_data[,3:5],"euclidean")))


k8<-kmeans(customer_data[,3:5],8,iter.max=100,nstart=50,algorithm="Lloyd")
s8<-plot(silhouette(k8$cluster,dist(customer_data[,3:5],"euclidean")))


k9<-kmeans(customer_data[,3:5],9,iter.max=100,nstart=50,algorithm="Lloyd")
s9<-plot(silhouette(k9$cluster,dist(customer_data[,3:5],"euclidean")))


k10<-kmeans(customer_data[,3:5],10,iter.max=100,nstart=50,algorithm="Lloyd")
s10<-plot(silhouette(k10$cluster,dist(customer_data[,3:5],"euclidean")))


#so here we get silhouette plot and we can see here customer segmentation is done here


# to determine optimum value of cluster we use nbclust and factoextra library

install.packages("NbClust")
install.packages("factoextra")

library(NbClust)
library(factoextra)

fviz_nbclust(customer_data[,3:5], kmeans, method = "silhouette")



set.seed(125)
stat_gap <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)


#prediction

k6<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6


pcclust=prcomp(customer_data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)

pcclust$rotation[,1:2]



#visualization

set.seed(1)
ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
        geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
        scale_color_discrete(name=" ",
                             breaks=c("1", "2", "3", "4", "5","6"),
                             labels=c("high annual income as well as a high annual spend", " high annual income and low yearly spend", "low annual income as well as low yearly spend of income.", "medium income salary as well as the medium annual spend of salary", "low annual income but its high yearly expenditure","slightly medium income salary with the medium annual spend of salary")) +
        ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

#kmean

ggplot(customer_data, aes(x =Spending.Score..1.100., y =Age)) + 
        geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
        scale_color_discrete(name=" ",
                             breaks=c("1", "2", "3", "4", "5","6"),
                             labels=c("high annual income as well as a high annual spend", " high annual income and low yearly spend", "low annual income as well as low yearly spend of income.", "medium income salary as well as the medium annual spend of salary", "low annual income but its high yearly expenditure","slightly medium income salary with the medium annual spend of salary")) +
        ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")





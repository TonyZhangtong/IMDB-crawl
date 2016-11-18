#Junqing Ma
#----------------Loading of Packages----------------#

library(randomForest)
library(miscTools)
library(ggplot2)
library(caret)
#preprocess
#----------------Data import----------------#
#import raw data from *.cvs file, and stored as data.frame
#input_file <-  file.path("..", "Data",  "movie_metadata.csv")
#input_file <-  file.path("movie_metadata.csv")
#rawMovieData<-read.csv("movie_metadata.csv")
rawMovieData<-read.csv("C:\\Users\\mjq\\Dropbox\\R program\\Project of Survey\\Code\\movie_metadata.csv",head=TRUE,sep = ",")

#---------------Data Recognization----------------#
#demonstrate that rawMovieData is.data.frame
print(class(rawMovieData))
print(names(rawMovieData))
#Determine the data types of each colunm in the data.frame
print(sapply(rawMovieData, class))
#check dimension
print(dim(rawMovieData))

#-------------- raw Data clearance----------------#

#----to remove rows with empty from rawdata
rawMovieData1 <- rawMovieData[!apply(rawMovieData, 1, function(x) any(x=="")),] 
rownames(rawMovieData1) <- 1:nrow(rawMovieData1)
#----to contine removing rows with NA 
#rawMovieData2 <- rawMovieData1[complete.cases(rawMovieData1),]
rawMovieData2<-na.omit(rawMovieData1)
rownames(rawMovieData2) <- 1:nrow(rawMovieData2)
#----to contine removing rows with 0 (except the column of facenumber_in_poster)

#-to definde  name of columns that should not contain zeros
#columnsNoZero=c('movie_facebook_likes')
#row_no_zero=apply(rawMovieData2[,c(14:21)],1,function(row) all(row!=0))
#row_no_zero = apply(rawMovieData2[,c(14:21)]!=0, 1, all)
#rawMovieData3 = rawMovieData2[row_no_zero,]
#print(row_sub)
#clearZone<-function(x)
#{
#  all(x!=0)
#}
#row_no_zero=lapply(rawMovieData2[,c(14:21)], clearZone)
# a=rawMovieData2
# a[a==0]<- NA
# rawMovieData3<-na.omit(a[,c(14:19)])
# method 2
dde=rawMovieData2[,c(14:21)]
rawMovieData3 = rawMovieData2[!as.logical(rowSums(dde==0)), ]
rm(dde)
# assign the cleared data to clearMovieData
clearedMovieData=rawMovieData3

#----------------Random Forest for Regression----------------#
#----------------select training data and test data
##set random number generator
set.seed(3000)
##set some colunms with strings to Integers or Bool
clearedMovieData$is_color <- factor(ifelse(clearedMovieData$color=='Color', 1, 0))
trainIndex <- createDataPartition(clearedMovieData$title_year, p = .9, 
                                  list = FALSE, 
                                  times = 1)
train = clearedMovieData[ trainIndex,]
test = clearedMovieData[ -trainIndex,]
##select predictor vectors(input factors) and response vectors(output factors)
cols <- c('is_color','title_year', 'num_voted_users', 'director_facebook_likes', 'num_user_for_reviews', 'num_critic_for_reviews',
          'cast_total_facebook_likes','actor_1_facebook_likes','actor_2_facebook_likes','actor_3_facebook_likes',
          'movie_facebook_likes','gross','facenumber_in_poster','imdb_score','budget','aspect_ratio')
rf <- randomForest(gross ~ ., data=train[,cols], ntree=150)
r2 <- rSquared(test$gross, test$gross - predict(rf, test[,cols]))
mse <- mean((test$gross - predict(rf, test[,cols]))^2)
rmse<-sqrt(mse)
print(rmse)
p <- ggplot(aes(x=actual, y=pred),
            data=data.frame(actual=test$gross, pred=predict(rf, test[,cols])))
p + geom_point() +
  geom_abline(color="red") +
  ggtitle(paste("RandomForest Regression in R r^2=", r2, sep=""))

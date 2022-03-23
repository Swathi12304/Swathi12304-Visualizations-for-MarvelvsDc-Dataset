df <- read.csv('C:/Users/HP/Downloads/marvelvsdc (1).csv')
str(df)
attach(df)
str(df)

plot(df$runtime)

scaleddata <- scale(df)
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
maxmindf <- as.data.frame(lapply(df,normalize))

trainset <- maxmindf[1:150, ]
testset <- maxmindf[151:200, ]

library(neuralnet)
nn <- neuralnet(runtime~imdb_votes+imdb_gross+tomato_meter+tomato_review+tom_aud_score+tom_ratings, 
                 data = trainset, linear.output = FALSE, threshold = 0.01)
plot(nn)

nn <- neuralnet(runtime~imdb_rating+imdb_votes+imdb_gross+tomato_meter+tomato_review+tom_aud_score+tom_ratings,
                data=trainset,algorithm="backprop",learningrate=0.0001)
plot(nn)

nn <- neuralnet(runtime~imdb_rating+imdb_votes+imdb_gross+tomato_meter+tomato_review+tom_aud_score+tom_ratings,
                  data=trainset,act.fct="logistic",linear.output = FALSE)
plot(nn)


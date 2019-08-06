# Regression model, predict likes with the help of number of views and category id

#Project Stage 1: Finding the dataset
# Downloaded from Kaggle

#Project Stage 2: Importing the dataset
library(caret)
youtube=read.csv("USvideos.csv")
youtube=youtube[complete.cases(youtube),]
myyoutube=youtube[1:100,c("category_id","views","likes")]

#Project Stage 3:Taking advantage of plots to show correlations, distributions and outliers in the data set

attach(myyoutube)

# Finding correlation
attach(myyoutube)
cor(likes,views)
# high correlation 0.709145
plot(likes,views)

pal <- c("red", "blue", "green")
p <- plot_ly(data = myyoutube, x = ~category_id, y = ~likes, color = ~category_id, colors = pal)
p

q <- plot_ly(data = myyoutube, x = ~likes, y = ~views,color = ~likes, size = ~likes)
q
# Plotting Distribution

ggplot(myyoutube,aes(category_id,views,likes,color=likes))+geom_jitter()

myyoutube %>%
  plot_ly() %>% 
  add_trace(x = ~as.numeric(category_id),y = ~views, color = ~likes, type = "box", 
            hoverinfo = 'name+y') %>%
  add_markers(x = ~jitter(as.numeric(category_id)), y = ~views, color = ~likes,
              marker = list(size = 6),
              hoverinfo = "text",
              showlegend = FALSE) %>% 
  layout(legend = list(orientation = "h",
                       x =0.5, xanchor = "center",
                       y = 1, yanchor = "bottom"
  ),
  xaxis = list(title = "category_id",
               showticklabels = FALSE))

# Plotting Outliers
boxplot(views)
library(plotly)
boxplotviews <- plot_ly(y = ~myyoutube$views, type = "box", boxpoints = "all", jitter = 0.3,pointpos = -1.8) %>%
  layout(
    title = 'Outliers in views',
    yaxis = list(
      title = 'Number of views',
      range = c(2932,10535242)
    )
  )
boxplotviews

boxplotlikes <- plot_ly(y = ~myyoutube$likes, type = "box", boxpoints = "all", jitter = 0.3,pointpos = -1.8) %>%
  layout(
    title = 'Outliers in likes',
    yaxis = list(
      title = 'Number of likes',
      range = c(0,576597)
    )
  )
boxplotlikes

# Best fitting line
ggplot(myyoutube,aes(likes,views,color=views))+geom_point(size=2,color="purple",fill="blue")+ggtitle("Best fitting line")+theme(text = element_text(size=10))+xlab("Number of likes")+ylab("Number of views")+theme_bw()+geom_smooth(method = "lm",se=FALSE)

#Project Stage 4: Building a model to predict likes in terms of number of views and category id.
# sampling method: bootstrap
train_controlyoutube= trainControl(method = "boot", number = 50)
m1=train(likes ~.,myyoutube,trControl=train_controlyoutube,method="lm")
m2=train(likes ~.,myyoutube,trControl=train_controlyoutube,method="rf")
m3=train(likes ~.,myyoutube,trControl=train_controlyoutube,method="kknn")

# visialise  the document, the one which hass the lowest RMSE is the best model.
allModels=resamples(list(LinearModel=m1,RandomForest=m2,Knearest=m3))
bwplot(allModels,scales=list(relation="free"))


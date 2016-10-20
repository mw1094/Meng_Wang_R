library(ggplot2)
  # install.packages("grid")
library(grid)
  # We need two packages above to finish questions

#2
ggplot(diamonds,aes(carat,price,color=factor(color)))+geom_point()+labs(
  title="Diamonds - Weight to Price by Color",x='Weight',y='Price')+
  theme(plot.title=element_text(colour='blue'))
  # plot data,carat=x-axis,price=y-axis,use 'color' as colur, and make a scatter point graph
  # add title name and x-y axis name
  # change title color to blue as sample shows



#3
ggplot(diamonds,aes(log(carat),log(price),color=factor(color)))+geom_point()+
  labs(title="Diamonds - Weight to Price by Color (Linear)",x='Weight',y='Price')+
  theme(plot.title=element_text(colour='blue'))
  # same as question 1 but use log(data) as input to remove non-linearity

#4
diamonds_lm = lm(log(price) ~ log(carat), diamonds) 
  # use linear regression function to sort data and use log(price) and residual price as input data
ggplot(diamonds,aes(log(carat), resid(diamonds_lm),color=factor(color)))+geom_point()+
  labs(title="Diamonds - Weight to Price by Color",x='Weight',y='Price Residuals')+
  theme(plot.title=element_text(colour='blue'),legend.position="top")
  # plot a scatter graph and add title names
  # re-position legend "factor(color)" to the top

#5
hist_carat <- ggplot(diamonds,aes(carat,colour=factor(color)))+geom_histogram(aes(y=..density..),binwidth=0.05) +
              theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position='none')
hist_price <- ggplot(diamonds,aes(price,fill=factor(color)))+geom_histogram(aes(y=..density..),binwidth=100) +
              theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position='none')
  # make two density histograms and name them as hist_carat and hist_price
  # take off axis name and 'factor(color)' as sample figure shows
diamonds_lm = lm(log(price) ~ log(carat), diamonds) 
ggplot(diamonds,aes(log(carat), resid(diamonds_lm),color=factor(color)))+geom_point()+
  labs(title="Diamonds - Weight to Price by Color",x='Weight',y='Price Residuals')+
  theme(plot.title=element_text(colour='blue'),legend.position='top')
  # repeat steps in #4 again to make background graph
vp1 <- viewport(w=0.4,h=0.2,x=0.25,y=0.16)
  # define the position of hist_price
vp2 <- viewport(w=0.4,h=0.2,x=0.82,y=0.75)
  # define the position of hist_carat
print(hist_price, vp = vp1)
print(hist_carat, vp = vp2)
  # place hist_price on position vp1, and hist_carat on position vp2


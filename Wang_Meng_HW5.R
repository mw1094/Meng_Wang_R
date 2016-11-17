library("ggplot2")
##Prof G - Nice work!
#1
#Print all methods associates with a dataframe
methods(class=data.frame)
#Print all attributes
##Prof G - a little messy
attributes(diamonds)
#Determine the number of columns in a dataframe
ncol(diamonds)

#2
#Determine how many rows in a dataframe
nrow(diamonds)

#3
#Extract column names and write in line
writeLines(colnames(diamonds))

#4
#Determine the type of each column
lapply(diamonds,class)

#5
#Loop through any dataframe and calculate the mean of
#every numeric column.
diamonds_num <- diamonds[,sapply(diamonds,is.numeric)]#find numeric columns
lapply(diamonds_num,mean,na.rm=TRUE)#calculate the mean of every numeric column

#question 6
#Loop through any dataframe and create a frequency table
#for every factor column.
lapply(diamonds[,sapply(diamonds,is.factor)],table)#find factor columns and make the table

#question 7
#Determine the number of rows containing NA (missing value) in each column
apply(sapply(diamonds,is.na),2,sum)
#Calculate the percentage of rows containing an NA in any of the columns
temp <- sum(rowSums(is.na(diamonds))>0)/nrow(diamonds)
sum_per <- cat(temp*100,'%',sep='')#percentage format

#question 8
pearson_coe <- function(dafra){
  #This function can accept any dataframe as a parameter and
  #returns a dataframe that contains each pair of column names in the first
  #column in a single string separated by a -,and their corresponding Pearson 
  #correlation coefficient in the second column.
  dafra=dafra[sapply(dafra,is.numeric)]#select numeric columns
  a=colnames(dafra)#let a be column names
  pair_names=c()
  pair_coe=c()
  for (i in 1:(length(a)-1)){#from the first column to the second-last column
    for (j in (i+1):length(a)){#from the second column to the last column 
      pair_names = c(pair_names,paste(a[i],a[j],sep='-'))#name1+'-'+name2
      pair_coe = c(pair_coe,cor(dafra[i],dafra[j],method="pearson"))#calculate coefficient 
    }
  }
  return (data.frame(pair_names,pair_coe)) #return a new dataframe
}
#e.g.
pearson_coe(diamonds)

  
    
#load the file
myfile=read.csv("D:\\fourth year assignments\\Biostat\\Assignment1\\Assignment_One\\data.csv")

#1.show the first 10 rows and the last 10 rows

#first 10 rows
h=head(myfile,10)
h
#last 10 rows
t=tail(myfile,10)
t

#2.Using Date of Birth attribute, extracts the gender, average commuting
#time, and ancestry data for the oldest three.

#sorting 
sorted_dob=sort(myfile$dob)
#sorted_dob
#ordering to find indices of the oldest three 
ordered_dob=order(myfile$dob)
#ordered_dob

oldest_three=head(ordered_dob,3) 
oldest_three
oldest_three2=head(sorted_dob,3)
oldest_three2

first_old<-c(myfile$gender[981],myfile$avg_commute[981],myfile$ancestry[981])
first_old
second_old<-c(myfile$gender[281],myfile$avg_commute[281],myfile$ancestry[281])
second_old
third_old<-c(myfile$gender[89],myfile$avg_commute[89],myfile$ancestry[89])
third_old

#3.Identifies the gender, daily internet use, average commute time, ancestry,
#and diseases among those with more than two children.
myfile[myfile$children>2,c("gender","daily_internet_use","avg_commute","ancestry","disease")]

#4.Using a table , indicate the number of rows that have any missing values and the number that do not.
na=is.na(myfile)
n=nrow(na)
n


#5.Provide a summary of the data for each column, showing "Min, 1st Qu,Median Mean, 3rd Qu and Max" for each numerical column
#finding class of each column to identify numeric

#for loop to identify the class of each column
for(i in colnames(myfile)){
  print(class(myfile[[i]]))
}

get_summary<-function(myfile){
  for(i in colnames(myfile)){
    if(class(myfile[[i]])=='numeric'||class(myfile[[i]])=='integer'){
      s=summary(myfile[[i]])
      print(s)
    }else{
      print(table(myfile[[i]]))
    }
  }
}

get_summary(myfile)


#6.Identify the columns that are having any missing values, and then remove any rows where all of the columns have missing values.
identify_col_na<-function(myfile){
  for(i in colnames(myfile)){
    ifelse(is.na(myfile[[i]]),print(sapply(myfile,names))
           ,print('no missing values in data columns'))
  }
}
identify_col_na(myfile)
identify_rows_na<-function(myfile){
  for(i in 1:nrow(myfile)){
    if(sum(is.na(myfile[i,]))==12){
      na.omit(myfile)
    }
    else{
      print("nothing to delete")
      break
    }
  }
}

identify_rows_na(myfile)
#7.Show the average daily usage of the internet for each level of education.
average=tapply(myfile$daily_internet_use,myfile$education,mean)
average
barplot(average,beside=T,col=c(1,2,3,4,5,6))

#8.Show the distribution of the children count using a histogram.
#barplot(table(myfile$children),col=c(1,2,3,4,5,6,7))
hist(table(myfile$children),col=c(1,2,3,4,5,6,7))
#9.Utilizing line graphs, compare how men and women's avg commute distributions differ.
sub_male=subset(myfile,gender=='male')
sub_female=subset(myfile,gender=='female')
plot(sub_male$avg_commute,type="o",col='red',main="avg_commute distribution for males")
plot(sub_female$avg_commute,type="o",col="blue",main="avg_commute distribution for females")
#10.Make a histogram to show the gender distribution.
#hist(table(myfile$gender),main="Gender Distribution",col = c('red','blue'))
counts=table(myfile$gender)
counts
barplot(counts,main="gender distribution",col=c('red','blue'))
#11. Use a histogram to show gender distribution for each disease.
counts<-table(myfile$gender,myfile$disease)
counts
barplot(counts,beside=T,legend=c('female','male'),
        ,args.legend=list(cex=0.50,x="topright"),col=c('red','green'),
        names=c("Alzehimer's","breast cancer","diabetes","endometriosis",
                "gastritis","heart disease","HIV/AIDS","hypertension","kidney disease",
                "multiple sclerosis","prostate cancer","schizophrenia","skin cancer"),las=3,cex.names=0.6)
#12.Use a chart to demonstrate whether there is a relationship between age and the type of disease.
library(eeptools)
x_age<-floor(age_calc(as.Date(myfile$dob),Sys.Date(),units='years'))
#x_age
hist(x_age,main="Age distribution",xlab="ages",col=2)

#13.Make a chart to show the total number of children per disease.
t=table(myfile$children,myfile$disease)
s=colSums(t)
barplot(s,beside=T,
        ,col=c(1,2,3,4,5,6,7,8,9,10,11,12,13),
        names=c("Alzehimer's","breast cancer","diabetes","endometriosis",
                "gastritis","heart disease","HIV/AIDS","hypertension","kidney disease",
                "multiple sclerosis","prostate cancer","schizophrenia","skin cancer")
        ,las=3,cex.names=0.7)

#14.Make a chart to show the ancestry distribution.
barplot(table(myfile$ancestry),las=2,cex.names=0.7,col=c(1,2,3,4,5,6,7,8
                                                         ,9,10,11,12,13,14
                                                         ,15,16,17,18,19,20))


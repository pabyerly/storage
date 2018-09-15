#modified code from https://ourcodingclub.github.io/2016/11/13/intro-to-r.html
#data from Byerly et al. 2018

#dplyr package: graphics 
library(dplyr) 

foxy=read.csv("kf_master_data.csv")
#data check: make sure dataset is structure correctly
head(foxy)
tail(foxy)
str(foxy)

#change number or character to factor & vice versa
#foxy$XXX=as.factor(foxy$XXX)

#filter data by species/group/whatever 
kf = filter(foxy, Species =="VUMA")
coy = filter(foxy, Species =="CALA" )
#and by species/variable type 
kf1=filter(foxy, Species == "VUMA", Season=="Summer")

#add/create a new column 
addstuff=mutate(foxy, Percent=Rabbit/Rodent)

#calculate summary statistics for data
kf_average=summarise(addstuff, kf_mean=mean(Percent))

#subset data for diff sites, species, whatever 
kf_grouped=group_by(addstuff, Rodent, Rabbit)
kf_summary=summarise(kf_grouped, Average=mean(Percent))
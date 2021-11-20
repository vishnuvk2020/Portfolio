# vocation analysis

library(data.table)
fleetdna_compositedata<-fread("data_for_fleet_dna_composite_data.csv",stringsAsFactors = FALSE)

# including voc_id, drive id, class id, fuel id

fleetdna_compositedata_vocation<-fleetdna_compositedata[,c("voc_id","trip_count","total_average_speed","max_speed","driving_average_speed","distance_total","acceleration_events_per_mile","deceleration_events_per_mile","total_stops" ,"average_stop_duration","max_elevation","min_elevation","max_climbing_rate" ,"average_climbing_rate","max_descending_rate","average_descending_rate","max_road_grade","mean_road_grade","maximum_kinetic_power_density_demand","total_kinetic_power_density_demand","average_kinetic_power_density_demand","maximum_potential_power_density_demand","total_potential_power_density_demand","average_potential_power_density_demand" ,"total_aerodynamic_power_density_demand","aerodynamic_speed","kinetic_intensity","average_aerodynamic_power_density_demand","maximum_rolling_power_density_demand","total_rolling_power_density_demand","average_rolling_power_density_demand","characteristic_acceleration","characteristic_deceleration","maximum_kinetic_power_density_regen","total_kinetic_power_density_regen","average_kinetic_power_density_regen","maximum_potential_power_density_regen","total_potential_power_density_regen","average_potential_power_density_regen" )]




 # identifying vocation ids: 5 School Bus,10 Mass Transit,4 Parcel Delivery,3 Warehouse Delivery,2 Beverage Delivery,14 Food Delivery

schoolbus<-fleetdna_compositedata_vocation[fleetdna_compositedata_vocation$voc_id==5,]
masstransit<-fleetdna_compositedata_vocation[fleetdna_compositedata_vocation$voc_id==10,]
parcel<-fleetdna_compositedata_vocation[fleetdna_compositedata_vocation$voc_id==4,]
beverage<-fleetdna_compositedata_vocation[fleetdna_compositedata_vocation$voc_id==2,]
warehouse<-fleetdna_compositedata_vocation[fleetdna_compositedata_vocation$voc_id==3,]
food<-fleetdna_compositedata_vocation[fleetdna_compositedata_vocation$voc_id==14,]
fleetdna_interest_vocations<-rbind(schoolbus,masstransit,parcel,beverage,warehouse,food)


# converting int varibales into factor levels

#vfleetdna_interest_vocations$fuel_id<-as.factor(fleetdna_interest_vocations$fuel_id)
fleetdna_interest_vocations$voc_id<-as.factor(fleetdna_interest_vocations$voc_id)
#fleetdna_interest_vocations$class_id<-as.factor(fleetdna_interest_vocations$class_id)
#fleetdna_interest_vocations$drive_id<-as.factor(fleetdna_interest_vocations$drive_id)

str(fleetdna_interest_vocations)


# splitting the data into test and train data

# creating a training and testing data set
set.seed(100)
index<-sample(1:2,nrow(fleetdna_interest_vocations),replace = TRUE,prob = c(0.8,0.2))
training.data<-fleetdna_interest_vocations[index==1,]
testing.data<-fleetdna_interest_vocations[index==2,]

# lasso regression

library(glmnet)
X <- model.matrix(voc_id~.,data=training.data)[,-1]
Y <- training.data$voc_id
lasso.best <- cv.glmnet(x=X,y=as.factor(Y),alpha=1,family="multinomial",type.multinomial = "grouped", type.measure ="mse",maxit = 1000000)
plot(lasso.best)
plot(lasso.best$glmnet.fit,xvar='lambda',label=TRUE)

coef(lasso.best, s = "lambda.min")



# creating formula for the vocation model
all.names<- names(fleetdna_interest_vocations)
#unwanted_variables<-c("voc_id","total_kinetic_power_density_demand","average_aerodynamic_power_density_demand","characteristic_deceleration " )
unwanted_variables<-c("voc_id","max_speed" )
required_variables<-all.names[!all.names%in%unwanted_variables]
x_variables<- paste(required_variables,sep = "",collapse = "+")
formula_model<-formula(paste("voc_id~",x_variables),collapse="")

# multinomial regression model

library(nnet)
mnl.fit <- multinom(formula_model,data=training.data,maxit=5000000)

# accuracy of the model in the prediction of drivetrain type

voc_id_predicted<-predict(mnl.fit,newdata = testing.data)
tab2<-table(voc_id_predicted,testing.data$voc_id) # confusion matrix
tab2
accuracy_mln<-sum(diag(tab2))/sum(tab2)
accuracy_mln


# only trip count is consiered 

library(effects)
mnl.eff <- Effect('trip_count',mnl.fit) 
mnl.eff

library(reshape)
library(ggplot2)
transformEffectForPlot <- function(data) {
  output <- melt(data$prob)
  names(output) <- c('frequency','preference','probability')
  output$preference <- gsub('\\.',' ',output$preference)
  output$preference <- gsub('prob ','',output$preference)
  output$u.probability <- melt(data$upper.prob)[,3]
  output$l.probability <- melt(data$lower.prob)[,3]
  return(output)
}
max(fleetdna_interest_vocations$trip_count)
min(fleetdna_interest_vocations$trip_count)
mnl.eff.forPlot <- transformEffectForPlot(mnl.eff)
# min(fleetdna_compositedata_mod$trip_count)
key <- data.table(frequency=1:6,trip_count=c(0,5,10,15,20,25))  # axis interval defined
mnl.eff.forPlot <- merge(x=mnl.eff.forPlot,y=key,by='frequency')

ggplot(mnl.eff.forPlot,aes(x=trip_count,colour=preference,fill=preference))+
  geom_line(aes(y=probability))+
  geom_ribbon(aes(ymin=l.probability,ymax=u.probability,linetype=NA),alpha=.15)+
  xlab('trip_count')+
  ylab('Probability')+
  theme_bw()+
  scale_color_brewer(name='voc_id',palette='Set1')+
  scale_fill_brewer(name='voc_id',palette='Set1')+
  guides(col=guide_legend(nrow=1,bycol=TRUE,title.position='top'))+
  theme(legend.position='bottom',legend.text=element_text(size=8))




# only number of stops is consiered is considered.

library(effects)
mnl.eff <- Effect("total_stops",mnl.fit) 
mnl.eff

library(reshape)
library(ggplot2)
transformEffectForPlot <- function(data) {
  output <- melt(data$prob)
  names(output) <- c('frequency','preference','probability')
  output$preference <- gsub('\\.',' ',output$preference)
  output$preference <- gsub('prob ','',output$preference)
  output$u.probability <- melt(data$upper.prob)[,3]
  output$l.probability <- melt(data$lower.prob)[,3]
  return(output)
}
max(fleetdna_interest_vocations$total_stops)     # finding the maximum value
min(fleetdna_interest_vocations$total_stops)
mnl.eff.forPlot <- transformEffectForPlot(mnl.eff)
key <- data.table(frequency=1:5,total_stops=c(0,200,400,600,800))  # axis interval defined
mnl.eff.forPlot <- merge(x=mnl.eff.forPlot,y=key,by='frequency')

ggplot(mnl.eff.forPlot,aes(x=total_stops,colour=preference,fill=preference))+
  geom_line(aes(y=probability))+
  geom_ribbon(aes(ymin=l.probability,ymax=u.probability,linetype=NA),alpha=.15)+
  xlab('total_stops')+
  ylab('Probability')+
  theme_bw()+
  scale_color_brewer(name='voc_id',palette='Set1')+
  scale_fill_brewer(name='voc_id',palette='Set1')+
  guides(col=guide_legend(nrow=1,bycol=TRUE,title.position='top'))+
  theme(legend.position='bottom',legend.text=element_text(size=8))



# only max road grade is consiered is considered.

library(effects)
mnl.eff <- Effect("max_road_grade",mnl.fit) 
mnl.eff

library(reshape)
library(ggplot2)
transformEffectForPlot <- function(data) {
  output <- melt(data$prob)
  names(output) <- c('frequency','preference','probability')
  output$preference <- gsub('\\.',' ',output$preference)
  output$preference <- gsub('prob ','',output$preference)
  output$u.probability <- melt(data$upper.prob)[,3]
  output$l.probability <- melt(data$lower.prob)[,3]
  return(output)
}
#max(fleetdna_interest_vocations$max_road_grade)      # finding the maximum value
mnl.eff.forPlot <- transformEffectForPlot(mnl.eff)
key <- data.table(frequency=1:5,max_road_grade=c(0,0.1,0.2,0.3,0.4))  # axis interval defined
mnl.eff.forPlot <- merge(x=mnl.eff.forPlot,y=key,by='frequency')

ggplot(mnl.eff.forPlot,aes(x=max_road_grade,colour=preference,fill=preference))+
  geom_line(aes(y=probability))+
  geom_ribbon(aes(ymin=l.probability,ymax=u.probability,linetype=NA),alpha=.15)+
  xlab('max_road_grade')+
  ylab('Probability')+
  theme_bw()+
  scale_color_brewer(name='voc_id',palette='Set1')+
  scale_fill_brewer(name='voc_id',palette='Set1')+
  guides(col=guide_legend(nrow=1,bycol=TRUE,title.position='top'))+
  theme(legend.position='bottom',legend.text=element_text(size=8))




# only acceleration_events_per_mile is consiered is considered.

library(effects)
mnl.eff <- Effect("acceleration_events_per_mile",mnl.fit) 
mnl.eff

library(reshape)
library(ggplot2)
transformEffectForPlot <- function(data) {
  output <- melt(data$prob)
  names(output) <- c('frequency','preference','probability')
  output$preference <- gsub('\\.',' ',output$preference)
  output$preference <- gsub('prob ','',output$preference)
  output$u.probability <- melt(data$upper.prob)[,3]
  output$l.probability <- melt(data$lower.prob)[,3]
  return(output)
}
max(fleetdna_interest_vocations$acceleration_events_per_mile)       # finding the maximum value
mnl.eff.forPlot <- transformEffectForPlot(mnl.eff)
key <- data.table(frequency=1:7,acceleration_events_per_mile=c(0,5,10,15,20,25,30))  # axis interval defined
mnl.eff.forPlot <- merge(x=mnl.eff.forPlot,y=key,by='frequency')

ggplot(mnl.eff.forPlot,aes(x=acceleration_events_per_mile,colour=preference,fill=preference))+
  geom_line(aes(y=probability))+
  geom_ribbon(aes(ymin=l.probability,ymax=u.probability,linetype=NA),alpha=.15)+
  xlab('acceleration_events_per_mile')+
  ylab('Probability')+
  theme_bw()+
  scale_color_brewer(name='voc_id',palette='Set1')+
  scale_fill_brewer(name='voc_id',palette='Set1')+
  guides(col=guide_legend(nrow=1,bycol=TRUE,title.position='top'))+
  theme(legend.position='bottom',legend.text=element_text(size=8))



# based on average driving speeds

library(effects)
mnl.eff <- Effect("driving_average_speed",mnl.fit) 
mnl.eff

library(reshape)
library(ggplot2)
transformEffectForPlot <- function(data) {
  output <- melt(data$prob)
  names(output) <- c('frequency','preference','probability')
  output$preference <- gsub('\\.',' ',output$preference)
  output$preference <- gsub('prob ','',output$preference)
  output$u.probability <- melt(data$upper.prob)[,3]
  output$l.probability <- melt(data$lower.prob)[,3]
  return(output)
}
#min(fleetdna_compositedata_mod$driving_average_speed)       # finding the maximum value
mnl.eff.forPlot <- transformEffectForPlot(mnl.eff)
key <- data.table(frequency=1:5,driving_average_speed=c(10,20,30,40,50,60))  # axis interval defined
mnl.eff.forPlot <- merge(x=mnl.eff.forPlot,y=key,by='frequency')

ggplot(mnl.eff.forPlot,aes(x=driving_average_speed,colour=preference,fill=preference))+
  geom_line(aes(y=probability))+
  geom_ribbon(aes(ymin=l.probability,ymax=u.probability,linetype=NA),alpha=.15)+
  xlab('driving_average_speed')+
  ylab('Probability')+
  theme_bw()+
  scale_color_brewer(name='voc_id',palette='Set1')+
  scale_fill_brewer(name='voc_id',palette='Set1')+
  guides(col=guide_legend(nrow=1,bycol=TRUE,title.position='top'))+
  theme(legend.position='bottom',legend.text=element_text(size=8))


# based on distance travelled


library(effects)
mnl.eff <- Effect("distance_total",mnl.fit) 
mnl.eff

library(reshape)
library(ggplot2)
transformEffectForPlot <- function(data) {
  output <- melt(data$prob)
  names(output) <- c('frequency','preference','probability')
  output$preference <- gsub('\\.',' ',output$preference)
  output$preference <- gsub('prob ','',output$preference)
  output$u.probability <- melt(data$upper.prob)[,3]
  output$l.probability <- melt(data$lower.prob)[,3]
  return(output)
}
#min(fleetdna_compositedata_mod$distance_total)       # finding the maximum value
mnl.eff.forPlot <- transformEffectForPlot(mnl.eff)
key <- data.table(frequency=1:5,distance_total=c(5,100,200,300,450))  # axis interval defined
mnl.eff.forPlot <- merge(x=mnl.eff.forPlot,y=key,by='frequency')

ggplot(mnl.eff.forPlot,aes(x=distance_total,colour=preference,fill=preference))+
  geom_line(aes(y=probability))+
  geom_ribbon(aes(ymin=l.probability,ymax=u.probability,linetype=NA),alpha=.15)+
  xlab('distance_total')+
  ylab('Probability')+
  theme_bw()+
  scale_color_brewer(name='voc_id',palette='Set1')+
  scale_fill_brewer(name='voc_id',palette='Set1')+
  guides(col=guide_legend(nrow=1,bycol=TRUE,title.position='top'))+
  theme(legend.position='bottom',legend.text=element_text(size=8))



# based on kinetic intensity


library(effects)
mnl.eff <- Effect("kinetic_intensity",mnl.fit) 
mnl.eff

library(reshape)
library(ggplot2)
transformEffectForPlot <- function(data) {
  output <- melt(data$prob)
  names(output) <- c('frequency','preference','probability')
  output$preference <- gsub('\\.',' ',output$preference)
  output$preference <- gsub('prob ','',output$preference)
  output$u.probability <- melt(data$upper.prob)[,3]
  output$l.probability <- melt(data$lower.prob)[,3]
  return(output)
}
#max(fleetdna_compositedata_mod$kinetic_intensity)       # finding the maximum value
mnl.eff.forPlot <- transformEffectForPlot(mnl.eff)
key <- data.table(frequency=1:7,kinetic_intensity=c(0,2,4,6,8,10,12))  # axis interval defined
mnl.eff.forPlot <- merge(x=mnl.eff.forPlot,y=key,by='frequency')

ggplot(mnl.eff.forPlot,aes(x=kinetic_intensity,colour=preference,fill=preference))+
  geom_line(aes(y=probability))+
  geom_ribbon(aes(ymin=l.probability,ymax=u.probability,linetype=NA),alpha=.15)+
  xlab('kinetic_intensity')+
  ylab('Probability')+
  theme_bw()+
  scale_color_brewer(name='voc_id',palette='Set1')+
  scale_fill_brewer(name='voc_id',palette='Set1')+
  guides(col=guide_legend(nrow=1,bycol=TRUE,title.position='top'))+
  theme(legend.position='bottom',legend.text=element_text(size=8))


# based on aerodynamic speed




library(effects)
mnl.eff <- Effect("aerodynamic_speed",mnl.fit) 
mnl.eff

library(reshape)
library(ggplot2)
transformEffectForPlot <- function(data) {
  output <- melt(data$prob)
  names(output) <- c('frequency','preference','probability')
  output$preference <- gsub('\\.',' ',output$preference)
  output$preference <- gsub('prob ','',output$preference)
  output$u.probability <- melt(data$upper.prob)[,3]
  output$l.probability <- melt(data$lower.prob)[,3]
  return(output)
}
# max(fleetdna_compositedata_mod$aerodynamic_speed)       # finding the maximum value
mnl.eff.forPlot <- transformEffectForPlot(mnl.eff)
key <- data.table(frequency=1:6,aerodynamic_speed=c(0,5,10,15,20,25))  # axis interval defined
mnl.eff.forPlot <- merge(x=mnl.eff.forPlot,y=key,by='frequency')

ggplot(mnl.eff.forPlot,aes(x=aerodynamic_speed,colour=preference,fill=preference))+
  geom_line(aes(y=probability))+
  geom_ribbon(aes(ymin=l.probability,ymax=u.probability,linetype=NA),alpha=.15)+
  xlab('aerodynamic_speed')+
  ylab('Probability')+
  theme_bw()+
  scale_color_brewer(name='voc_id',palette='Set1')+
  scale_fill_brewer(name='voc_id',palette='Set1')+
  guides(col=guide_legend(nrow=1,bycol=TRUE,title.position='top'))+
  theme(legend.position='bottom',legend.text=element_text(size=8))











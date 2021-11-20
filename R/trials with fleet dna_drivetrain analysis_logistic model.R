
# data analysis for drive train
library(data.table)
fleetdna_compositedata<-fread("data_for_fleet_dna_composite_data.csv",stringsAsFactors = FALSE)


 names(fleetdna_compositedata)



fleetdna_compositedata_mod<-fleetdna_compositedata[,c("drive_id","trip_count","total_average_speed","max_speed","driving_average_speed","distance_total","acceleration_events_per_mile","deceleration_events_per_mile","total_stops" ,"average_stop_duration","max_elevation","min_elevation","max_climbing_rate" ,"average_climbing_rate","max_descending_rate","average_descending_rate","max_road_grade","mean_road_grade","maximum_kinetic_power_density_demand","total_kinetic_power_density_demand","average_kinetic_power_density_demand","maximum_potential_power_density_demand","total_potential_power_density_demand","average_potential_power_density_demand" ,"total_aerodynamic_power_density_demand","aerodynamic_speed","kinetic_intensity","average_aerodynamic_power_density_demand","maximum_rolling_power_density_demand","total_rolling_power_density_demand","average_rolling_power_density_demand","characteristic_acceleration","characteristic_deceleration","maximum_kinetic_power_density_regen","total_kinetic_power_density_regen","average_kinetic_power_density_regen","maximum_potential_power_density_regen","total_potential_power_density_regen","average_potential_power_density_regen" )]

# converting int varibales into factor levels
fleetdna_compositedata_mod$drive_id<-as.factor(fleetdna_compositedata_mod$drive_id)
# fleetdna_compositedata$class_id<-as.factor(fleetdna_compositedata$class_id)
# fleetdna_compositedata$voc_id<-as.factor(fleetdna_compositedata$voc_id)
# fleetdna_compositedata$fuel_id<-as.factor(fleetdna_compositedata$fuel_id)


fleetdna_compositedata_mod<-fleetdna_compositedata_mod[complete.cases(fleetdna_compositedata_mod),]

# summary of the data table
str(fleetdna_compositedata_mod) 

# fitting a linear model with all variables

# linear_model_driveid<-lm(drive_id~.,data = fleetdna_compositedata)
# 
# driveid_fitted<-fitted(linear_model_driveid)
# plot(driveid_fitted,resid(linear_model_driveid))   # residual plot
# summary(linear_model_driveid)

# creating a training and testing data set
set.seed(100)
index<-sample(1:2,nrow(fleetdna_compositedata_mod),replace = TRUE,prob = c(0.8,0.2))
training.data<-fleetdna_compositedata_mod[index==1,]
testing.data<-fleetdna_compositedata_mod[index==2,]



# selection of variables for the model

# Lasso regression
library(glmnet)
X <- model.matrix(drive_id~.,data=training.data)[,-1]
Y <- training.data$drive_id
lasso.best <- cv.glmnet(x=X,y=as.factor(Y),alpha=1,family="multinomial",type.multinomial = "grouped", type.measure ="mse",maxit = 1000000)
plot(lasso.best)
plot(lasso.best$glmnet.fit,xvar='lambda',label=TRUE)

coef(lasso.best, s = "lambda.min")  # getting the coefficients for each outcome

# predict(lasso.best, newx = X[1:10,], s = "lambda.min", type = "class") # predicting the outcome
#  lasso.auto <- glmnet(x=X,y=as.factor(Y),alpha=1,lambda=lasso.best$lambda.min)
# lasso.auto$beta
# 
# # stepwise regression with AIC-BIC criterion
# library(MASS)
# 
#  model1<-lm(drive_id~.,data = fleetdna_compositedata_mod)
# # 
# model.aic <- stepAIC(model1,direction='both') # doing both forward  stepwise regression
#  model.bic <- stepAIC(model1,direction='both',k=log(nrow(fleetdna_compositedata_mod)))
# 
# 








# creating formula for the model
all.names<- names(fleetdna_compositedata_mod)
unwanted_variables<-c("drive_id","max_speed","average_descending_rate","total_kinetic_power_density_demand","average_potential_power_density_demand","average_aerodynamic_power_density_demand","total_rolling_power_density_demand","total_potential_power_density_regen" )
required_variables<-all.names[!all.names%in%unwanted_variables]
x_variables<- paste(required_variables,sep = "",collapse = "+")
formula_model<-formula(paste("drive_id~",x_variables),collapse="")

# multinomial regression model

library(nnet)
mnl.fit <- multinom(formula_model,data=training.data,maxit=5000)

# accuracy of the model in the prediction of drivetrain type

drive_id_predicted<-predict(mnl.fit,newdata = testing.data)
tab2<-table(drive_id_predicted,testing.data$drive_id) # confusion matrix
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
#max(fleetdna_compositedata_mod$trip_count)
mnl.eff.forPlot <- transformEffectForPlot(mnl.eff)
min(fleetdna_compositedata_mod$trip_count)
key <- data.table(frequency=1:5,trip_count=c(0,10,20,30,40))  # axis interval defined
mnl.eff.forPlot <- merge(x=mnl.eff.forPlot,y=key,by='frequency')

ggplot(mnl.eff.forPlot,aes(x=trip_count,colour=preference,fill=preference))+
  geom_line(aes(y=probability))+
  geom_ribbon(aes(ymin=l.probability,ymax=u.probability,linetype=NA),alpha=.15)+
  xlab('trip_count')+
  ylab('Probability')+
  theme_bw()+
  scale_color_brewer(name='drive_id',palette='Set1')+
  scale_fill_brewer(name='drive_id',palette='Set1')+
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
#min(fleetdna_compositedata_mod$total_stops)       # finding the maximum value
mnl.eff.forPlot <- transformEffectForPlot(mnl.eff)
key <- data.table(frequency=1:10,total_stops=c(0,100,200,300,400,500,600,700,800,825))  # axis interval defined
mnl.eff.forPlot <- merge(x=mnl.eff.forPlot,y=key,by='frequency')

ggplot(mnl.eff.forPlot,aes(x=total_stops,colour=preference,fill=preference))+
  geom_line(aes(y=probability))+
  geom_ribbon(aes(ymin=l.probability,ymax=u.probability,linetype=NA),alpha=.15)+
  xlab('total_stops')+
  ylab('Probability')+
  theme_bw()+
  scale_color_brewer(name='drive_id',palette='Set1')+
  scale_fill_brewer(name='drive_id',palette='Set1')+
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
#min(fleetdna_compositedata_mod$max_road_grade)       # finding the maximum value
mnl.eff.forPlot <- transformEffectForPlot(mnl.eff)
key <- data.table(frequency=1:5,max_road_grade=c(0,0.1,0.2,0.3,0.4))  # axis interval defined
mnl.eff.forPlot <- merge(x=mnl.eff.forPlot,y=key,by='frequency')

ggplot(mnl.eff.forPlot,aes(x=max_road_grade,colour=preference,fill=preference))+
  geom_line(aes(y=probability))+
  geom_ribbon(aes(ymin=l.probability,ymax=u.probability,linetype=NA),alpha=.15)+
  xlab('max_road_grade')+
  ylab('Probability')+
  theme_bw()+
  scale_color_brewer(name='drive_id',palette='Set1')+
  scale_fill_brewer(name='drive_id',palette='Set1')+
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
#min(fleetdna_compositedata_mod$acceleration_events_per_mile)       # finding the maximum value
mnl.eff.forPlot <- transformEffectForPlot(mnl.eff)
key <- data.table(frequency=1:7,acceleration_events_per_mile=c(0,5,10,15,20,25,30))  # axis interval defined
mnl.eff.forPlot <- merge(x=mnl.eff.forPlot,y=key,by='frequency')

ggplot(mnl.eff.forPlot,aes(x=acceleration_events_per_mile,colour=preference,fill=preference))+
  geom_line(aes(y=probability))+
  geom_ribbon(aes(ymin=l.probability,ymax=u.probability,linetype=NA),alpha=.15)+
  xlab('acceleration_events_per_mile')+
  ylab('Probability')+
  theme_bw()+
  scale_color_brewer(name='drive_id',palette='Set1')+
  scale_fill_brewer(name='drive_id',palette='Set1')+
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
  scale_color_brewer(name='drive_id',palette='Set1')+
  scale_fill_brewer(name='drive_id',palette='Set1')+
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
key <- data.table(frequency=1:5,distance_total=c(0,100,200,300,400,500))  # axis interval defined
mnl.eff.forPlot <- merge(x=mnl.eff.forPlot,y=key,by='frequency')

ggplot(mnl.eff.forPlot,aes(x=distance_total,colour=preference,fill=preference))+
  geom_line(aes(y=probability))+
  geom_ribbon(aes(ymin=l.probability,ymax=u.probability,linetype=NA),alpha=.15)+
  xlab('distance_total')+
  ylab('Probability')+
  theme_bw()+
  scale_color_brewer(name='drive_id',palette='Set1')+
  scale_fill_brewer(name='drive_id',palette='Set1')+
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
  scale_color_brewer(name='drive_id',palette='Set1')+
  scale_fill_brewer(name='drive_id',palette='Set1')+
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
  scale_color_brewer(name='drive_id',palette='Set1')+
  scale_fill_brewer(name='drive_id',palette='Set1')+
  guides(col=guide_legend(nrow=1,bycol=TRUE,title.position='top'))+
  theme(legend.position='bottom',legend.text=element_text(size=8))











# combination probabality for drivetrains
# fleetdna_compositedata_mod_new<-expand.grid(distance_total=seq(from=5,to=569,by=5),total_stops=seq(from=2,to=830,by=25))
# 
# fleetdna_compositedata_mod_new$drive_id<-predict(mnl.fit,newdata = fleetdna_compositedata_mod_new)
# 
library(ggplot2)
ggplot()+
  geom_point(data=testing.data,aes(x=distance_total,y=total_stops,color=drive_id_predicted),alpha=2.5)+
  geom_point(data=training.data,aes(x=distance_total,y=total_stops,fill=drive_id),colour='black',pch=21)+
  xlab('distance_total')+
  ylab('total_stops')+
  theme_bw()



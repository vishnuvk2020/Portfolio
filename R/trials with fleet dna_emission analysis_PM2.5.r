# PM 2.5 analysis


library(data.table)
fleetdna_compositedata<-fread("data_for_fleet_dna_composite_data.csv",stringsAsFactors = FALSE)

fleetdna_compositedata_mod<-fleetdna_compositedata[,c("fuel_id","voc_id","class_id","drive_id","trip_count","total_average_speed","max_speed","driving_average_speed","distance_total","acceleration_events_per_mile","deceleration_events_per_mile","total_stops" ,"average_stop_duration","max_elevation","min_elevation","max_climbing_rate" ,"average_climbing_rate","max_descending_rate","average_descending_rate","max_road_grade","mean_road_grade","maximum_kinetic_power_density_demand","total_kinetic_power_density_demand","average_kinetic_power_density_demand","maximum_potential_power_density_demand","total_potential_power_density_demand","average_potential_power_density_demand" ,"total_aerodynamic_power_density_demand","aerodynamic_speed","kinetic_intensity","average_aerodynamic_power_density_demand","maximum_rolling_power_density_demand","total_rolling_power_density_demand","average_rolling_power_density_demand","characteristic_acceleration","characteristic_deceleration","maximum_kinetic_power_density_regen","total_kinetic_power_density_regen","average_kinetic_power_density_regen","maximum_potential_power_density_regen","total_potential_power_density_regen","average_potential_power_density_regen" )]
names(fleetdna_compositedata)

fleetdna_compositedata_mod<-fleetdna_compositedata_mod[complete.cases(fleetdna_compositedata_mod),]

fleetdna_compositedata_mod[,NOX:=numeric()]
fleetdna_compositedata_mod[,CO:=numeric()]
fleetdna_compositedata_mod[,PM2.5:=numeric()]
fleetdna_compositedata_mod[,PM10:=numeric()]
# adding NOX,PM and CO values in to the data set

for( i in 1:nrow(fleetdna_compositedata_mod)){
  
  # gasoline class 2
  if( fleetdna_compositedata_mod$fuel_id[i] == 0 & fleetdna_compositedata_mod$class_id[i]==2)
  {
    
    fleetdna_compositedata_mod[i,NOX:=(2.734*distance_total)]
    fleetdna_compositedata_mod[i,CO:=(11.220*distance_total)]
    fleetdna_compositedata_mod[i,PM2.5:=(0.043*distance_total)]
    fleetdna_compositedata_mod[i,PM10:=(0.049*distance_total)]
  }
  
  # gasoline class 3
  if( fleetdna_compositedata_mod$fuel_id[i] == 0 & fleetdna_compositedata_mod$class_id[i]==3)
  {
    
    fleetdna_compositedata_mod[i,CO:=(15.810*distance_total)]
    fleetdna_compositedata_mod[i,NOX:=(2.920*distance_total)]
    fleetdna_compositedata_mod[i,PM2.5:=(0.045*distance_total)]
    fleetdna_compositedata_mod[i,PM10:=(0.051*distance_total)]
  }
  # gasoline class 4
  if( fleetdna_compositedata_mod$fuel_id[i] == 0 & fleetdna_compositedata_mod$class_id[i]==4)
  {
    
    fleetdna_compositedata_mod[i,CO:=(33.860*distance_total)]
    fleetdna_compositedata_mod[i,NOX:=(4.133*distance_total)]
    fleetdna_compositedata_mod[i,PM2.5:=(0.058*distance_total)]
    fleetdna_compositedata_mod[i,PM10:=(0.074*distance_total)]
  }
  # gasoline class 5
  if( fleetdna_compositedata_mod$fuel_id[i] == 0 & fleetdna_compositedata_mod$class_id[i]==5)
  {
    
    fleetdna_compositedata_mod[i,CO:=(19.580*distance_total)]
    fleetdna_compositedata_mod[i,NOX:=(3.735*distance_total)]
    fleetdna_compositedata_mod[i,PM2.5:=(0.046*distance_total)]
    fleetdna_compositedata_mod[i,PM10:=(0.055*distance_total)]
  }
  
  # gasoline class 6
  if( fleetdna_compositedata_mod$fuel_id[i] == 0 & fleetdna_compositedata_mod$class_id[i]==6)
  {
    
    fleetdna_compositedata_mod[i,CO:=(18.130*distance_total)]
    fleetdna_compositedata_mod[i,NOX:=(3.650*distance_total)]
    fleetdna_compositedata_mod[i,PM2.5:=(0.045*distance_total)]
    fleetdna_compositedata_mod[i,PM10:=(0.054*distance_total)]
    
  }
  
  # gasoline class 7
  if( fleetdna_compositedata_mod$fuel_id[i] == 0 & fleetdna_compositedata_mod$class_id[i]==7)
  {
    
    fleetdna_compositedata_mod[i,CO:=(23.130*distance_total)]
    fleetdna_compositedata_mod[i,NOX:=(4.199*distance_total)]
    fleetdna_compositedata_mod[i,PM2.5:=(0.046*distance_total)]
    fleetdna_compositedata_mod[i,PM10:=(0.056*distance_total)]
    
  }
  # Diesel class 2
  
  if( fleetdna_compositedata_mod$fuel_id[i] == 1 & fleetdna_compositedata_mod$class_id[i]==2)
  {
    
    fleetdna_compositedata_mod[i,CO:=(0.839*distance_total)]
    fleetdna_compositedata_mod[i,NOX:=(3.088*distance_total)]
    fleetdna_compositedata_mod[i,PM2.5:=(0.091*distance_total)]
    fleetdna_compositedata_mod[i,PM10:=(0.099*distance_total)]
    
  }
  # Diesel class 3
  
  if( fleetdna_compositedata_mod$fuel_id[i] == 1 & fleetdna_compositedata_mod$class_id[i]==3)
  {
    
    fleetdna_compositedata_mod[i,CO:=(0.908*distance_total)]
    fleetdna_compositedata_mod[i,NOX:=(3.298*distance_total)]
    fleetdna_compositedata_mod[i,PM2.5:=(0.073*distance_total)]
    fleetdna_compositedata_mod[i,PM10:=(0.079*distance_total)]
    
  }
  # Diesel class 4
  
  if( fleetdna_compositedata_mod$fuel_id[i] == 1 & fleetdna_compositedata_mod$class_id[i]==4)
  {
    
    fleetdna_compositedata_mod[i,CO:=(1.163*distance_total)]
    fleetdna_compositedata_mod[i,NOX:=(4.352*distance_total)]
    fleetdna_compositedata_mod[i,PM2.5:=(0.089*distance_total)]
    fleetdna_compositedata_mod[i,PM10:=(0.096*distance_total)]
    
  }
  # Diesel class 5
  
  if( fleetdna_compositedata_mod$fuel_id[i] == 1 & fleetdna_compositedata_mod$class_id[i]==5)
  {
    
    fleetdna_compositedata_mod[i,CO:=(1.189*distance_total)]
    fleetdna_compositedata_mod[i,NOX:=(4.548*distance_total)]
    fleetdna_compositedata_mod[i,PM2.5:=(0.079*distance_total)]
    fleetdna_compositedata_mod[i,PM10:=(0.085*distance_total)]
    
  }
  
  # Diesel class 6
  
  if( fleetdna_compositedata_mod$fuel_id[i] == 1 & fleetdna_compositedata_mod$class_id[i]==6)
  {
    
    fleetdna_compositedata_mod[i,CO:=(1.367*distance_total)]
    fleetdna_compositedata_mod[i,NOX:=(5.990*distance_total)]
    fleetdna_compositedata_mod[i,PM2.5:=(0.172*distance_total)]
    fleetdna_compositedata_mod[i,PM10:=(0.186*distance_total)]
    
  }
  
  # Diesel class 7
  
  if( fleetdna_compositedata_mod$fuel_id[i] == 1 & fleetdna_compositedata_mod$class_id[i]==7)
  {
    
    fleetdna_compositedata_mod[i,CO:=(1.719*distance_total)]
    fleetdna_compositedata_mod[i,NOX:=(7.471*distance_total)]
    fleetdna_compositedata_mod[i,PM2.5:=(0.177*distance_total)]
    fleetdna_compositedata_mod[i,PM10:=(0.192*distance_total)]
    
  }
  
  
}
final_data<-fleetdna_compositedata_mod[complete.cases(fleetdna_compositedata_mod),]
final_data$fuel_id<-as.factor(final_data$fuel_id)
final_data$voc_id<-as.factor(final_data$voc_id)
final_data$class_id<-as.factor(final_data$class_id)
final_data$drive_id<-as.factor(final_data$drive_id)
str(final_data)

#deleting distance,co,NOX and pm 10 from the data base 

final_data_pm2.5<-final_data[,c(1:8,10:42,45)]       

# splitting data into training and testing data
set.seed(1000) 
index<-sample(2,nrow(final_data_pm2.5),replace = TRUE,prob=c(0.8,0.2))
training_data_pm2.5<-final_data_pm2.5[index==1,]
testing_data_pm2.5<-final_data_pm2.5[index==2,]

# variable identification using lasso regression

library(glmnet)

X <- model.matrix( PM2.5~.,data=training_data_pm2.5)[,-1]
Y <- training_data_pm2.5$PM2.5
lasso.best <- cv.glmnet(x=X,y=Y,alpha=1)
plot(lasso.best)
plot(lasso.best$glmnet.fit,xvar='lambda',label=TRUE)

lasso.auto <- glmnet(x=X,y=Y,alpha=1,lambda=lasso.best$lambda.min)
lasso.auto$beta


# creating formula for the model
all.names<- names(training_data_pm2.5)
unwanted_variables<-c("characteristic_acceleration","fuel_id","drive_id","voc_id","class_id","max_speed","driving_average_speed","deceleration_events_per_mile","maximum_rolling_power_density_demand","average_rolling_power_density_demand","total_stops","max_climbing_rate","max_road_grade","total_potential_power_density_regen","PM2.5")
required_variables<-all.names[!all.names%in%unwanted_variables]
x_variables<- paste("s(",required_variables,")",collapse = "+")
formula_pm2.5<-formula(paste("PM2.5~drive_id+fuel_id+voc_id+class_id+",x_variables))


# GAM model for PM2.5

library(mgcv) 
gam.fit_pm2.5 <- gam(formula = formula_pm2.5,data = training_data_pm2.5) 
summary(gam.fit_pm2.5)
plot(gam.fit_pm2.5,se=TRUE,pages=10)
predicted_pm2.5_testingdata<-predict(gam.fit_pm2.5,newdata = testing_data_pm2.5)
predicted_pm2.5_traiingdata<-predict(gam.fit_pm2.5,data=training_data_pm2.5)
MSE_insample<- mean((predicted_pm2.5_traiingdata-training_data_pm2.5$PM2.5)^2)
MSE_outsample<-mean((predicted_pm2.5_testingdata-testing_data_pm2.5$PM2.5)^2)
cat("out of sample mean square error for pm2.5 GAM model:",MSE_outsample)
cat("IN sample mean square error for pm2.5 GAM model:",MSE_insample)

# Modified GAM model based on analysing the plots

spline_variables<-c("maximum_potential_power_density_regen","average_kinetic_power_density_regen","maximum_kinetic_power_density_regen","average_aerodynamic_power_density_demand ","kinetic_intensity","maximum_potential_power_density_demand","maximum_kinetic_power_density_demand","total_average_speed","average_potential_power_density_regen","total_kinetic_power_density_regen","total_aerodynamic_power_density_demand" ,"aerodynamic_speed","min_elevation","max_elevation")
linear_variables<-required_variables[!required_variables%in%spline_variables]
x_variables_spline<- paste("s(",spline_variables,")",collapse = "+")
x_variables_linear<-paste(linear_variables,collapse = "+")
formula_pm2.5_modgam<-formula(paste("PM2.5~drive_id+fuel_id+voc_id+class_id+",x_variables_linear,"+",x_variables_spline))

# fitting modified GAM for pm 2.5

library(mgcv) 
gam.fit_pm2.5_modified <- gam(formula = formula_pm2.5_modgam,data = training_data_pm2.5) 
summary(gam.fit_pm2.5_modified)
plot(gam.fit_pm2.5_modified,se=TRUE,pages=3)




predicted_pm2.5_testingdata_modgam<-predict(gam.fit_pm2.5_modified,newdata = testing_data_pm2.5)  # finding MSE
predicted_pm2.5_traiingdata_modgam<-predict(gam.fit_pm2.5_modified,data=training_data_pm2.5)
MSE_insample_modgam<- mean((predicted_pm2.5_traiingdata_modgam-training_data_pm2.5$PM2.5)^2)
MSE_outsample_modgam<-mean((predicted_pm2.5_testingdata_modgam-testing_data_pm2.5$PM2.5)^2)
cat("out of sample mean square error for modified pm2.5 GAM model:",MSE_outsample_modgam)
cat("IN sample mean square error for modified pm2.5 GAM model:",MSE_insample_modgam)


options(max.print=10000)
x<-fitted((gam.fit_pm2.5_modified ),data=training_data_pm2.5)
y<-resid(gam.fit_pm2.5_modified )
plot(x,y,xlab="Fitted values",ylab="Residual", main = "Residual plot for PM2.5: GAM")  # residual plot
abline(h=0,col="red")

# comparing two models using residual plots and anova table



gam.check(gam.fit_pm2.5)


gam.check(gam.fit_pm2.5_modified)

anova(gam.fit_pm2.5,gam.fit_pm2.5_modified,test="F")


# Building a linear model to compare GAM

linearmodel_variables_pm2.5<-paste(required_variables,collapse = "+")
formula_lm<-formula(paste("PM2.5~drive_id+fuel_id+voc_id+class_id+",linearmodel_variables_pm2.5))
linear_model_pm2.5<-lm(formula=formula_lm,data = training_data_pm2.5)
summary(linear_model_pm2.5)

predicted_pm2.5_testingdata_linear<-predict(linear_model_pm2.5,newdata = testing_data_pm2.5)            # evaluating in sampe and out of sample MSE
predicted_pm2.5_traiingdata_linear<-predict(linear_model_pm2.5,data=training_data_pm2.5)
MSE_insample_linear<- mean((predicted_pm2.5_traiingdata_linear-training_data_pm2.5$PM2.5)^2)
MSE_outsample_linear<-mean((predicted_pm2.5_testingdata_linear-testing_data_pm2.5$PM2.5)^2)
cat("out of sample mean square error for PM2.5 linear model:",MSE_outsample_linear)
cat("IN sample mean square error for PM2.5 linear model:",MSE_insample_linear)


options(max.print=10000)                                       # residual plots
x_linear<-fitted((linear_model_pm2.5),data=training_data_pm2.5)
y_linear<-resid(linear_model_pm2.5)
plot(x_linear,y_linear,xlab="Fitted values",ylab="Residual", main = "Residual plot for PM2.5: Linear model")  # residual plot
abline(h=0,col="red")







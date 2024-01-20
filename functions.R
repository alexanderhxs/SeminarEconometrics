library(vars)
library(bvarsv)
library(ggplot2)
library(reshape2)
#function for computing the connectedness over a roling window
ff = function(time, values, windowsize = 200, progess = 500){
  #'''
  #@return: data frame with the time stamp of the end7 of the rolling window and the
  #connectedness measure: correlation^2/K (K=dim(values))
  #'''
  
  #get size of data and initialize results
  K = ncol(values)
  n = nrow(values)
  results = data.frame(Time = time[(windowsize):length(time)], Connectedness = NaN)
  
  #iterate over date
  for (i in 1:(n-windowsize+1)){
    
    #compute var model and store connectedness measure in results
    var_model = VAR(values[i:(i+windowsize-1),], p = 1)
    correlation = cor(residuals(var_model))
    correlation_p2 = correlation * correlation
    
    #normaliza by dividing through row sum
    row_sums = rowSums(correlation_p2)
    correlation_p2 = sweep(correlation_p2, 1, row_sums, "/")
    results[i,2] = (sum(correlation_p2)-sum(diag(correlation_p2)))/K
    
    if((i %% progess) == 0){
      print(paste(Sys.time(), ' --- ' ,i, ' of ', n,' iterations'))
    }
  }
  return(results) 
}

#plot time series
plot_ts <- function(data, mark_shocks = FALSE){
  
  #get columns
  K = ncol(data)-1
  names = colnames(data)
  
  #design grid
  num_col <- ceiling(sqrt(K))
  num_row <- ceiling(K / num_col)
  par(mfrow = c(num_row, num_col))
  
  #get scale
  min = min(data[,-1])
  max = max(data[,-1])
  
  #plot time series
  for (i in 1:K) {
    plot(data[,1], data[,(i+1)], type = "l", col = i, xlab = "Time"
         , ylab = names[i+1], ylim = c(min,max))
  }
  
  par(mfrow = c(1, 1))
}


#plot multiple time series in one diagram
plot_ts2 = function(data, mark_shocks = FALSE){
  
  #set parameters
  K = ncol(data)
  names = colnames(data)
  
  #get scale
  min = min(data[,-1], na.rm = TRUE)
  max = max(data[,-1], na.rm = TRUE)
  print(paste(min,max))
  #plot base graph
  plot(data[,1], data[,2], type = 'l', col = 1 , xlab = 'Time', ylab = 'Correlation Measure', ylim = c(min,max))
  
  #add lines
  for (i in 3:K){
    line_data = na.omit(data[,c(1,i)])
    lines(line_data[,1], line_data[,2] , type = 'l', col = i-1, lw = 1)
  }
  if(mark_shocks){
    for (shock in shocks){
      rect(xleft = as.Date(shock[2]), xright = as.Date(shock[3]), ybottom = min,
           ytop = max, density = 5, col = rgb(0,0,1,1))
      text(x = as.Date(shock[2]), y = min, labels = shock[1], cex = .4)
    }
  }
  legend('topleft', legend = names[-1], text.col = seq(1,K,1), cex = .5)
}

gg = function(time, values, tau = 200){
  
  K = ncol(values)
  
  #fit model
  values = as.matrix(values)
  fit = bvar.sv.tvp(values, p = 1, nburn = 2000, nrep = 10000, tau = tau, itprint = 500)
  
  print(paste('length of output: ',dim(fit$H.postmean)[3]))
  print(paste('length of input: ',nrow(values)))
  
  #create result matrix
  col2 = rep(NaN,dim(fit$H.postmean)[3])
  results = data.frame(Time = time[(tau+2):length(time)], Connectedness = col2)
  colnames(results) = c('Time','Connectedness')
  
  #iterate ofer all time points
  for (t in 1:dim(fit$H.postmean)[3]){
    
    #calculate conectedness measure
    cor = cov2cor(fit$H.postmean[,,t])
    cor_p2 = cor * cor
    row_sums = rowSums(cor_p2)
    cor_p2 = sweep(cor_p2, 1, row_sums, "/")
    results[t,2] = (sum(cor_p2)-sum(diag(cor_p2)))/K
    
  }
  return(results)
}

#calculate directed connectedness
ff2 = function(time, values, var_index = 1, windowsize = 200, progess = 500){
  
  n = nrow(values)
  results = data.frame(Time = time[(windowsize):length(time)], from_others = NaN, to_others = NaN) 
  
  #iterate over date
  for (i in 1:(n-windowsize+1)){
    
    #compute var model and store connectedness measure in results
    var_model = VAR(values[i:(i+windowsize-1),], p = 1)
    correlation = cor(residuals(var_model))
    correlation_p2 = correlation * correlation
    
    #normaliza by dividing through row sum
    row_sums = rowSums(correlation_p2)
    correlation_p2 = sweep(correlation_p2, 1, row_sums, "/")
    results[i,2] = sum(correlation_p2[var_index,]) - correlation_p2[var_index,var_index]
    results[i,3] = sum(correlation_p2[,var_index]) - correlation_p2[var_index,var_index]
    
    if((i %% progess) == 0){
      print(paste(Sys.time(), ' --- ' ,i, ' of ', n,' iterations'))
    }
  }
  return(results) 
}


plot_ts_ggplot <- function(data, title) {

  long_data <- melt(data, id = 'Time')
  long_data = na.omit(long_data)
  
  ggplot(long_data, aes(x = as.Date(Time), y = value, colour = variable)) +
    geom_line(size = 0.1) +
    labs(title = title, x = 'Time', y ='Correlation measure') +
    geom_vline(xintercept = as.Date(c('2008-09-15','2000-03-31' ,'2022-02-24', '2020-02-20')), linetype = 'dashed', color = 'red') +
    annotate("text", x = as.Date("2022-02-24"), y = Inf, label = "Ukraine Invasion", vjust = 1, hjust = 0.5, size = 3) +
    annotate("text", x = as.Date("2008-09-15"), y = Inf, label = "Financial Crisis", vjust = 1, hjust = 0.5, size = 3) + 
    annotate("text", x = as.Date("2020-02-20"), y = -Inf, label = "Corona Crash", vjust = 0, hjust = 0.5, size = 3) + 
    annotate("text", x = as.Date("2000-03-31"), y = -Inf, label = "Dotcom Bubble", vjust = 0, hjust = 0.5, size = 3) +
    coord_cartesian(xlim = c(min(long_data$Time), max(long_data$Time))) +
    theme(plot.title = element_text(hjust = 0.5))
    
}

shocks = list(c('Financial Crisis (15. September)','2008-09-15','2008-09-15'),
               c('Ukraine Invasion','2022-02-24','2022-04-07'),
               c('Corona Crash','2020-02-20','2020-04-07'),
               c('Dotcom Bubble','2000-03-01','2000-03-31'))

# Beispielaufruf
# plot_zeitreihe(deine_daten)




CrossPrediction <- function (temperature, years, trend, kriging.classical, kriging.robust, file_prediction="", future=0) {
  prediction.trend <- data.frame("temperature"=c(temperature[(kObservationNum - 1):kObservationNum], trend[(kObservationNum + 1):src.nrows]),
    "year"=GetPredictionYears(years, src.nrows, future, kObservationNum))
  
  prediction.krigingClassical <- data.frame("temperature"=c(temperature[(kObservationNum - 1):kObservationNum], trend[(kObservationNum + 1):src.nrows] + kriging.classical$var1.pred),
    "year"=GetPredictionYears(years, src.nrows, future, kObservationNum))
  
  prediction.krigingRobust <- data.frame("temperature"=c(temperature[(kObservationNum - 1):kObservationNum], trend[(kObservationNum + 1):src.nrows] + kriging.robust$var1.pred),
    "year"=GetPredictionYears(years, src.nrows, future, kObservationNum))
  
  actual <- data.frame("temperature"=temperature[(kObservationNum - 1):src.nrows],
    "year"=GetPredictionYears(years, src.nrows, 0))
  
  if (nchar(file_prediction)) {
    plot.crossprediction <- ggplot() +
      geom_line(data=prediction.krigingClassical, aes(x=year, y=temperature, color="Прогноз КригингК")) + 
      geom_line(data=prediction.krigingRobust, aes(x=year, y=temperature, color="Прогноз КригингР")) + 
      geom_line(data=prediction.trend, aes(x=year, y=temperature, color="Прогноз Тренд")) +
      geom_line(data=actual, aes(x=year, y=temperature, colour="Актуальное")) +
      labs(color="") +
      scale_x_continuous(breaks=seq(min(actual$year), max(actual$year) + 5 + future, by=1)) + xlab("Год наблюдения") +
      scale_y_continuous(breaks=seq(16, 28, .5)) + ylab("Температура, С") +
      theme(axis.text.x = element_text(angle=45, hjust=1)) +
      labs(color="")
    ggsave(plot=plot.crossprediction, file=file_prediction, width=7, height=4)
  }
  
  prediction.krigingRobust$temperature[3:(src.nrows-kObservationNum)] - actual$temperature[3:(src.nrows - kObservationNum)] ## what the heck? why 3? 
}

output$param_comparison <- renderPlot({
  input$goPlot # Re-run when button is clicked
  
  # Create 0-row data frame which will be used to store data
  dat <- data.frame(manual=numeric(0), classical=numeric(0), robust=numeric(0))
  
  computePredictionResidualMSE <- function(data, trend, variog=ComputeVariogram, cressie, x, cutoff) {
    variogram <- variog(data, x=x, cressie=cressie, cutoff=cutoff)
    kriging <- PredictWithKriging(data, x=x, observations=observations(), variogram_model=variogram$var_model)
    residual <- CrossPrediction(src.data$temperature, src.data$year, trend, kriging)
    return(MSE(residual))
  }
  
  withProgress(message = 'Идет вычисление', value = 0, {
    # Number of times we'll go through the loop
    n <- maxRange()
    data <- residuals()$temperature
    trend <- trend()
    cutoffs <- 1:n
    
    for (cutoff in cutoffs) {
      manualResult    <- computePredictionResidualMSE(data=data, trend=trend, variog=ComputeManualVariogram, x=c(1:observations()), cressie=FALSE, cutoff=cutoff)
      classicalResult <- computePredictionResidualMSE(data=data, trend=trend, x=c(1:observations()), cressie=FALSE, cutoff=cutoff)
      robustResult    <- computePredictionResidualMSE(data=data, trend=trend, x=c(1:observations()), cressie=TRUE, cutoff=cutoff)
      
      # Each time through the loop, add another row of data. This is
      # a stand-in for a long-running computation.
      dat <- rbind(dat, data.frame(manual=manualResult, classical=classicalResult, robust=robustResult))
      
      # Increment the progress bar, and update the detail text.
      incProgress(1/n, detail = paste(trunc(cutoff / n * 100), "%"))
    }
  })
  
  ggplot() + 
    geom_line(data=data.frame("X"=cutoffs, "Y"=dat$manual), aes(x=X, y=Y, linetype="Фиксированная")) + 
    geom_line(data=data.frame("X"=cutoffs, "Y"=dat$classical), aes(x=X, y=Y, linetype="Классическая")) + 
    geom_line(data=data.frame("X"=cutoffs, "Y"=dat$robust), aes(x=X, y=Y, linetype="Робастная")) +
    scale_linetype_manual(name="Lines", values=c("Фиксированная"="solid", "Классическая"="dashed", "Робастная"="dashed")) +
    scale_x_continuous(breaks=cutoffs) +
    xlab("Максимальное расстояние") + ylab("MSE") +
    theme(axis.text.x = element_text(angle=90, hjust=1))
})
})
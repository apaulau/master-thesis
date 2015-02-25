CrossPrediction <- function (temperature, years, trend, kriging.classical, kriging.robust, file_prediction="", future=0) {
  prediction.trend <- data.frame("temperature"=c(temperature[(kObservationNum - 1):kObservationNum], trend[(kObservationNum + 1):src.nrows]),
    "year"=GetPredictionYears(years, src.nrows, future))
  
  prediction.krigingClassical <- data.frame("temperature"=c(temperature[(kObservationNum - 1):kObservationNum], trend[(kObservationNum + 1):src.nrows] + kriging.classical$var1.pred),
    "year"=GetPredictionYears(years, src.nrows, future))
  
  prediction.krigingRobust <- data.frame("temperature"=c(temperature[(kObservationNum - 1):kObservationNum], trend[(kObservationNum + 1):src.nrows] + kriging.robust$var1.pred),
    "year"=GetPredictionYears(years, src.nrows, future))
  
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
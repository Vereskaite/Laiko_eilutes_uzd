options(scipen = 9999)

data_8.7 <- read.table("m-gs1n3-5304.txt",header=F) 
names(data_8.7) <- c("Y1", "Y3", "year", "month", "day")
Data_8.7 <- data_8.7 %>% 
  mutate(date = paste0(year,"-",month,"-",day),
         date = as.Date(date, format = "%Y-%m-%d"),
         Y1 = log(Y1),
         Y3 = log(Y3)
  ) %>% 
  dplyr::select(Y1,Y3,date)

ts_Data_8.7 <- ts(Data_8.7[,1:2])
plot(ts_Data_8.7)

#In order to define lag order we use AIC/BIC
VARselect(ts_Data_8.7, lag.max = 6, type = "none")

#Pasirenkam Lag = 4

var_1 <- VAR(ts_Data_8.7,p=4)
summary(var_1)

acf(residuals(var_1))
#modelis geras

irf_results_Y1 <- irf(var_1, impulse = "Y1", response = c("Y1", "Y3"), n.ahead = 6)
plot(irf_results_Y1)

irf_results_Y3 <- irf(var_1, impulse = "Y3", response = c("Y1", "Y3"), n.ahead = 6)
plot(irf_results_Y3)

#impulse response function is persistent, 
#shocks from one treasury bill to another remain in all 6 observed lags
#increasee of 1 % in Y3 treasury interest compounds to around 0.1% in Y1 treasury bill.

forecast_horizon <- 12
forecast_results <- predict(var_1, n.ahead = forecast_horizon)

### reikia prideti 
# Create a data frame for the forecasted values and confidence intervals
nrow(Data_8.7) + forecast_horizon
forecast_data <- data.frame(
  Period = 1:(nrow(Data_8.7) + forecast_horizon),
  Actual = c(Data_8.7[,1], rep(NA, forecast_horizon)),
  Forecast = c(rep(NA, nrow(Data_8.7)), forecast_results$fcst$Y1[,1]),
  Lower_CI = c(rep(NA, nrow(Data_8.7)), forecast_results$fcst$Y1[,2]),
  Upper_CI = c(rep(NA, nrow(Data_8.7)), forecast_results$fcst$Y1[,3])
) %>% 
  filter(Period > 400)

# Create the ggplot2 plot
plot_forecast_VAR <- ggplot(forecast_data, aes(x = Period)) +
  geom_line(aes(y = Actual), color = "black", linetype = "solid") +
  geom_line(aes(y = Forecast), color = "red", linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "blue", alpha = 0.3) +
  labs(
    title = "Forecasted Values for Y1",
    y = "Y1",
    x = "Period"
  ) +
  theme_minimal()


#Checking cointegration
johansen_test_result <- ca.jo(data.frame(ts_Data_8.7[,1], ts_Data_8.7[,2]), type = "eigen", ecdet = "const", K = 4)
summary(johansen_test_result)

# r=0, test statistic > critical, therefore we cannot accept hypthosesis of r = 0 (or no cointegration)
#there exists cointegration
 # if rank would be 0, it would mean in the matrix there are no independent vectors and therefore series
#are not cointegrated

#Fitting ECM
vecm <- tsDyn::VECM(ts_Data_8.7, lag=4)
summary(vecm)

#priklausomybe differenced lagams yra silpnoka, tik su 10% confidence
# ECT abu stipriai significant, abu negatively, tai reiskias, jog:
#in the short term, if consumption deviates from the long-term equilibrium by 1%, 
#it will adjust by approximately 0.033% towards the equilibrium in the following period.

#Y1 treasury is tend to come back to equilibrium faster than Y3.

#VECM forecast
forecast_results_vecm <- predict(vecm, n.ahead = forecast_horizon)

nrow(Data_8.7) + forecast_horizon
forecast_data_VECM <- data.frame(
  Period = 1:(nrow(Data_8.7) + forecast_horizon),
  Actual = c(Data_8.7[,1], rep(NA, forecast_horizon)),
  Forecast_VECM = c(rep(NA, nrow(Data_8.7)), forecast_results_vecm[,1])
) %>% 
  filter(Period > 400)

# Create the ggplot2 plot
plot_forecast_VECM <- ggplot(forecast_data_VECM, aes(x = Period)) +
  geom_line(aes(y = Actual), color = "black", linetype = "solid") +
  geom_line(aes(y = Forecast_VECM), color = "red", linetype = "dashed") +
  # geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "blue", alpha = 0.3) +
  labs(
    title = "Forecasted Values for Y1",
    y = "Y1",
    x = "Period"
  ) +
  theme_minimal()


### Compare VAR ir VECM
#SUJUNGTI LENTELES IR GRAFIKUS

final_table <- cbind(forecast_data[,1:3], forecast_data_VECM[,3])

ggplot(final_table, aes(x = Period)) +
  geom_line(aes(y = Actual), color = "black", linetype = "solid") +
  geom_line(aes(y = Forecast), color = "blue", linetype = "dashed") +
  geom_line(aes(y = Forecast_VECM), color = "red", linetype = "dashed") +
  # geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "blue", alpha = 0.3) +
  labs(
    title = "Forecasted Values for Y1",
    y = "Y1",
    x = "Period"
  ) +
  theme_minimal()

#VECM model produces slightly higher results, but they are relatively similar

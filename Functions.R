Metadata<-function(df){
  library(DataExplorer)
  library(kableExtra)
  z<-introduce(df)
  z<-as.data.frame(t(z))
  colnames(z)<-c()
  knitr::kable(
    z,
    caption="Data Introduction"
  ) %>% kable_styling(bootstrap_options = c("striped", "hover"),
                      full_width = F,
                      font_size = 12,
                      position = "left")
  
}

Evaluation <- function(model, y_pred, y_true) {
  library(MLmetrics)
  adjr2 <- summary(model)$adj.r.squared
  mse <- MSE(y_pred, y_true)
  rmse <- RMSE(y_pred, y_true)
  mae <- MAE(y_pred, y_true)
  print(paste0("Adjusted R-squared: ", round(adjr2, 4)))
  print(paste0("MSE: ", round(mse, 4)))
  print(paste0("RMSE: ", round(rmse, 4)))
  print(paste0("MAE: ", round(mae, 4)))
}
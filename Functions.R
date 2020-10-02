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
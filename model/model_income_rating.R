# Modeling for Income Regression

pca_transform <- function(data, n_components, type = "train"){
  if(!is.data.frame(data)){
    stop("Error: Must be a dataframe.")
  }
  if(missing(n_components) || !(is.numeric(n_components)) || n_components <= 0){
    stop("Error: 'n_components' must be provided")
  }
  scale_data <- scale(data[,!(colnames(data) %in% c("income","star_ratings"))])
  pca_data <- prcomp(scale_data)
  pca_ncomponents <- pca_data[['x']][ , colnames(pca_data[['x']]) %in% paste0("PC", 1:n_components)]
  
  if(type == "train"){
    final_data <- data.frame(income=data[,c("income")],pca_ncomponents)
  }else{
    final_data <- data.frame(pca_ncomponents)
  }
  return(final_data)
}

modelPCARegression <- function(data, response){
  if(!is.data.frame(data)){
    stop("Error: Must be a dataframe.")
  }
  if(missing(response) || !(response %in% colnames(data)) || !is.character(response)){
    stop("Error: Response variable Income should be in the dataframe.")
  }
  pca_model <- lm(data = data, 
                  formula = as.formula(paste(response, "~ .")) )
  return(pca_model)
}


# Modeling for Star Rating Classification 

# install.packages('glmnet')
library(glmnet)

modelElasticNet <- function(data, a, l){
  if(!(is.data.frame(data))){
    stop("Error: Must be a dataframe.")
  }
  if(missing(a) || !(is.numeric(a)) || length(a) != 1){
    stop("Error: Only one 'alpha' must be provided and in numeric value")
  }
  if(missing(l) || !(is.numeric(l)) || length(l) != 1){
    stop("Error: Only one 'lambda' must be provided and in numeric value")
  }
  
  X <- as.matrix(data[,!(colnames(data) %in% c("income","star_ratings"))])
  Y <- as.matrix(factor(data[,'star_ratings']))
  eNet_model <- glmnet(x = X, y = Y, family = 'multinomial', alpha = a, lambda = l)
  
  return(eNet_model)
  
}





main <- function(){
  print("Generating Results")
  data_train <- read.csv('data/restaurants_train.csv')
  predict_data <- read.csv('data/restaurants_test.csv')
  # PCA-Reg
  df_transformed <- pca_transform(data=data_train, n_components = 10, type = 'train')
  model_output_pca <- modelPCARegression(df_transformed,"income")
  print(summary(model_output_pca))

  # Predicted data should be in PCA also
  predictions_income <- predict(model_output_pca, newdata = pca_transform(predict_data, 10, type = 'test'))
  print(head(predictions_income, 5))
  print('Predict Income: Complete')
  
  # Elastic Net
  model_output_elnet <- modelElasticNet(data = data_train, a = 0.9, l = 0.00602685052286242)
  print(model_output_elnet)
  predictions_rating <- predict(model_output_elnet, newx = as.matrix(predict_data), type='class')
  colnames(predictions_rating)[1] <- 'star_ratings'
  print(head(predictions_rating, 5))
  print('Predict Star Rating: Complete')
  
  
  cat("Writing CSV file.\n=================================\n")
  predictions_df <- data.frame(income = predictions_income, star_ratings = predictions_rating)
  print(head(predictions_df,6))
  cat("=================================")
  write.csv(predictions_df, './predict-data/incomeratings_predict.csv', row.names=FALSE)
  cat("\n Complete.")
  
  
}

if (interactive()) {
  main()
}

  
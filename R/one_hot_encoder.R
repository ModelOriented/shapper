one_hot_encoder <- function(data) {

  factors <- sapply(data, class)
  expanded_data <- model.matrix(~., data)

  res <- as.data.frame(expanded_data)[,-1]

  decoder <- list(coltypes = factors,
                  assign = attr(expanded_data, "assign")[-1],
                  new_colnames = colnames(res)
                )
    for(col in colnames(data)){
    if(factors[col] == "factor") decoder$levels[[col]] <- levels(data[ ,col])
  }
  attr(res, "decoder") <- decoder


  return(res)
}


one_hot_decoder <- function(data, decoding_data){

  expanded_data <- as.data.frame(data)
  coltypes <- decoding_data$coltypes
  assign <- decoding_data$assign
  var_levels <- decoding_data$levels
  new_colnames <- decoding_data$new_colnames
  colnames(expanded_data) <- new_colnames

  # print(head(expanded_data))
  for (i in 1:length(coltypes)) {
    colname <- names(coltypes)[i]
    if(coltypes[i] == "factor"){
      columns <- as.data.frame(expanded_data[, assign == i])
      columns <- sweep(columns, 2, 1:ncol(columns), "*")
      orig_factor <- factor(rowSums(columns))
      levels(orig_factor) <- var_levels[[colname]]
      expanded_data[colname] <- orig_factor
    }
  }

  if(sum(-which(assign %in% which(coltypes == "factor"))) != 0){
    expanded_data <- expanded_data[, -which(assign %in% which(coltypes == "factor"))]
    expanded_data <- expanded_data[, names(coltypes)]
  }
  return(expanded_data)
}

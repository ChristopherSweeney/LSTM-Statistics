library(keras)

test_train_split<-function(data,partion){
  num_par = length(data)%/%parition
  data_test_train = matrix(, nrow = len, ncol = num_par)
  for(i in 1:num_par){
  }
}

forward_regression<-function(data,coeff){
  line = rep(0,length(data)+1)
  for(i in length(coeff):length(data)){
      line[i+2] = matrix(data[i+1-length(coeff):i+1])%*%coeff
  }
  return(line)
}

propgate_forward<-function(generator_func,model,iter){
  predictions = vector()
  for(i in 1:iter){
    gen = generator_func()
    samples = unlist(gen[1], use.names=FALSE)
    targets = gen[2]
    input = array(0, dim = c(1, length(samples),1))
    input[1,,1] =samples
    predict = model %>% predict(input)
    predictions = append(predictions,predict)
  }
  return(predictions)
}

propgate_forward_regress<-function(generator_func,coeff,iter){
  predictions = vector()
  for(i in 1:iter){
    gen = generator_func()
    samples = unlist(gen[1], use.names=FALSE)
    targets = gen[2]
    predict = coeff%*%samples
    predictions = append(predictions,predict)
  }
  return(predictions)
}

generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle, batch_size, step) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    samples <- array(0, dim = c(length(rows), lookback / step,1))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[j]+1 - lookback, rows[j], length.out = dim(samples)[2])
      samples[j,,1] <- data[indices]
      targets[j] <- data[rows[j] + delay]
    }            
    list(samples, targets)
  }
}


BrierScore <- function(x, pred=NULL, scaled = FALSE, ...){


  .Brier <- function(resp, pred, scaled = FALSE, ...){

    # check for k > 2
    # Frank Harrell: I don't think an overall Brier score for polytomous
    #                outcomes has been fully worked out.
    # https://stats.stackexchange.com/questions/187627/calculating-the-brier-score-for-multinomial-models
    
    # see also BrierScore.mult
    # mclust::BrierScore
    
    if(!all(unique(resp) %in% c(0, 1)))
      stop("BrierScore can only be calculated for binomial outcomes.")

    res <- mean(resp * (1-pred)^2 + (1-resp) * pred^2)

    if(scaled){
      mean_y <- mean(resp)

      Bmax <- mean_y * (1-mean_y)^2 + (1-mean_y) * mean_y^2
      res <- 1 - res/Bmax
    }

    return(res)

  }


  NumResponse <- function(x){
    # returns the numeric response from a model in 0-1
    # works for glm, rp, rf, nn, c5, svm, qda, lda, nb, lb

    x$terms <- eval(x$call$formula)
    res <- model.response(model.frame(x))
    
    if(is.factor(res))
      res <- as.numeric(res) -1
    
    return(res)
    
  }
  

  if(!is.null(pred)) {
    .Brier(x, pred, scaled)

  } else {

    if(inherits(x, "glm")) {
      pred <- predict(x, type="response")
      resp <- NumResponse(x) # as.numeric(x$y)

     } else {
      pred <- predict(x, type="prob")[, 2]
      # resp <- as.numeric(model.extract(x$model, "response")) - 1
      resp <- NumResponse(x)
     }

    .Brier(resp=resp, pred=pred, scaled=scaled)

  }

}




BrierScore.mult <- function(x, scaled=FALSE, ...){

  
  # .BrierScore.mclust <- function (z, class) {
  #   
  #   z <- as.matrix(z)
  #   z <- sweep(z, 1, STATS = rowSums(z), FUN = "/")
  #   
  #   cl <- unmap(class, groups = if (is.factor(class)) 
  #     levels(class)
  #     else NULL)
  #   
  #   if (any(dim(cl) != dim(z))) 
  #     stop("input arguments do not match!")
  #   
  #   sum((cl - z)^2)/(2 * nrow(cl))
  #   
  # }
  
  # https://en.wikipedia.org/wiki/Brier_score

  ref <- model.response(model.frame(x))
  res <- mean(apply((DescTools::Dummy(ref, method = "full") - 
                              predict(x, type="prob"))^2, 1, sum))

  # check for reference, this is not correct!!
  # if(scaled){
  #   mean_y <- mean(x)
  #
  #   Bmax <- mean_y * (1-mean_y)^2 + (1-mean_y) * mean_y^2
  #   res <- 1 - res/Bmax
  # }

  return(res)

}



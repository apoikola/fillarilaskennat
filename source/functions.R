# Functions aux

# Plot gam smooths with ggplot2
# from http://stackoverflow.com/questions/19735149/is-it-possible-to-plot-the-smooth-components-of-a-gam-fit-with-ggplot2
EvaluateSmooths = function(model, select=NULL, x=NULL, n=100) {
  if (is.null(select)) {
    select = 1:length(model$smooth)
  }
  do.call(rbind, lapply(select, function(i) {
    smooth = model$smooth[[i]]
    data = model$model
    
    if (is.null(x)) {
      min = min(data[smooth$term])
      max = max(data[smooth$term])
      x = seq(min, max, length=n)
    }
    if (smooth$by == "NA") {
      by.level = "NA"
    } else {
      by.level = smooth$by.level
    }
    range = data.frame(x=x, by=by.level)
    names(range) = c(smooth$term, smooth$by)
    
    mat = PredictMat(smooth, range)
    par = smooth$first.para:smooth$last.para
    
    y = mat %*% model$coefficients[par]
    
    se = sqrt(rowSums(
      (mat %*% model$Vp[par, par, drop = FALSE]) * mat
    ))
    
    return(data.frame(
      label=smooth$label
      , x.var=smooth$term
      , x.val=x
      , by.var=smooth$by
      , by.val=by.level
      , value = y
      , se = se
    ))
  }))
}
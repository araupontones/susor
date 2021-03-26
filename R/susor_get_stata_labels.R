#' Assign labels to categorical values using stata attributes


susor_get_stata_labels = function(x){
  levels = attributes(x)$labels
  labels = names(levels)

  factor(x,
         levels = levels,
         labels = labels)

}

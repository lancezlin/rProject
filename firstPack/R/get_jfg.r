get_jfg <- function(data){
  family_group_name <- c()
  job_family_group <- data$"Job Family Group"
  len <- length(job_family_group)
  for (i in 1:len){
    if (is.element(job_family_group[i], family_group_name)){
    #if (any(family_group_name != job_family_group[i])){
    #family_group_name <- append(family_group_name, job_family_group[i])
      next
    }
    else {
      family_group_name <- append(family_group_name, job_family_group[i])
    }
  }
  return (family_group_name)
}

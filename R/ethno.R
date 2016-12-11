# ethno
#
# this function will get ethnicities from the textmap ethnicity
# classifier.
#
# citation:
# Ambekar, A., Ward, C., Mohammed, J., Male, S., and Skiena, S. 2009.
# Name-ethnicity classification from open sources.
# In Proceedings of the 15th ACM SIGKDD international Conference on Knowledge Discovery and Data Mining (Paris, France, June 28 - July 01, 2009).
# KDD '09. ACM, New York, NY, 49-58. DOI= http://doi.acm.org/10.1145/1557019.1557032

# $.ajax({
#   type: "POST",
#   url: "www.textmap.com/ethnicity_api/api",
#   data: JSON.stringify(
#     { "names" : [ "George Washington", "John Smith", "Barack Obama" ] }
#   ),
#   success: function (data) {
#     console.log(data);
#   },
#   dataType: "json"
# });

.prep_json <- function(name_vec){
  name_list <- list(names = name_vec)
  jsonlite::toJSON(name_list)
}

.pluck_scores <- function(df){
  return(as.vector(df[order(df[,"score"], decreasing=T),][1,]))
  }

.pluck_top_results <- function(result){
  lapply(result[[1]]$scores, .pluck_scores) %>%
    unlist()
}

.prep_result <- function(ethno_results){
  result_table <- httr::content(ethno_results, "text") %>%
    jsonlite::fromJSON()
}

.get_ethnicity <- function(name) {
  url = "www.textmap.com/ethnicity_api/api"
  name_json <- .prep_json(name)
  results = httr::POST(url = url, body = name_json, encode = "json", httr::verbose())
  prepped_result = .prep_result(results)
  top_result = .pluck_top_results(prepped_result)
  return(c(name=name, top_result))
}

get_ethno <- function(name_vec, full=FALSE){
  if(length(name_vec) > 2000){
    stop("length of name_vec > 2000\n\tPlease break your query into batches of 2000 or less")
  }
  ethno_list <- lapply(name_vec, .get_ethnicity)
  if(full){
    return(ethno_list)
  } else{
    ethno_dat <- lapply(ethno_list, function(x){x[1:3]})
    ethno_df <- data.frame(t(matrix(unlist(ethno_dat),nrow=3)), stringsAsFactors = F)
    names(ethno_df) <- c("name", "score", "ethnicity")
    return(ethno_df)
  }
}

# Notes:
# need to batch together names in order to save
# network latency
# need to be able to handle the error:
# Error: lexical error: invalid char in json text.
# want to be able to batch and submit more than
# 2000 names without overrunning the API limit.


#' generate a textflow.batchFlow connection from an input and output file into a model
#based on:
# sampling_generator <- function(X_data, Y_data, batch_size) {
#   function() {
#     rows <- sample(1:nrow(X_data), batch_size, replace = TRUE)
#     list(X_data[rows,], Y_data[rows,])
#   }
# }
#
# model %>%
#   fit_generator(sampling_generator(X_train, Y_train, batch_size = 128),
#                 steps_per_epoch = nrow(X_train) / 128, epochs = 10)
#' generate a textflow.batchFlow connection from an input and output file into a model
#' @import R6
#' @export
#'
text_data_generator<-function(directory=".",inputfile="inputs.txt",outputfile="outputs.txt",nchars=20,batchSize=32,inputs="")
{
  textFlowObject<-textflow.batchFlow$new(file.path(directory,inputfile),
                                         file.path(directory,outputfile),
                                         nchars,
                                         batchSize,
                                         inputs = inputs)
  return(textFlowObject)
}



#' wrapper for batch reading data from file
#' @export
flow_text_from_file<-function(generator=text_data_generator()){
  function(...){
    generator$flow_from_file(...)
  }
}

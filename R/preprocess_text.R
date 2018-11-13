#' flow text from a textdf to an input and output file
#' @import tidyverse stringi tidytext tokenizers
#' @export
flow_text_to_file<-function(textdf,nchars=20,outputs,filepath=tempdir(),append=FALSE,filenames=c("inputs.txt","outputs.txt")){

  cat(sprintf("Mapping Reviews: %01d to %01d ----------------\n\n", 0,nrow(textdf)))

  pb <- txtProgressBar(min = 0, max = nrow(textdf), style = 3)

  catchoutput<-textdf%>%
  {
    lapply(1:nrow(.),function(rown,df,pb){

      # unless told explicitly to append, it will
      appendoutFile<-ifelse(append,TRUE,ifelse(rown==1,FALSE,TRUE))
      if(file.exists(file.path(filepath,filenames[1])) & !appendoutFile){
        file.remove(file.path(filepath,filenames[1]))
      }
      if(file.exists(file.path(filepath,filenames[2])) & !appendoutFile){
        file.remove(file.path(filepath,filenames[2]))
      }

      tmpdf<-df[rown,]
      Review<-c(tmpdf$tokenized[[1]],"ENDOFREVIEW")

      # reviews_dataset <- map(
      #   seq(1, length(Review) - nchars),
      #   ~list(sentence = Review[.x:(.x + nchars - 1)],next_char = Review[.x + nchars])
      # )%>%purrr::transpose()

      #for each row from nchar to end of the re
      catchOutput<-lapply(
        seq(1, length(Review) - 1),
        function(x){
          cat(which(outputs%in%Review[x]),"\n",
              file=file.path(filepath,filenames[1]),
              append=TRUE) #unless told explicitly to append, on
          cat(which(outputs%in%Review[x+1]),"\n",
              file=file.path(filepath,filenames[2]),
              append=TRUE) #unless told explicitly to append, on
          invisible(x)
        }
      )

      setTxtProgressBar(pb, rown)
    },.,pb)}
}

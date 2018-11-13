
library(keras)
library(knitr)
library(tidyverse)
library(tidytext)
library(tokenizers)
library(stringi)
library(data.table)
library(abind)
#read in csv and correct spellings

wineData<-fread("inst/wine.txt",header = FALSE)%>%
  setnames(c("Name","Year","Description","Points","Price","Designation","Variety","Appelation","Winery","Alcohol","BottleSize","Category","URL"))

wineData <- wineData %>%
  filter(!is.na(Description),
         Category=="Red")%>%
  mutate(Alcohol=ifelse(!is.na(Alcohol),Alcohol,ifelse(Alcohol=="N/A",0,0)))%>%
  group_by(Variety)%>%
  filter(n()>100)%>%
  ungroup%>%
  as_tibble


wineData$tokenized<-wineData %>%
  select(Description)%>%
  unlist%>%
  stri_enc_toascii()%>%
  stri_enc_toutf8()%>%
  str_to_lower()%>%
  {gsub(" ros ","rose",.)}%>%
  {gsub("\\n","",.)}%>%
  {gsub("\\r","",.)}%>%
  {gsub("\\r\\n","",.)}%>%
  {gsub("\\\\","\\",.)}%>%
  {gsub("\032","",.)}%>%
  tokenize_characters(strip_non_alphanum = FALSE, simplify=FALSE)

wineData$Variety<-strsplit(wineData$Variety,split = ", |(-(?!style)(?![GSM]))",perl = TRUE)

wineTypes<-wineData$Variety%>%
{do.call('c',.)}%>%
  unique%>%sort

Nchars<-20

Outputs <- wineData$tokenized %>%
{do.call('c',.)}%>%
  unique() %>%
  sort() %>%
  c("ENDOFREVIEW")

Inputs <- Outputs



#this function will slice up the Red wine Reviews data.frame for training the DNN.
# We unfortunately cannot set up the code for running all the reviews as once!
# we will wrap the fitting of the model with some for loops, and also set it up such that
# the mapping is only done once, and it reads RDS files every time
wineData%>%
  head(n=5000)%>%
  flow_text_to_file(Nchars,outputs = Outputs,filepath = "inst",filenames = c("5000_inputs2.txt","5000_outputs2.txt"))


#create Model

batchSize<-128

Redmodel <- keras_model_sequential()

Redmodel %>%
  layer_cudnn_lstm(batchSize, input_shape = c(Nchars, length(Inputs)),return_sequences = TRUE) %>%
  layer_dropout(.3)%>%
  layer_cudnn_lstm(batchSize, return_sequences = TRUE) %>%
  layer_dropout(.3)%>%
  layer_cudnn_lstm(batchSize, return_sequences = TRUE) %>%
  layer_dropout(.3)%>%
  layer_cudnn_lstm(batchSize)%>%
  layer_dense(length(Outputs),activation = 'relu' )%>%
  layer_dense(length(Outputs),activation = )%>%
  layer_activation("softmax")

optimizer <- optimizer_rmsprop(lr = 0.0001)

Redmodel %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer
)

# Training & Results ----------------------------------------------------

sample_mod <- function(preds, temperature = 1){
  preds <- log(preds)/temperature
  exp_preds <- exp(preds)
  preds <- exp_preds/sum(exp(preds))

  rmultinom(1, 1, preds) %>%
    as.integer() %>%
    which.max()
}


#### output
on_epoch_end <- function(wineDF,baseLen=10,outputChars){

  function(epoch, logs) {

    cat(sprintf("epoch: %02d ---------------\n\n", epoch))

    # for(diversity in c(0.2, 0.5, 1, 1.2)){
    for(diversity in c(.8)){

      cat(sprintf("diversity: %f ---------------\n\n", diversity))

      tmpdf<-wineDF$tokenized[1:baseLen]

      Seed <- tmpdf
      splitSeed<-Seed
      generated <- ""
      next_char <- ""

      reviewChars<-0

      while(next_char!="ENDOFREVIEW"){

        x <- data.frame(sapply(outputChars, function(x){as.integer(x == splitSeed)}))%>%as.matrix

        x <- tensorflow::array_reshape(x, c(1, dim(x)))

        preds <- try(predict(Redmodel, x))

        next_index <- sample_mod(preds, diversity)
        next_char <- outputChars[next_index]

        generated <- str_c(generated, next_char, collapse = "")
        splitSeed <- c(splitSeed[-1], next_char)

        reviewChars<-reviewChars+1

        if(reviewChars>1000){
          break
        }
      }

      print(generated)
      cat("\n\n")

    }
  }
}

print_callback <- callback_lambda(on_epoch_end=on_epoch_end(wineDF = wineData,baseLen = Nchars,outputChars = Outputs))

# sampling_generator <- function(X_data, Y_data, batch_size) {
#   function() {
#     rows <- sample(1:nrow(X_data), batch_size, replace = TRUE)
#     list(X_data[rows,], Y_data[rows,])
#   }
# }

# model %>%
#   fit_generator(sampling_generator(X_train, Y_train, batch_size = 128),
#                 steps_per_epoch = nrow(X_train) / 128, epochs = 10)

textFlowGenerator<-text_data_generator(directory="inst",
                                       inputfile="5000_inputs2.txt",
                                       outputfile="5000_outputs2.txt",
                                       nchars=20,
                                       batchSize=batchSize,
                                       inputs=Inputs)


epochsteps<-as.integer((as.numeric(strsplit(system("wc -l inst/5000_inputs2.txt",intern = TRUE),split=" ")[[1]][1])-(5000*20))/batchSize)

Redmodel %>% fit_generator(
  flow_text_from_file(textFlowGenerator),
  steps_per_epoch = epochsteps,
  epochs = 20,
  callbacks = print_callback
)



#' wrapper for batch reading data from file
#'
#' @import R6
textflow.batchFlow<-R6Class("textflow.batchFlow",
                            private=list(
                              inputcon=NULL,
                              inputfilepath=NULL,
                              outputcon=NULL,
                              outputfilepath=NULL,
                              nchar=NULL,
                              batchsize=NULL,
                              lastTensor=NULL,
                              tmpTensor=NULL,
                              inputs=NULL,

                              generateTensor=function(verbose=FALSE){

                                newTensorX<-array(0,dim=c(private$batchsize,private$nchar,length(private$inputs)))
                                newTensorY<-array(0,dim=c(private$batchsize,length(private$inputs)))
                                tmpOutputarray<-array(0,dim=c(1,length(private$inputs)))

                                batchstep<-0
                                while(batchstep < private$batchsize){
                                  batchstep<-batchstep+1
                                  if(length(input<<-readLines(private$inputcon,n=1))>0){
                                    output<-readLines(private$outputcon,n=1)

                                    if(verbose){
                                      cat("Batchstep:",batchstep,"\n")
                                      cat("input:",input,"\n")
                                      cat("inputVal:", private$inputs[as.numeric(input)],"\n")
                                      cat("output:",input,"\n")
                                      cat("-----\n")
                                    }
                                    #if the prior entry was the "end of review", skip down nchar-1 rows and reset the tmpTensor

                                    private$tmpTensor$input[1:private$nchar-1,]<-private$tmpTensor$input[-1,]
                                    private$tmpTensor$input[private$nchar,]<-0
                                    private$tmpTensor$input[private$nchar,as.numeric(input)]<-1

                                    private$tmpTensor$output[1,]<-0
                                    private$tmpTensor$output[as.numeric(output)]<-1



                                    newTensorX[batchstep,,]<-private$tmpTensor$input
                                    newTensorY[batchstep,]<-private$tmpTensor$output

                                    if(as.numeric(output)==length(private$inputs)){
                                      private$preprocess()
                                    }
                                  }else{
                                    private$resetConnection()
                                  }
                                }

                                return(private$lastTensor<-list(newTensorX,newTensorY))

                              },

                              preprocess=function(){

                                firstInputTensor<-array(0,dim = c(private$nchar,length(private$inputs)))
                                inputcol<-as.numeric(readLines(private$inputcon,n=private$nchar-1))

                                for(rowEntry in 2:private$nchar){
                                  firstInputTensor[rowEntry,inputcol[rowEntry-1]]<-1
                                }

                                firstOutputTensor<-array(0,dim = c(1,length(private$inputs)))

                                firstOutputTensor[as.numeric(readLines(private$outputcon,n=private$nchar-1))[private$nchar-1]]<-1


                                private$tmpTensor<-list(input=firstInputTensor,
                                                     output=firstOutputTensor)

                              },

                              resetConnection=function(){
                                close(private$inputcon)
                                close(private$outputcon)
                                private$inputcon<-file(private$inputfilepath,open = "r")
                                private$outputcon<-file(private$outputfilepath,open = "r")
                                private$preprocess()

                              }

                            ),

                            public=list(
                              #identify Read in files

                              initialize=function(inputfile,outputfile,lenchars,batchsize=32,inputs){
                                self$inputFile(inputfile)
                                self$outputFile(outputfile)
                                self$nchars(lenchars)
                                self$setBatchSize(batchsize)
                                self$setInputs(inputs)

                                private$preprocess()
                              },

                              inputFile=function(filename){
                                private$inputcon<-file(filename,open = "r")
                                private$inputfilepath<-filename

                              },
                              outputFile=function(filename){
                                private$outputcon<-file(filename,open = "r")
                                private$outputfilepath<-filename

                              },
                              nchars=function(nchar){
                                private$nchar<-nchar
                              },
                              setBatchSize=function(batchsize){
                                private$batchsize<-batchsize
                              },
                              setInputs=function(inputs){
                                private$inputs<-inputs
                              },

                              flow_from_file=function(verbose=FALSE){
                                private$generateTensor(verbose=verbose)
                                return(private$lastTensor)
                              },

                              close_connection=function(){
                                close(private$inputcon)
                                close(private$outputcon)
                              }
                            )
)




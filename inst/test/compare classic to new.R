#comparisons

original<-readRDS(file.path("../WineR/sommeli_AI/MAPPED/",paste0("MappedReviews_",1,"_",500,".RDS")))

batch1<-list(x=original$x[1:128,,1:57],
             y=original$y[1:128,])

batch2<-list(x=original$x[c(129:c(129+127)),,1:57],
             y=original$y[c(129:c(129+127)),])

batch3<-list(x=original$x[c(257:c(257+127)),,1:57],
             y=original$y[c(257:c(257+127)),])

textFlowGenerator<-text_data_generator(directory="inst",
                                       inputfile="500_inputs3.txt",
                                       outputfile="500_outputs3.txt",
                                       nchars=20,
                                       batchSize=128,
                                       inputs=Inputs)

getbatch<-flow_text_from_file(textFlowGenerator)

newBatch_1<-getbatch(verbose=TRUE)
newBatch_2<-getbatch(verbose=TRUE)
newBatch_3<-getbatch()

all(newBatch_1[[1]]==batch1$x)
all(newBatch_1[[2]]==batch1$y)

lapply(1:128,function(x)rowSums(newBatch_2[[1]][x,,]==batch2$x[x,,]))
textFlowGenerator$close_connection()

all(newBatch_2[[1]]==batch2$x)
all(newBatch_2[[2]]==batch2$y)

all(newBatch_3[[1]]==batch3$x)
all(newBatch_3[[2]]==batch3$y)

table(newBatch2[[1]]==batch2$x)
table(newBatch3[[1]][1,,]==batch3$x[2,,])


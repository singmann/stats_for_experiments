# descriptives statistics by condition
desc_objZ <- describeBy(mydata$objectiveZ, 
                        group = mydata$laplong)
desc_openZ <- describeBy(mydata$openZ, 
                         group = mydata$laplong)
desc_obj <- describeBy(mydata$objective, 
                       group = mydata$laplong)
desc_open <- describeBy(mydata$open, 
                        group = mydata$laplong)
desc_objprop <- describeBy(mydata$objectiveprop, 
                        group = mydata$laplong)
desc_openprop <- describeBy(mydata$openprop, 
                         group = mydata$laplong)
desc_wc <- describeBy(mydata$wordcount, 
                      group = mydata$laplong)
desc_vo <- describeBy(mydata$threegramspercent, 
                      group = mydata$laplong)

# get 95% CI around means by condition
# across conditions
#MO1
CI_objprop_MO1 <- gsub("0.", ".", gsub(";", ",", descr(MO_Study1$objectiveprop)$`central tendency`$`95% CI mean`))
CI_openprop_MO1 <- gsub("0.", ".", gsub(";", ",", descr(MO_Study1$openprop)$`central tendency`$`95% CI mean`))
CI_wc_MO1 <- gsub(";", ",", descr(MO_Study1$Wcount)$`central tendency`$`95% CI mean`)
CI_vo_MO1 <- gsub(";", ",", descr(100*MO_Study1$threeGR)$`central tendency`$`95% CI mean`)

# this replication
CI_objprop <- gsub("0.", ".", gsub(";", ",", descr(mydata$objectiveprop)$`central tendency`$`95% CI mean`))
CI_openprop <- gsub("0.", ".", gsub(";", ",", descr(mydata$openprop)$`central tendency`$`95% CI mean`))
CI_wc <- gsub(";", ",", descr(mydata$wordcount)$`central tendency`$`95% CI mean`)
CI_vo <- gsub(";", ",", descr(mydata$threegramspercent)$`central tendency`$`95% CI mean`)

# longhand
CI_objZ_longhand <- gsub(";", ",", descr(mydata$objectiveZ[ mydata$condition_label == "longhand"])$`central tendency`$`95% CI mean`)
CI_openZ_longhand <- gsub(";", ",", descr(mydata$openZ[ mydata$condition_label == "longhand"])$`central tendency`$`95% CI mean`)
CI_obj_longhand <- gsub(";", ",", descr(mydata$objective[ mydata$condition_label == "longhand"])$`central tendency`$`95% CI mean`)
CI_open_longhand <- gsub(";", ",", descr(mydata$open[ mydata$condition_label == "longhand"])$`central tendency`$`95% CI mean`)
CI_objprop_longhand <- gsub("0.", ".", gsub(";", ",", descr(mydata$objectiveprop[ mydata$condition_label == "longhand"])$`central tendency`$`95% CI mean`))
CI_openprop_longhand <- gsub("0.", ".", gsub(";", ",", descr(mydata$openprop[ mydata$condition_label == "longhand"])$`central tendency`$`95% CI mean`))
CI_wc_longhand <- gsub(";", ",", descr(mydata$wordcount[ mydata$condition_label == "longhand"])$`central tendency`$`95% CI mean`)
CI_vo_longhand <- gsub(";", ",", descr(mydata$threegramspercent[ mydata$condition_label == "longhand"])$`central tendency`$`95% CI mean`)

# laptop
CI_objZ_laptop <- gsub(";", ",", descr(mydata$objectiveZ[ mydata$condition_label == "laptop"])$`central tendency`$`95% CI mean`)
CI_openZ_laptop <- gsub(";", ",", descr(mydata$openZ[ mydata$condition_label == "laptop"])$`central tendency`$`95% CI mean`)
CI_obj_laptop <- gsub(";", ",", descr(mydata$objective[ mydata$condition_label == "laptop"])$`central tendency`$`95% CI mean`)
CI_open_laptop <- gsub(";", ",", descr(mydata$open[ mydata$condition_label == "laptop"])$`central tendency`$`95% CI mean`)
CI_objprop_laptop <- gsub("0.", ".", gsub(";", ",", descr(mydata$objectiveprop[ mydata$condition_label == "laptop"])$`central tendency`$`95% CI mean`))
CI_openprop_laptop <- gsub("0.", ".", gsub(";", ",", descr(mydata$openprop[ mydata$condition_label == "laptop"])$`central tendency`$`95% CI mean`))
CI_wc_laptop <- gsub(";", ",", descr(mydata$wordcount[ mydata$condition_label == "laptop"])$`central tendency`$`95% CI mean`)
CI_vo_laptop <- gsub(";", ",", descr(mydata$threegramspercent[ mydata$condition_label == "laptop"])$`central tendency`$`95% CI mean`)



# descriptives statistics by condition and lecture

# first subset by lecture
alg <- subset(mydata,whichtalklabel=="algorithms")
ideas <- subset(mydata,whichtalklabel=="ideas")
indus <- subset(mydata,whichtalklabel=="indus")
inequality <- subset(mydata,whichtalklabel=="inequalities")
islam <- subset(mydata,whichtalklabel=="islam")

# objective Z
desc_objZ_alg <- describeBy(alg$objectiveZ, 
                            group = alg$laplong)
desc_objZ_ideas <- describeBy(ideas$objectiveZ, 
                              group = ideas$laplong)
desc_objZ_indus <- describeBy(indus$objectiveZ, 
                              group = indus$laplong)
desc_objZ_inequality <- describeBy(inequality$objectiveZ,
                                   group = inequality$laplong)
desc_objZ_islam <- describeBy(islam$objectiveZ, 
                              group = islam$laplong)
# open Z
desc_openZ_alg <- describeBy(alg$openZ, 
                            group = alg$laplong)
desc_openZ_ideas <- describeBy(ideas$openZ, 
                              group = ideas$laplong)
desc_openZ_indus <- describeBy(indus$openZ, 
                              group = indus$laplong)
desc_openZ_inequality <- describeBy(inequality$openZ,
                                   group = inequality$laplong)
desc_openZ_islam <- describeBy(islam$openZ, 
                              group = islam$laplong)

# objective
desc_obj_alg <- describeBy(alg$objective, 
                            group = alg$laplong)
desc_obj_ideas <- describeBy(ideas$objective, 
                              group = ideas$laplong)
desc_obj_indus <- describeBy(indus$objective, 
                              group = indus$laplong)
desc_obj_inequality <- describeBy(inequality$objective,
                                   group = inequality$laplong)
desc_obj_islam <- describeBy(islam$objective, 
                              group = islam$laplong)
# open
desc_open_alg <- describeBy(alg$open, 
                            group = alg$laplong)
desc_open_ideas <- describeBy(ideas$open, 
                              group = ideas$laplong)
desc_open_indus <- describeBy(indus$open, 
                              group = indus$laplong)
desc_open_inequality <- describeBy(inequality$open,
                                   group = inequality$laplong)
desc_open_islam <- describeBy(islam$open, 
                              group = islam$laplong)

# objective proportion correct
desc_objprop_alg <- describeBy(alg$objectiveprop, 
                           group = alg$laplong)
desc_objprop_ideas <- describeBy(ideas$objectiveprop, 
                             group = ideas$laplong)
desc_objprop_indus <- describeBy(indus$objectiveprop, 
                             group = indus$laplong)
desc_objprop_inequality <- describeBy(inequality$objectiveprop,
                                  group = inequality$laplong)
desc_objprop_islam <- describeBy(islam$objectiveprop, 
                             group = islam$laplong)
# open proportion correct
desc_openprop_alg <- describeBy(alg$openprop, 
                            group = alg$laplong)
desc_openprop_ideas <- describeBy(ideas$openprop, 
                              group = ideas$laplong)
desc_openprop_indus <- describeBy(indus$openprop, 
                              group = indus$laplong)
desc_openprop_inequality <- describeBy(inequality$openprop,
                                   group = inequality$laplong)
desc_openprop_islam <- describeBy(islam$openprop, 
                              group = islam$laplong)
# word count
desc_wc_alg <- describeBy(alg$wordcount, 
                            group = alg$laplong)
desc_wc_ideas <- describeBy(ideas$wordcount, 
                              group = ideas$laplong)
desc_wc_indus <- describeBy(indus$wordcount, 
                              group = indus$laplong)
desc_wc_inequality <- describeBy(inequality$wordcount,
                                   group = inequality$laplong)
desc_wc_islam <- describeBy(islam$wordcount, 
                              group = islam$laplong)
# verbatim overlap
desc_vo_alg <- describeBy(alg$threegramspercent, 
                            group = alg$laplong)
desc_vo_ideas <- describeBy(ideas$threegramspercent, 
                              group = ideas$laplong)
desc_vo_indus <- describeBy(indus$threegramspercent, 
                              group = indus$laplong)
desc_vo_inequality <- describeBy(inequality$threegramspercent,
                                   group = inequality$laplong)
desc_vo_islam <- describeBy(islam$threegramspercent, 
                              group = islam$laplong)

# bind results together for table
dv_table_laptop <- rbind(as.data.frame(desc_objZ$"0.5"),
                         as.data.frame(desc_objZ_alg$"0.5"),
                         as.data.frame(desc_objZ_ideas$"0.5"),
                         as.data.frame(desc_objZ_indus$"0.5"),
                         as.data.frame(desc_objZ_inequality$"0.5"),
                         as.data.frame(desc_objZ_islam$"0.5"),
                         as.data.frame(desc_openZ$"0.5"),
                         as.data.frame(desc_openZ_alg$"0.5"),
                         as.data.frame(desc_openZ_ideas$"0.5"),
                         as.data.frame(desc_openZ_indus$"0.5"),
                         as.data.frame(desc_openZ_inequality$"0.5"),
                         as.data.frame(desc_openZ_islam$"0.5"),
                         as.data.frame(desc_obj$"0.5"),
                         as.data.frame(desc_obj_alg$"0.5"),
                         as.data.frame(desc_obj_ideas$"0.5"),
                         as.data.frame(desc_obj_indus$"0.5"),
                         as.data.frame(desc_obj_inequality$"0.5"),
                         as.data.frame(desc_obj_islam$"0.5"),
                         as.data.frame(desc_open$"0.5"),
                         as.data.frame(desc_open_alg$"0.5"),
                         as.data.frame(desc_open_ideas$"0.5"),
                         as.data.frame(desc_open_indus$"0.5"),
                         as.data.frame(desc_open_inequality$"0.5"),
                         as.data.frame(desc_open_islam$"0.5"),
                         as.data.frame(desc_objprop$"0.5"),
                         as.data.frame(desc_objprop_alg$"0.5"),
                         as.data.frame(desc_objprop_ideas$"0.5"),
                         as.data.frame(desc_objprop_indus$"0.5"),
                         as.data.frame(desc_objprop_inequality$"0.5"),
                         as.data.frame(desc_objprop_islam$"0.5"),
                         as.data.frame(desc_openprop$"0.5"),
                         as.data.frame(desc_openprop_alg$"0.5"),
                         as.data.frame(desc_openprop_ideas$"0.5"),
                         as.data.frame(desc_openprop_indus$"0.5"),
                         as.data.frame(desc_openprop_inequality$"0.5"),
                         as.data.frame(desc_openprop_islam$"0.5"),
                         as.data.frame(desc_wc$"0.5"),
                         as.data.frame(desc_wc_alg$"0.5"),
                         as.data.frame(desc_wc_ideas$"0.5"),
                         as.data.frame(desc_wc_indus$"0.5"),
                         as.data.frame(desc_wc_inequality$"0.5"),
                         as.data.frame(desc_wc_islam$"0.5"),
                         as.data.frame(desc_vo$"0.5"),
                         as.data.frame(desc_vo_alg$"0.5"),
                         as.data.frame(desc_vo_ideas$"0.5"),
                         as.data.frame(desc_vo_indus$"0.5"),
                         as.data.frame(desc_vo_inequality$"0.5"),
                         as.data.frame(desc_vo_islam$"0.5")
)
dv_table_longhand <- rbind(as.data.frame(desc_objZ$"-0.5"),
                           as.data.frame(desc_objZ_alg$"-0.5"),
                           as.data.frame(desc_objZ_ideas$"-0.5"),
                           as.data.frame(desc_objZ_indus$"-0.5"),
                           as.data.frame(desc_objZ_inequality$"-0.5"),
                           as.data.frame(desc_objZ_islam$"-0.5"),
                           as.data.frame(desc_openZ$"-0.5"),
                           as.data.frame(desc_openZ_alg$"-0.5"),
                           as.data.frame(desc_openZ_ideas$"-0.5"),
                           as.data.frame(desc_openZ_indus$"-0.5"),
                           as.data.frame(desc_openZ_inequality$"-0.5"),
                           as.data.frame(desc_openZ_islam$"-0.5"),
                           as.data.frame(desc_obj$"-0.5"),
                           as.data.frame(desc_obj_alg$"-0.5"),
                           as.data.frame(desc_obj_ideas$"-0.5"),
                           as.data.frame(desc_obj_indus$"-0.5"),
                           as.data.frame(desc_obj_inequality$"-0.5"),
                           as.data.frame(desc_obj_islam$"-0.5"),
                           as.data.frame(desc_open$"-0.5"),
                           as.data.frame(desc_open_alg$"-0.5"),
                           as.data.frame(desc_open_ideas$"-0.5"),
                           as.data.frame(desc_open_indus$"-0.5"),
                           as.data.frame(desc_open_inequality$"-0.5"),
                           as.data.frame(desc_open_islam$"-0.5"),
                           as.data.frame(desc_objprop$"-0.5"),
                           as.data.frame(desc_objprop_alg$"-0.5"),
                           as.data.frame(desc_objprop_ideas$"-0.5"),
                           as.data.frame(desc_objprop_indus$"-0.5"),
                           as.data.frame(desc_objprop_inequality$"-0.5"),
                           as.data.frame(desc_objprop_islam$"-0.5"),
                           as.data.frame(desc_openprop$"-0.5"),
                           as.data.frame(desc_openprop_alg$"-0.5"),
                           as.data.frame(desc_openprop_ideas$"-0.5"),
                           as.data.frame(desc_openprop_indus$"-0.5"),
                           as.data.frame(desc_openprop_inequality$"-0.5"),
                           as.data.frame(desc_openprop_islam$"-0.5"),
                           as.data.frame(desc_wc$"-0.5"),
                           as.data.frame(desc_wc_alg$"-0.5"),
                           as.data.frame(desc_wc_ideas$"-0.5"),
                           as.data.frame(desc_wc_indus$"-0.5"),
                           as.data.frame(desc_wc_inequality$"-0.5"),
                           as.data.frame(desc_wc_islam$"-0.5"),
                           as.data.frame(desc_vo$"-0.5"),
                           as.data.frame(desc_vo_alg$"-0.5"),
                           as.data.frame(desc_vo_ideas$"-0.5"),
                           as.data.frame(desc_vo_indus$"-0.5"),
                           as.data.frame(desc_vo_inequality$"-0.5"),
                           as.data.frame(desc_vo_islam$"-0.5")
)

minNlaptop <- min(dv_table_laptop[2:6,"n"])
maxNlaptop <- max(dv_table_laptop[2:6,"n"])
minNlonghand <- min(dv_table_longhand[2:6,"n"])
maxNlonghand <- max(dv_table_longhand[2:6,"n"])

lecture <- rep(c("All lectures","Algorithms","Ideas","Indus","Inequality","Islam"),8)

dv_table <- cbind(lecture,
                  subset(dv_table_longhand, select=c("mean","sd","n")),
                  subset(dv_table_laptop, select=c("mean","sd","n")))

# compute Cohen's d effect sizes
dv_table$"Cohen's d" <- mes(dv_table[,2],dv_table[,5],dv_table[,3],dv_table[,6],dv_table[,4],dv_table[,7])$d
dv_table$"Lower limit d" <- mes(dv_table[,2],dv_table[,5],dv_table[,3],dv_table[,6],dv_table[,4],dv_table[,7])$l.d
dv_table$"Upper limit d" <- mes(dv_table[,2],dv_table[,5],dv_table[,3],dv_table[,6],dv_table[,4],dv_table[,7])$u.d

# compute Hedges' g effect sizes
dv_table$"Hedges' g" <- mes(dv_table[,2],dv_table[,5],dv_table[,3],dv_table[,6],dv_table[,4],dv_table[,7])$g
dv_table$"Lower limit g" <- mes(dv_table[,2],dv_table[,5],dv_table[,3],dv_table[,6],dv_table[,4],dv_table[,7])$l.g
dv_table$"Upper limit g" <- mes(dv_table[,2],dv_table[,5],dv_table[,3],dv_table[,6],dv_table[,4],dv_table[,7])$u.g

# rename rows to fix weirdness
rownames(dv_table) <- c(1:nrow(dv_table))

# rename columns
names(dv_table) <- c("Measure",
                     "M", 
                     "SD", 
                     "N", 
                     "M", 
                     "SD", 
                     "N", 
                     "Cohen's d", 
                     "Lower limit d", 
                     "Upper limit d",
                     "Hedges' g", 
                     "Lower limit g", 
                     "Upper limit g")
################################################################################
# Set Up
################################################################################

# remove.packages("ddandrda)
# install.packages("devtools")
# devtools::install_github("hannahblo/ddandrda")


library(ddandrda)
library(OpenML)
library(dplyr)
library(gurobi)

# library(oofos)



################################################################################
# Old!!! Data Set - ignore the next part
################################################################################
#
# library(prefmod) # Just help out until we got a data set
# # Information about the data set, page 5f
# # https://cran.r-project.org/web/packages/prefmod/prefmod.pdf
# data_set <- prefmod::cemspc
# head(data_set)
# View(data_set)
# # The data set contains a mistake:
# data_set <- data_set[!is.na(rowSums(data_set)), ]
# # There is a mistake in the data set, see
# # https://www.academia.edu/17714335/Corrigendum_Modelling_the_effect_of_subject
# # -specific_covariates_in_paired_comparison_studies_with_an_application_to_
# # university_rankings
# data_set[ ,15] <- 2 - data_set[ ,15]
#
# # Converting the data set into matrices which represent the partial orders
# # note that not necessarily every matrix represents a antisymmetric relation
#
# convert_to_matrix <- function(row_values, set_NA_to = NA) {
#   # set_NA_to, can equal 0, 1, 2 or NA,
#   #   0: incomparable, 1: first is better, 2: second better
#
#   # Result: Matrix wrapped up in a list with rows/columns refer to
#   #       (1:"London", 2:"Paris", 3:"Milano", 4:"St. Gallen", 5:"Barcelona", 6:"Stockholm")
#
#
#   graph_mat <- matrix(rep(-1, 6*6), nrow = 6)
#   diag(graph_mat) <- 1
#
#   comparison_index <- c(c(1,2), c(1,3), c(2,3), c(1,4), c(2,4), c(3,4),
#                         c(1,5), c(2,5), c(3,5), c(4,5),
#                         c(1,6), c(2,6), c(3,6), c(4,6), c(5,6))
#   # vergleich dies mit der Definition von V1- V15 auf Seite 6 von
#   # https://cran.r-project.org/web/packages/prefmod/prefmod.pdf
#
#   for (i in 1:15) {
#     if (is.na(row_values[i])) {
#       graph_mat[comparison_index[i*2 - 1], comparison_index[i*2]] <- (set_NA_to %% 2) # modulo 2
#       graph_mat[comparison_index[i*2], comparison_index[i*2 - 1]] <- max(set_NA_to - 1, 0)
#     } else if (row_values[i] == 0) {
#       graph_mat[comparison_index[i*2 - 1], comparison_index[i*2]] <- 1
#       graph_mat[comparison_index[i*2], comparison_index[i*2 - 1]] <- 0
#     } else if (row_values[i] == 1) {
#       graph_mat[comparison_index[i*2 - 1], comparison_index[i*2]] <- 0
#       graph_mat[comparison_index[i*2], comparison_index[i*2 - 1]] <- 0
#     } else if (row_values[i] == 2) {
#       graph_mat[comparison_index[i*2 - 1], comparison_index[i*2]] <- 0
#       graph_mat[comparison_index[i*2], comparison_index[i*2 - 1]] <- 1
#     }
#   }
#   return(as.matrix(graph_mat))
# }
#
# # convertieren der einzelnen Zeilen zu matrix
# graph_mat <- list()
# index <- 1
# for (index_loop in 1:dim(data_set)[1]) {
#   graph_mat[[index_loop]] <- convert_to_matrix(data_set[index_loop, ])
# }
#
#
# # check which one are dags
# check_po <- unlist(lapply(graph_mat, test_if_porder)) #  ddandrda::
# data_po <- graph_mat[check_po]
#
# # Overview
# dim(data_set) # [1] 212  17
# length(data_po) # 98




################################################################################
# ML Data Set Preparation
################################################################################
data_all <- listOMLDataSets()

rel_datasets <- data_all %>% filter(status == "active" &
                                     number.of.classes == 2 &
                                     number.of.features < 100 &
                                     number.of.instances < 1000 &
                                     number.of.instances > 100 &
                                     number.of.instances.with.missing.values == 0 &
                                     max.nominal.att.distinct.values < 5
)
flows <- listOMLFlows()

nds = c()
flows_add = flows
flows_add$ndata = NA
for(i in 14978:nrow(flows)){
  run.results = tryCatch(listOMLRunEvaluations(flow.id = flows_add$flow.id[i], limit = 10000), error = function(e) {print(paste("non-numeric argument", input));
                   NaN})
  if(nrow(run.results)> 0){
    flows_add$ndata[i] = length(unique(run.results$task.id))
  }
}

#### flows 2333 (rpart), 2330 (ranger), 2409 (knn), 2337 (xgboost), 2408 (glmnet), 2336(svm), 2317(logit), 2313 lda
test = getOMLFlow(flow.id = 2333)
#test = getOMLTask(task.id = 3729)
#### 4689
flows_for_paper <- c(2333, 2330, 2337, 2408, 2336)
outls = list()
for (i in 1:length(flows_for_paper)) {
  temp <- listOMLRunEvaluations(flow.id = flows_for_paper[i], limit = 10000)
  if (i == 1) {
    datasets = temp$data.name
  } else {
    datasets = intersect(datasets,temp$data.name)
  }
  outls[[i]] = temp
}
data_final <- do.call('rbind', outls)

data_final <-  data_final[data_final$data.name %in% datasets,]

data_final <- data_final %>% group_by(flow.id, data.name) %>% slice(n())

extractDataId = function(taskid){
  tryCatch({res = getOMLTask(task.id = taskid)}, error = function(e){return(NA)})
  return(res$input$data.set$desc$id)
}

data_final$data.id = sapply(data_final$task.id,function(x)extractDataId(taskid = x))

### filter data (man kann neue filter einfach in dem filter statement hinzufügen!)
data_final = data_final %>% 
             group_by(data.id) %>% 
             mutate(count = n()) %>% 
             left_join(data_all, by = "data.id") %>%
             filter((count == length(flows_for_paper)) &
                    (number.of.instances.x > 120) &
                    (number.of.classes == 2)
                    )

runList = getOMLRun(509222)
params  = getOMLRunParList(runList)

hm = listOMLSetup(4689)
hm$full.name
data_final <- data_final[order(data_final$data.name), ]

View(data_final)
dim(data_final) # [1] 1616   30

colnames(data_final)
# [1] "run.id"                        "task.id"
# [3] "setup.id"                      "flow.id"
# [5] "flow.name"                     "flow.version"
# [7] "flow.source"                   "learner.name"
# [9] "data.name"                     "upload.time"
# [11] "area.under.roc.curve"          "average.cost"
# [13] "f.measure"                     "kappa"
# [15] "kb.relative.information.score" "mean.absolute.error"
# [17] "mean.prior.absolute.error"     "number.of.instances"
# [19] "precision"                     "predictive.accuracy"
# [21] "prior.entropy"                 "recall"
# [23] "relative.absolute.error"       "root.mean.prior.squared.error"
# [25] "root.mean.squared.error"       "root.relative.squared.error"
# [27] "total.cost"                    "usercpu.time.millis"
# [29] "usercpu.time.millis.testing"   "usercpu.time.millis.training"

learner_unique <- unique(data_final$learner.name)
length(learner_unique)
learner_unique
# [1] "classif.lda"      "classif.multinom" "classif.ranger"
# [4] "classif.rpart"    "classif.svm"      "classif.xgboost"
# [7] "classif.glmnet"   "classif.kknn"

data_set_unique <- unique(data_final$data.name)
length(data_set_unique) # [1] 202

# Function which converts to to partial orders
convert_to_matrix <- function(single_data_eval) {
  list_learner <- single_data_eval$learner.name
  if (length(list_learner) != length(unique(list_learner))) {
    print(single_data_eval[ ,"data.name"])
    stop("Fehler im obigen Datensatz")
  }
  number_learner <- length(list_learner)
  graph_mat <- matrix(rep(0, number_learner * number_learner),
                      nrow = number_learner)
  rownames(graph_mat) <- colnames(graph_mat) <- list_learner
  diag(graph_mat) <- 1

  for (learner_i in list_learner) {
    learner_base <- single_data_eval[
      which(single_data_eval$learner.name == learner_i),
      -c(1, 2)]
    for (learner_j in list_learner) {
      learner_comp <- single_data_eval[
        which(single_data_eval$learner.name == learner_j),
        -c(1, 2)]
      if (all(learner_base < learner_comp)) {
        graph_mat[learner_i, learner_j] <- 1
      }
    }
  }
  return(graph_mat)
}





######### First data set
data_set_eval_1 <- data_final[, c("data.name", "learner.name",
                              "f.measure", "predictive.accuracy",
                              "area.under.roc.curve",
                              "root.mean.squared.error")]
data_set_eval_1[ ,"root.mean.squared.error"] <-
  1 - data_set_eval_1[ ,"root.mean.squared.error"]

data_set_eval_1 <- data_set_eval_1[data_set_eval_1$learner.name %in%
                                       c("classif.ranger",
                                         "classif.rpart",
                                         "classif.svm",
                                         "classif.xgboost",
                                         "classif.glmnet",
                                         "classif.kknn"),
                               ]

# single_data_eval <- data_set_eval_1[seq(1,6), ]
# convert_to_matrix(single_data_eval)
# list_mat_porders_ml <- rep(NA, length(data_set_unique))
list_mat_porders_ml_1 <- list()
number_classifiers <- 6
for (i in seq(1, length(data_set_unique))) {
  list_mat_porders_ml_1[i] <- list(convert_to_matrix(data_set_eval_1[seq((i - 1) * number_classifiers + 1,
                                                     i * number_classifiers), ]))
}

length(list_mat_porders_ml_1) # 202
length(unique(list_mat_porders_ml_1)) # 170
Reduce("|", list_mat_porders_ml_1)
Reduce("&", list_mat_porders_ml_1)
Reduce("+", list_mat_porders_ml_1)

# Formal context given by the partial orders in list_mat
fc_ml_porder <- compute_conceptual_scaling(input_porder = list_mat_porders_ml_1)

# VC dimension is upper bound
# see package oofos on github.com/schollmeyer
ml_porder_model <- compute_extent_vc_dimension(fc_ml_porder)
vc_fc_ml_porder <- gurobi::gurobi(ml_porder_model, list(outputflag = 0))

vc_fc_ml_porder$objval # Vc dimension is 11






######### Second data set -> less performance measures than first data set
data_set_eval_2 <- data_final[, c("data.name", "learner.name",
                                  "f.measure", "predictive.accuracy",
                                  "area.under.roc.curve")]

data_set_eval_2 <- data_set_eval_2[data_set_eval_2$learner.name %in%
                                     c("classif.ranger",
                                       "classif.rpart",
                                       "classif.svm",
                                       "classif.xgboost",
                                       "classif.glmnet",
                                       "classif.kknn"),
]

list_mat_porders_ml_2 <- list()
number_classifiers <- 6
for (i in seq(1, length(data_set_unique))) {
  list_mat_porders_ml_2[i] <- list(convert_to_matrix(data_set_eval_2[seq((i - 1) * number_classifiers + 1,
                                                                       i * number_classifiers), ]))
}

length(list_mat_porders_ml_2) # 202
length(unique(list_mat_porders_ml_2)) # 170
Reduce("|", list_mat_porders_ml_2)
Reduce("&", list_mat_porders_ml_2)
Reduce("+", list_mat_porders_ml_2)

# Formal context given by the partial orders in list_mat
fc_ml_porder_2 <- compute_conceptual_scaling(input_porder = list_mat_porders_ml_2)

# VC dimension is upper bound
# see package oofos on github.com/schollmeyer
ml_porder_model_2 <- compute_extent_vc_dimension(fc_ml_porder_2)
vc_fc_ml_porder_2 <- gurobi::gurobi(ml_porder_model_2, list(outputflag = 0))

vc_fc_ml_porder_2$objval # Vc dimension is 12







######### Third data set -> less classifiers than data set one
data_set_eval_3 <- data_final[, c("data.name", "learner.name",
                                  "f.measure", "predictive.accuracy",
                                  "area.under.roc.curve",
                                  "root.mean.squared.error")]
data_set_eval_3[ ,"root.mean.squared.error"] <-
  1 - data_set_eval_3[ ,"root.mean.squared.error"]

data_set_eval_3 <- data_set_eval_3[data_set_eval_3$learner.name %in%
                                     c("classif.ranger",
                                       "classif.rpart",
                                       "classif.svm",
                                       "classif.xgboost"),
]

# single_data_eval <- data_set_eval_1[seq(1,6), ]
# convert_to_matrix(single_data_eval)
# list_mat_porders_ml <- rep(NA, length(data_set_unique))
number_classifiers <- 4
list_mat_porders_ml_3 <- list()
for (i in seq(1, length(data_set_unique))) {
  list_mat_porders_ml_3[i] <- list(convert_to_matrix(data_set_eval_3[seq((i - 1) * number_classifiers + 1,
                                                                         i * number_classifiers), ]))
}

length(list_mat_porders_ml_3) # 202
length(unique(list_mat_porders_ml_3)) # 67
Reduce("|", list_mat_porders_ml_3)
Reduce("&", list_mat_porders_ml_3)
Reduce("+", list_mat_porders_ml_3)

# Formal context given by the partial orders in list_mat
fc_ml_porder_3 <- compute_conceptual_scaling(input_porder = list_mat_porders_ml_3)

# VC dimension is upper bound
# see package oofos on github.com/schollmeyer
ml_porder_model_3 <- compute_extent_vc_dimension(fc_ml_porder_3)
vc_fc_ml_porder_3 <- gurobi::gurobi(ml_porder_model_3, list(outputflag = 0))

vc_fc_ml_porder_3$objval # Vc dimension is 7






######### Forth data set -> less classifiers than data set one,
# but more than data set three
data_set_eval_4 <- data_final[, c("data.name", "learner.name",
                                  "f.measure", "predictive.accuracy",
                                  "area.under.roc.curve",
                                  "root.mean.squared.error")]
data_set_eval_4[ ,"root.mean.squared.error"] <-
  1 - data_set_eval_4[ ,"root.mean.squared.error"]

data_set_eval_4 <- data_set_eval_4[data_set_eval_4$learner.name %in%
                                     c("classif.ranger",
                                       "classif.rpart",
                                       "classif.svm",
                                       "classif.xgboost",
                                       "classif.glmnet"),
]

# single_data_eval <- data_set_eval_1[seq(1,6), ]
# convert_to_matrix(single_data_eval)
# list_mat_porders_ml <- rep(NA, length(data_set_unique))
list_mat_porders_ml_4 <- list()
number_classifiers <- 5
for (i in seq(1, length(data_set_unique))) {
  list_mat_porders_ml_4[i] <- list(convert_to_matrix(data_set_eval_4[seq((i - 1) * number_classifiers + 1,
                                                                         i * number_classifiers), ]))
}

length(list_mat_porders_ml_4) # 202
length(unique(list_mat_porders_ml_4)) # 131
Reduce("|", list_mat_porders_ml_4)
Reduce("&", list_mat_porders_ml_4)
Reduce("+", list_mat_porders_ml_4)

# Formal context given by the partial orders in list_mat
fc_ml_porder_4 <- compute_conceptual_scaling(input_porder = list_mat_porders_ml_4)

# VC dimension is upper bound
# see package oofos on github.com/schollmeyer
ml_porder_model_4 <- compute_extent_vc_dimension(fc_ml_porder_4)
vc_fc_ml_porder_4 <- gurobi::gurobi(ml_porder_model_4, list(outputflag = 0))

vc_fc_ml_porder_4$objval # Vc dimension is 10





######### Fith data set -> less classifiers than data set one,
# but more than data set three
data_set_eval_5 <- data_final[, c("data.name", "learner.name",
                                  "f.measure", "predictive.accuracy",
                                  "area.under.roc.curve",
                                  "root.mean.squared.error")]
data_set_eval_5[ ,"root.mean.squared.error"] <-
  1 - data_set_eval_5[ ,"root.mean.squared.error"]

data_set_eval_5 <- data_set_eval_5[data_set_eval_5$learner.name %in%
                                     c("classif.ranger",
                                       "classif.rpart",
                                       "classif.svm",
                                       "classif.xgboost",
                                       "classif.kknn"),
]

# single_data_eval <- data_set_eval_1[seq(1,6), ]
# convert_to_matrix(single_data_eval)
# list_mat_porders_ml <- rep(NA, length(data_set_unique))
list_mat_porders_ml_5 <- list()
number_classifiers <- 5
for (i in seq(1, length(data_set_unique))) {
  list_mat_porders_ml_5[i] <- list(convert_to_matrix(data_set_eval_5[seq((i - 1) * number_classifiers + 1,
                                                                         i * number_classifiers), ]))
}

length(list_mat_porders_ml_5) # 202
length(unique(list_mat_porders_ml_5)) # 131
Reduce("|", list_mat_porders_ml_5)
Reduce("&", list_mat_porders_ml_5)
Reduce("+", list_mat_porders_ml_5)

# Formal context given by the partial orders in list_mat
fc_ml_porder_5 <- compute_conceptual_scaling(input_porder = list_mat_porders_ml_5)

# VC dimension is upper bound
# see package oofos on github.com/schollmeyer
ml_porder_model_5 <- compute_extent_vc_dimension(fc_ml_porder_5)
vc_fc_ml_porder_5 <- gurobi::gurobi(ml_porder_model_5, list(outputflag = 0))

vc_fc_ml_porder_5$objval # Vc dimension is 9

################################################################################
# Descriptive Analysis
################################################################################

# Compute the ufg
start_time <- Sys.time()
ufg_depth <- ddandrda::compute_ufg_depth_porder(list_mat_porders_ml,
                                         list_mat_porders_ml,
                                         min_card_ufg = as.integer(2),
                                         max_card_ufg = as.integer(4))
end_time <- Sys.time()
end_time - start_time


# descriptive part



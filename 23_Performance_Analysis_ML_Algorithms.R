################################################################################
# Set Up
################################################################################

# remove.packages("ddandrda)
# install.packages("devtools")
# devtools::install_github("hannahblo/ddandrda")


# library(ddandrda)
# library(oofos)
library(OpenML)
library(dplyr)
library(gurobi)
library(farff)


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
# for(i in 14978:nrow(flows)){
#   run.results = tryCatch(listOMLRunEvaluations(flow.id = flows_add$flow.id[i], limit = 10000), error = function(e) {print(paste("non-numeric argument", input));
#                    NaN})
#   if(nrow(run.results)> 0){
#     flows_add$ndata[i] = length(unique(run.results$task.id))
#   }
# }

#### flows 2333 (rpart), 2330 (ranger), 2409 (knn), 2337 (xgboost), 2408 (glmnet), 2336(svm), 2317(logit), 2313 lda
test = getOMLFlow(flow.id = 2333)
#test = getOMLTask(task.id = 3729)
#### 4689
flows_for_paper <- c(2333, 2330, 2317, 2337, 2408, 2336,  2409)
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
  print(taskid)
  if (length(tryCatch({res = getOMLTask(task.id = taskid)}, error = function(e){return(NA)})) <= 1) {
    return(NA)
  }
  return(res$input$data.set$desc$id)
}

data_final$data.id = sapply(data_final$task.id,function(x)extractDataId(taskid = x))
data_final[which(is.na(data_final$data.id)), ]
if (length(which(is.na(data_final$data.id))) > 0) {
  data_final <- data_final[-which(is.na(data_final$data.id)), ]
}

# setwd()
# saveRDS(data_final, "data_final.rds")
# data_final <- readRDS("data_final.rds")
# saveRDS(data_all, "data_all.rds")
# data_all <- readRDS("data_all.rds")
# flows_for_paper <- c(2333, 2330, 2317, 2337, 2408, 2336,  2409)


# runList = getOMLRun(509222)
# params  = getOMLRunParList(runList)
#
# hm = listOMLSetup(4689)
# hm$full.name


################################################################################
# Data Set for complete computation
################################################################################

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





################################################################################
# Data Set for complete computation of Scal
################################################################################
### filter data (man kann neue filter einfach in dem filter statement hinzuf?gen!)
data_final_filter = data_final %>%
  group_by(data.id) %>%
  mutate(count = n()) %>%
  left_join(data_all, by = "data.id") %>%
  filter((count == length(flows_for_paper)) &
           (number.of.instances.x > 550) &
           (number.of.classes == 2)
  )
data_final_filter <- data_final_filter[order(data_final_filter$data.name), ]

learner_unique <- unique(data_final_filter$learner.name)
data_set_unique <- unique(data_final_filter$data.name)

# View(data_final_filter)
# dim(data_final_filter)
# colnames(data_final_filter)
# length(learner_unique)
# learner_unique
# length(data_set_unique)

data_set_eval_1 <- data_final_filter[, c("data.name", "learner.name",
                              "f.measure", "predictive.accuracy",
                              "area.under.roc.curve",
                              "root.mean.squared.error")]
data_set_eval_1[ ,"root.mean.squared.error"] <-
  1 - data_set_eval_1[ ,"root.mean.squared.error"]

data_set_eval_1 <- data_set_eval_1[data_set_eval_1$learner.name %in%
                                       c("classif.ranger",
                                         "classif.rpart",
                                         "classif.multinom",
                                         "classif.kknn",
                                         "classif.glmnet"),
                               ]

# single_data_eval <- data_set_eval_1[seq(1,6), ]
# convert_to_matrix(single_data_eval)
# list_mat_porders_ml <- rep(NA, length(data_set_unique))
list_mat_porders_ml_1 <- list()
number_classifiers <- 5
for (i in seq(1, length(data_set_unique))) {
  list_mat_porders_ml_1[i] <- list(convert_to_matrix(data_set_eval_1[seq((i - 1) * number_classifiers + 1,
                                                     i * number_classifiers), ]))
}

length(list_mat_porders_ml_1) # 47
length(unique(list_mat_porders_ml_1)) # 40
Reduce("|", list_mat_porders_ml_1)
Reduce("&", list_mat_porders_ml_1)
Reduce("+", list_mat_porders_ml_1)

# Formal context given by the partial orders in list_mat
fc_ml_porder <- compute_conceptual_scaling(input_porder = list_mat_porders_ml_1)

# VC dimension is upper bound
# see package oofos on github.com/schollmeyer
ml_porder_model <- compute_extent_vc_dimension(fc_ml_porder)
vc_fc_ml_porder <- gurobi::gurobi(ml_porder_model, list(outputflag = 0))

vc_fc_ml_porder$objval # Vc dimension is 7







################################################################################
# Data Set for sampling from Scal
################################################################################
### filter data (man kann neue filter einfach in dem filter statement hinzuf?gen!)
data_final_filter = data_final %>%
  group_by(data.id) %>%
  mutate(count = n()) %>%
  left_join(data_all, by = "data.id") %>%
  filter((count == length(flows_for_paper)) &
           (number.of.instances.x > 100) &
           (number.of.classes == 2)
  )
data_final_filter <- data_final_filter[order(data_final_filter$data.name), ]

learner_unique <- unique(data_final_filter$learner.name)
data_set_unique <- unique(data_final_filter$data.name)

# View(data_final_filter)
# dim(data_final_filter)
# colnames(data_final_filter)
# length(learner_unique)
# learner_unique
# length(data_set_unique)

data_set_eval_2 <- data_final_filter[, c("data.name", "learner.name",
                                         "f.measure", "predictive.accuracy",
                                         "area.under.roc.curve",
                                         "root.mean.squared.error")]
data_set_eval_2[ ,"root.mean.squared.error"] <-
  1 - data_set_eval_2[ ,"root.mean.squared.error"]

data_set_eval_2 <- data_set_eval_2[data_set_eval_2$learner.name %in%
                                     c("classif.ranger",
                                       "classif.rpart",
                                       "classif.multinom",
                                       "classif.kknn",
                                       "classif.glmnet"),
]

# single_data_eval <- data_set_eval_1[seq(1,6), ]
# convert_to_matrix(single_data_eval)
# list_mat_porders_ml <- rep(NA, length(data_set_unique))
list_mat_porders_ml_2 <- list()
number_classifiers <- 5
for (i in seq(1, length(data_set_unique))) {
  list_mat_porders_ml_2[i] <- list(convert_to_matrix(data_set_eval_2[seq((i - 1) * number_classifiers + 1,
                                                                         i * number_classifiers), ]))
}

length(list_mat_porders_ml_2) # 160
length(unique(list_mat_porders_ml_2)) # 112
Reduce("|", list_mat_porders_ml_2)
Reduce("&", list_mat_porders_ml_2)
Reduce("+", list_mat_porders_ml_2)

# Formal context given by the partial orders in list_mat
fc_ml_porder <- compute_conceptual_scaling(input_porder = list_mat_porders_ml_2)

# VC dimension is upper bound
# see package oofos on github.com/schollmeyer
ml_porder_model <- compute_extent_vc_dimension(fc_ml_porder)
vc_fc_ml_porder <- gurobi::gurobi(ml_porder_model, list(outputflag = 0))

vc_fc_ml_porder$objval # Vc dimension is 9



################################################################################
# Computation of the depth
################################################################################

# Compute the ufg with computing all Scal elements
list_mat_porders_ml <- list_mat_porders_ml_1
unique_list_mat_porders_ml <- unique(list_mat_porders_ml_1)
start_time <- Sys.time()
ufg_depth <- compute_ufg_depth_porder(porder_observed = list_mat_porders_ml,
                                      porder_depth = unique_list_mat_porders_ml,
                                      min_card_ufg = as.integer(2),
                                      max_card_ufg = as.integer(7))
total_time <- Sys.time() - start_time
saveRDS(ufg_depth, "ufg_depth.rds")
saveRDS(total_time, "total_time.rds")


# Compute the approximation by sampling Scal
list_mat_porders_ml <- list_mat_porders_ml_2
unique_list_mat_porders_ml <- unique(list_mat_porders_ml_2)
approx_ufg_depth <- approx_ufg_depth_porder(stop_criteria = list(
  number_iterations = Inf,
  number_premises = Inf,
  max_time = as.numeric(300)),
  porder_observed = list_mat_porders_ml,
  porder_depth = unique_list_mat_porders_ml,
  min_card_ufg = as.integer(2),
  max_card_ufg = as.integer(9))
saveRDS(approx_ufg_depth, "ufg_depth_approx.rds")



################################################################################
# Descriptive Analysis
################################################################################
library(hasseDiagram) # for hasse

depth_value <- ufg_depth
unique_porders <- unique_list_mat_porders_ml

print(paste0("The minimal value is ", min(depth_value)))
print(paste0("The maximal value is ", max(depth_value)))
print(paste0("The mean value is ", mean(depth_value)))
print(paste0("The standard deviation is ", sd(depth_value)))
print(paste0("The median is ", median(depth_value)))
print(paste0("The number of depth value duplicates are ", length(depth_value) -
               length(unique(depth_value))))

### Distribution of Depth Values
pdf("plots_depth_approx.pdf", onefile = TRUE)
boxplot(depth_value, main = "Boxplot of the depth values")
dev.off()

### Maximal Value
maximal_value <- max(depth_value)
max_depth_index <- sort(depth_value, index.return = TRUE, decreasing = TRUE)$ix[seq(1, 8)]
names_columns <- colnames(unique_porders[[1]])
item_number <- dim(unique_porders[[1]])[1]

setwd()
pdf("plots_maximal_values_approx.pdf", onefile = TRUE)
for (i in max_depth_index) {
  mat <- matrix(as.logical(unique_porders[[i]]), ncol = item_number)
  colnames(mat) <- rownames(mat) <- names_columns
  hasse(mat)
}
dev.off()


### Minimaö Values
maximal_value <- min(depth_value)
min_depth_index <- sort(depth_value, index.return = TRUE, decreasing = FALSE)$ix[seq(1, 8)]
names_columns <- colnames(unique_porders[[1]])
item_number <- dim(unique_porders[[1]])[1]

setwd()
pdf("plots_minimal_values_approx.pdf", onefile = TRUE)
for (i in min_depth_index) {
  mat <- matrix(as.logical(unique_porders[[i]]), ncol = item_number)
  colnames(mat) <- rownames(mat) <- names_columns
  hasse(mat)
}
dev.off()



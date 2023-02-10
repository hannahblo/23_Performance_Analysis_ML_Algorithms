################################################################################
# Set Up
################################################################################

### Information about packages ddandrda. This package is under developement on
# git. Installation can be done by:
# remove.packages("ddandrda")
# install.packages("devtools")
# devtools::install_github("hannahblo/ddandrda")

### Information about packages oofos. This package is under developement on git.
# Note that to install this package, the R-package gurobi is needed. This is an
#
# Further, the RBGL Package is needed
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("RBGL")
# After installation of gurobi and RBGL nochmal nachsehen, the following
# library(gurobi)
# install.packages("devtools")
# devtools::install_github("schollmeyer/oofos")

### All the other R-packages are on CRAN packages (07.02.2023)

library(ddandrda)
library(oofos)
library(gurobi)
library(OpenML)
library(dplyr)
library(farff)
library(ggplot2)
library(reshape2)
library(hasseDiagram)



################################################################################
# Functions needed to perform the analysis
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


# Function to compute the weights of the fc
get_weighted_representation <- function(x, y = rep(1, dim(x)[1])) {
  ## computes weighthed representation of a data matrix x with duplicated rows,
  ##  returns unique(x) together with counts: how often appears the column,
  # mean_y: mean of y in the set of the duplicated columns
  xd <- data.frame(cbind(x, y))
  names(xd)[1] <- "v1"
  v1 <- "v1"
  p <- dim(x)[2]
  result <- as.matrix(plyr::ddply(xd, names(xd[(1:p)]), dplyr::summarise, count = length(v1), mean.y = mean(y), sum.y = sum(y)))
  x_weighted <- result[, (1:p)]
  colnames(x_weighted) <- colnames(x)
  return(list(x_weighted = x_weighted, y_weighted = result[, p + 3], mean_y = result[, p + 2], counts = result[, p + 1]))
}


#' Test if new observation lies in conclusion based on nominal scaling
#'
#' @description Based on nominal scaling this function tests if a further
#' object lies in the conclusion of a premise
#'
#' @param subset (vector of (0,1)): 1 represents that the point is within the
#' subset
#' @param obj_porder_obs (nominal): observation to test if lies in conclusion
#' @param info_list (containing data_values): nominal attribute of each
#' observation  (same length as premise)
#'
#' @return logical value. TRUE if obj_nominal_obs lies in the conclusion, else
#' FALSE is returened
test_porder_in_concl <- function(subset, obj_porder_obs,
                                 info_list = NULL) {
  number_item <- dim(subset[[1]])[[1]]
  subset_intersect <- 1 * Reduce("&", subset,
                                 init = matrix(1,
                                               nrow = number_item,
                                               ncol = number_item
                                 )
  )
  subset_union <- 1 * Reduce("|", subset,
                             init = matrix(0,
                                           nrow = number_item,
                                           ncol = number_item
                             )
  )

  number_obj_porder <- length(obj_porder_obs)

  in_conclusion <- rep(FALSE, length(obj_porder_obs))

  for (index_obj_porder in seq_along(1:number_obj_porder)) {
    if (all(subset_intersect <= obj_porder_obs[[index_obj_porder]]) &&
        all(obj_porder_obs[[index_obj_porder]] <= subset_union)) {
      in_conclusion[index_obj_porder] <- TRUE
    }
  }
  return(in_conclusion)
}


################################################################################
# ML Data Set Preparation
################################################################################
# Load the data set from OpenML (see https://www.openml.org/)
data_all <- OpenML::listOMLDataSets()
rel_datasets <- data_all %>% filter(status == "active" &
                                     number.of.classes == 2 &
                                     number.of.features < 100 &
                                     number.of.instances < 1000 &
                                     number.of.instances > 100 &
                                     number.of.instances.with.missing.values == 0 &
                                     max.nominal.att.distinct.values < 5
)
# flows <- OpenML::listOMLFlows()
#
# nds = c()
# flows_add = flows
# flows_add$ndata = NA
# for(i in 14978:nrow(flows)){
#   run.results = tryCatch(listOMLRunEvaluations(flow.id = flows_add$flow.id[i], limit = 10000), error = function(e) {print(paste("non-numeric argument", input));
#                    NaN})
#   if(nrow(run.results)> 0){
#     flows_add$ndata[i] = length(unique(run.results$task.id))
#   }
# }

#### flows 2333 (rpart), 2330 (ranger), 2409 (knn), 2337 (xgboost), 2408 (glmnet), 2336(svm), 2317(logit), 2313 lda
# test = getOMLFlow(flow.id = 2333)
#test = getOMLTask(task.id = 3729)
#### 4689

# @Malter: was sind diese flows?
flows_for_paper <- c(2333, 2330, 2317, 2337, 2408, 2336,  2409)
outls <- list()
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


#@Malte: was war das nochmal?
extractDataId <- function(taskid){
  print(taskid)
  if (length(tryCatch({res = getOMLTask(task.id = taskid)}, error = function(e){return(NA)})) <= 1) {
    return(NA)
  }
  return(res$input$data.set$desc$id)
}
data_final$data.id = sapply(data_final$task.id,function(x)extractDataId(taskid = x))


# data_final[which(is.na(data_final$data.id)), ]
# taskid = 3019: Data set has been deactivated. Thus delete this one
if (length(which(is.na(data_final$data.id))) > 0) {
  data_final <- data_final[-which(is.na(data_final$data.id)), ]
}

# Saving the objects
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




# ################################################################################
# # 1. Data Set for complete computation of Scal
# ################################################################################
# ### filter data (man kann neue filter einfach in dem filter statement hinzuf?gen!)
# data_final_filter = data_final %>%
#   group_by(data.id) %>%
#   mutate(count = n()) %>%
#   left_join(data_all, by = "data.id") %>%
#   filter((count == length(flows_for_paper)) &
#            (number.of.instances.x > 550) &
#            (number.of.classes == 2)
#   )
# data_final_filter <- data_final_filter[order(data_final_filter$data.name), ]
#
# learner_unique <- unique(data_final_filter$learner.name)
# data_set_unique <- unique(data_final_filter$data.name)
#
# # View(data_final_filter)
# # dim(data_final_filter)
# # colnames(data_final_filter)
# # length(learner_unique)
# # learner_unique
# # length(data_set_unique)
#
# data_set_eval_1 <- data_final_filter[, c("data.name", "learner.name",
#                               "f.measure", "predictive.accuracy",
#                               "area.under.roc.curve",
#                               "root.mean.squared.error")]
# data_set_eval_1[ ,"root.mean.squared.error"] <-
#   1 - data_set_eval_1[ ,"root.mean.squared.error"]
#
# data_set_eval_1 <- data_set_eval_1[data_set_eval_1$learner.name %in%
#                                        c("classif.ranger",
#                                          "classif.rpart",
#                                          "classif.multinom",
#                                          "classif.kknn",
#                                          "classif.glmnet"),
#                                ]
#
# # single_data_eval <- data_set_eval_1[seq(1,6), ]
# # convert_to_matrix(single_data_eval)
# # list_mat_porders_ml <- rep(NA, length(data_set_unique))
# list_mat_porders_ml_1 <- list()
# number_classifiers <- 5
# for (i in seq(1, length(data_set_unique))) {
#   list_mat_porders_ml_1[i] <- list(convert_to_matrix(data_set_eval_1[seq((i - 1) * number_classifiers + 1,
#                                                      i * number_classifiers), ]))
# }
#
# length(list_mat_porders_ml_1) # 47
# length(unique(list_mat_porders_ml_1)) # 40
# Reduce("|", list_mat_porders_ml_1)
# Reduce("&", list_mat_porders_ml_1)
# Reduce("+", list_mat_porders_ml_1)
#
# # Formal context given by the partial orders in list_mat
# fc_ml_porder <- compute_conceptual_scaling(input_porder = list_mat_porders_ml_1)
#
# # VC dimension is upper bound
# # see package oofos on github.com/schollmeyer
# ml_porder_model <- compute_extent_vc_dimension(fc_ml_porder)
# vc_fc_ml_porder <- gurobi::gurobi(ml_porder_model)
#
# vc_fc_ml_porder$objval # Vc dimension is 7
#





################################################################################
# Data Set
################################################################################
### filter data (man kann neue filter einfach in dem filter statement hinzuf?gen!)
data_final_filter = data_final %>%
  group_by(data.id) %>%
  mutate(count = n()) %>%
  left_join(data_all, by = "data.id") %>%
  filter((count == length(flows_for_paper)) &
           (number.of.instances.x > 450) &
           (number.of.instances.x < 10000) & # 10000 500
           (number.of.classes == 2)
  )
data_final_filter <- data_final_filter[order(data_final_filter$data.name), ]

# learner_unique <- unique(data_final_filter$learner.name)
# data_set_unique <- unique(data_final_filter$data.name)

# View(data_final_filter)
# dim(data_final_filter)
# colnames(data_final_filter)
# length(learner_unique)
# learner_unique
# length(data_set_unique)

# We are only interested in the following performance measures
data_set_eval <- data_final_filter[, c("data.name", "learner.name",
                                         "f.measure", "predictive.accuracy",
                                         "area.under.roc.curve",
                                         "root.mean.squared.error")]
# In contrast to the other performance measure, lower root.mean.squared.error
# is better.
data_set_eval[ ,"root.mean.squared.error"] <-
  1 - data_set_eval[ ,"root.mean.squared.error"]

# We are only interested in the following classifiers
data_set_eval <- data_set_eval[data_set_eval$learner.name %in%
                                     c("classif.ranger",
                                       "classif.rpart",
                                       "classif.multinom",
                                       "classif.kknn",
                                       "classif.glmnet"), ]

# single_data_eval <- data_set_eval_1[seq(1,6), ]
# convert_to_matrix(single_data_eval)
# list_mat_porders_ml <- rep(NA, length(data_set_unique))
list_mat_porders_ml <- list()
number_classifiers <- 5
for (i in seq(1, length(unique(data_final_filter$data.name)))) {
  list_mat_porders_ml[i] <- list(convert_to_matrix(data_set_eval[seq((i - 1) * number_classifiers + 1,
                                                                         i * number_classifiers), ]))
}


# saveRDS(list_mat_porders_ml, "list_mat_porders_ml.rds")
# saveRDS(data_set_eval, "data_set_eval.rds")



################################################################################
# Descriptive analysis of existence of edges (Step 1: not with depth function)
################################################################################

### Which edge exists
length(list_mat_porders_ml) # 80
length(unique(list_mat_porders_ml)) # 58
Reduce("|", list_mat_porders_ml)
Reduce("&", list_mat_porders_ml)
Reduce("+", list_mat_porders_ml)


edges <- Reduce("+", list_mat_porders_ml)
colnames(edges) <- rownames(edges) <- c("multinom", "ranger", "rpart", "glmnet", "kknn")
df_edge_exist <- melt(edges)
df_edge_exist <- df_edge_exist[df_edge_exist$value != 0, ]

ggplot(df_edge_exist, aes(x = Var2, y = Var1)) +
  geom_raster(aes(fill = value)) +
  scale_fill_gradient(low = "lightcyan1", high = "darkcyan") +
  labs(x = "is below", y = "is above") +
  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 0.3),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12, vjust = -1),
        axis.title.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_blank())
# TODO automatisches absichern






################################################################################
# Computation of the ufg-depth
################################################################################

### Compute the VC dimension
# Formal context given by the partial orders in list_mat
fc_ml_porder <- ddandrda::compute_conceptual_scaling(input_porder = list_mat_porders_ml)
ml_porder_model <- oofos::compute_extent_vc_dimension(fc_ml_porder)
vc_fc_ml_porder <- gurobi::gurobi(ml_porder_model)
vc <- vc_fc_ml_porder$objval # 8



### Compute the ufg-depth
# Preparation of the computation, needed as input of
porder_all <- ddandrda::compute_all_partial_orders(5, list = FALSE, complemented = TRUE)
list_porder_all <- ddandrda::compute_all_partial_orders(5, list = TRUE, complemented = FALSE)

data_context <- get_weighted_representation(fc_ml_porder) # duplication
n_row_context <- nrow(data_context$x_weighted)
count_dup <- data_context$counts
number_obs <- sum(data_context$counts)
# TODO
list_ml_porder_unique <- ddandrda::convert_context_to_list(data_context$x_weighted[ ,(1:25)],  complemented = FALSE)

whole_context <- rbind(data_context$x_weighted, porder_all) # context of all posets
index <- which(!duplicated(whole_context))
whole_context <- whole_context[index,]



# Computation of S, see article (1)
start_time <- Sys.time()
ufg_premises <- oofos::enumerate_ufg_premises(whole_context, n_row_context) # das ist seltsam
total_time <- Sys.time() - start_time

# saveRDS(total_time, "total_time.rds")
# saveRDS(ufg_premises, "ufg_premises.rds")
# length(ufg_premises)


# ufg depth computation
emp_prob <- count_dup / number_obs
depth_ufg <- rep(0, length(list_ml_porder_unique))
constant_c <- 0

for (i in 1:length(ufg_premises)) {
  # print(paste0("Iteration ", i,  " of ", dim(ufg_premises)[1]))
  index_premise <- ufg_premises[[i]]
  if (length(index_premise) < 2) {
    print(paste0("cardinaltiy ufg_premise is ", length(index_premise)))
  }

  prod_emp_ufg <- prod(emp_prob[index_premise])
  concl_ufg <- test_porder_in_concl(list_ml_porder_unique[index_premise], list_ml_porder_unique) * 1

  depth_ufg <- depth_ufg + concl_ufg * prod_emp_ufg
  constant_c <- constant_c + prod_emp_ufg
}

depth_value <- depth_ufg / constant_c


# Adding duplicate values
depth_value_all <- c()
list_data_all <- vector("list", sum(count_dup))
saving <- 1
for (i in 1:length(depth_value)) {
  for (j in 1:count_dup[i]) {
    list_data_all[[saving]] <- list_ml_porder_unique[[i]]
    saving <- saving + 1
  }
  depth_value_all <- append(depth_value_all, rep(depth_value[i], count_dup[i]))

}


saveRDS(constant_c, "constant_c.rds")
saveRDS(depth_ufg, "ufg_depth.rds")
saveRDS(vc, "vc.rds")
saveRDS(depth_value_all, "depth_values.rds")


# ufg_depths <- compute_ufg_depth(data_context$x_weighted, fc_ml_porder,
#                                 ufgs,data_context$counts)
# start_time <- Sys.time()
# ufg_depth <- compute_ufg_depth_porder(porder_observed = list_mat_porders_ml,
#                                       porder_depth = unique_list_mat_porders_ml,
#                                       min_card_ufg = as.integer(2),
#                                       max_card_ufg = as.integer(vc))





# ################################################################################
# # Approx: Data Set for sampling from Scal
# ################################################################################
# ### filter data (man kann neue filter einfach in dem filter statement hinzuf?gen!)
# data_final_filter = data_final %>%
#   group_by(data.id) %>%
#   mutate(count = n()) %>%
#   left_join(data_all, by = "data.id") %>%
#   filter((count == length(flows_for_paper)) &
#            (number.of.instances.x > 100) &
#            (number.of.classes == 2)
#   )
# data_final_filter <- data_final_filter[order(data_final_filter$data.name), ]
#
# learner_unique <- unique(data_final_filter$learner.name)
# data_set_unique <- unique(data_final_filter$data.name)
#
# # View(data_final_filter)
# # dim(data_final_filter)
# # colnames(data_final_filter)
# # length(learner_unique)
# # learner_unique
# # length(data_set_unique)
#
# data_set_eval_2 <- data_final_filter[, c("data.name", "learner.name",
#                                          "f.measure", "predictive.accuracy",
#                                          "area.under.roc.curve",
#                                          "root.mean.squared.error")]
# data_set_eval_2[ ,"root.mean.squared.error"] <-
#   1 - data_set_eval_2[ ,"root.mean.squared.error"]
#
# data_set_eval_2 <- data_set_eval_2[data_set_eval_2$learner.name %in%
#                                      c("classif.ranger",
#                                        "classif.rpart",
#                                        "classif.multinom",
#                                        "classif.kknn",
#                                        "classif.glmnet"),
# ]
#
# # single_data_eval <- data_set_eval_1[seq(1,6), ]
# # convert_to_matrix(single_data_eval)
# # list_mat_porders_ml <- rep(NA, length(data_set_unique))
# list_mat_porders_ml_2 <- list()
# number_classifiers <- 5
# for (i in seq(1, length(data_set_unique))) {
#   list_mat_porders_ml_2[i] <- list(convert_to_matrix(data_set_eval_2[seq((i - 1) * number_classifiers + 1,
#                                                                          i * number_classifiers), ]))
# }
#
# length(list_mat_porders_ml_2) # 160
# length(unique(list_mat_porders_ml_2)) # 112
# Reduce("|", list_mat_porders_ml_2)
# Reduce("&", list_mat_porders_ml_2)
# Reduce("+", list_mat_porders_ml_2)
#
# # Formal context given by the partial orders in list_mat
# fc_ml_porder <- compute_conceptual_scaling(input_porder = list_mat_porders_ml_2)
#
# # VC dimension is upper bound
# # see package oofos on github.com/schollmeyer
# ml_porder_model <- compute_extent_vc_dimension(fc_ml_porder)
# vc_fc_ml_porder <- gurobi::gurobi(ml_porder_model, list(outputflag = 0))
#
# vc_fc_ml_porder$objval # Vc dimension is 9
#


################################################################################
# Computation of the depth
################################################################################

# Compute the ufg with computing all Scal elements
# list_mat_porders_ml <- list_mat_porders_ml_1
# vc <- 8
# unique_list_mat_porders_ml <- unique(list_mat_porders_ml_1)
# start_time <- Sys.time()
# ufg_depth <- compute_ufg_depth_porder(porder_observed = list_mat_porders_ml,
#                                       porder_depth = unique_list_mat_porders_ml,
#                                       min_card_ufg = as.integer(2),
#                                       max_card_ufg = as.integer(vc))
# total_time <- Sys.time() - start_time
# saveRDS(ufg_depth, "ufg_depth.rds")
# saveRDS(total_time, "total_time.rds")
# ufg_depth <- readRDS("ufg_depth_complete.rds")


# # Compute the approximation by sampling Scal
# list_mat_porders_ml <- list_mat_porders_ml_2
# unique_list_mat_porders_ml <- unique(list_mat_porders_ml_2)
# approx_ufg_depth <- approx_ufg_depth_porder(stop_criteria = list(
#   number_iterations = Inf,
#   number_premises = Inf,
#   max_time = as.numeric(300)),
#   porder_observed = list_mat_porders_ml,
#   porder_depth = unique_list_mat_porders_ml,
#   min_card_ufg = as.integer(2),
#   max_card_ufg = as.integer(9))
# saveRDS(approx_ufg_depth, "ufg_depth_approx.rds")


################################################################################
# Notes on computation time
################################################################################
## 1. Data Set for complete computation of Scal
# computation time of the ufg complete, not approximated:
# Time difference of 1.52838 hours

## 2. Data Set for complete computation of Scal
# computation time of the ufg complete, not approximated:
#

################################################################################
# Descriptive Analysis -> duplicates
################################################################################
names_columns <- c("multinom", "ranger", "rpart", "glmnet", "kknn")
item_number <- dim(list_ml_porder_unique[[1]])[1]

sort(count_dup, index.return = TRUE, decreasing = TRUE)
mat <- matrix(as.logical(list_ml_porder_unique[[27]]), ncol = item_number)
colnames(mat) <- rownames(mat) <- names_columns
hasse(t(mat), parameters = list(arrow = "backward", shape = "roundrect"))


################################################################################
# Descriptive Analysis: ufg depth
################################################################################
print(paste0("The minimal value is ", min(depth_value_all)))
print(paste0("The maximal value is ", max(depth_value_all)))
print(paste0("The mean value is ", mean(depth_value_all)))
print(paste0("The standard deviation is ", sd(depth_value_all)))
print(paste0("The median is ", median(depth_value_all)))
print(paste0("The number of depth value duplicates (reduced by duplicates given by the data) are ", length(depth_value) -
               length(unique(depth_value))))

### Distribution of Depth Values
pdf("boxplot_depth.pdf", onefile = TRUE)
boxplot(depth_value_all, main = "Boxplot of the depth values")
dev.off()

# ### Maximal Value
# maximal_value <- max(depth_value_all)
# max_depth_index <- sort(depth_value_all, index.return = TRUE, decreasing = TRUE)$ix[seq(1, 8)]
# max_depth_index_int <- sort(depth_value_all, index.return = TRUE, decreasing = TRUE)$ix[1]
#
#
# setwd()
# pdf("plots_maximal_values.pdf", onefile = TRUE)
# for (i in max_depth_index) {
#   mat <- matrix(as.logical(list_data_all[[i]]), ncol = item_number)
#   colnames(mat) <- rownames(mat) <- names_columns
#   # print(mat * 1)
#   # hasse(mat) plots the graph from top to bottom, with smallest value at bottom
#   # -> change and set arrow to "backward"
#   hasse(t(mat), parameters = list(arrow = "backward", shape = "roundrect"))
# }
# dev.off()
#
#
# ### Minimaö Values
# minimal_value <- min(depth_value_all)
# min_depth_index <- sort(depth_value_all, index.return = TRUE, decreasing = FALSE)$ix[seq(1, 8)]
#
# setwd()
# pdf("plots_minimal_values.pdf", onefile = TRUE)
# for (i in min_depth_index) {
#   mat <- matrix(as.logical(list_data_all[[i]]), ncol = item_number)
#   colnames(mat) <- rownames(mat) <- names_columns
#   # print(mat * 1)
#   # hasse(mat) plots the graph from top to bottom, with smallest value at bottom
#   # -> change and set arrow to "backward"
#   hasse(t(mat), parameters = list(arrow = "backward", shape = "roundrect"))
# }
# dev.off()



## All partial orders
max_depth_index <- sort(depth_value_all, index.return = TRUE, decreasing = TRUE)$ix
pdf("plots_all_from_highest_to_lowest.pdf", onefile = TRUE)
for (i in max_depth_index) {
  mat <- matrix(as.logical(list_data_all[[i]]), ncol = item_number)
  colnames(mat) <- rownames(mat) <- names_columns
  # print(mat * 1)
  # hasse(mat) plots the graph from top to bottom, with smallest value at bottom
  # -> change and set arrow to "backward"
  hasse(t(mat), parameters = list(arrow = "backward", shape = "roundrect"))
}
dev.off()



## Intersections (high to low)
max_depth_index <- sort(depth_value_all, index.return = TRUE, decreasing = TRUE)$ix
pdf("plots_intersect_from_highest_to_lowest.pdf", onefile = TRUE)
for (i in 1:length(max_depth_index)) {
  intersect <- matrix(rep(TRUE, item_number * item_number), ncol = item_number)
  for (j in seq(1, i)) {
    intersect <- intersect & matrix(as.logical(list_data_all[[max_depth_index[j]]]), ncol = item_number)
  }
  colnames(intersect) <- rownames(intersect) <- names_columns
  # print(mat * 1)
  # hasse(mat) plots the graph from top to bottom, with smallest value at bottom
  # -> change and set arrow to "backward"
  hasse(t(intersect), parameters = list(arrow = "backward", shape = "roundrect"))
}
dev.off()



## Intersections (low to high)
min_depth_index <- sort(depth_value_all, index.return = TRUE, decreasing = FALSE)$ix
pdf("plots_intersect_from_lowest_to_highest.pdf", onefile = TRUE)
for (i in 1:length(min_depth_index)) {
  intersect <- matrix(rep(TRUE, item_number * item_number), ncol = item_number)
  for (j in seq(1, i)) {
    intersect <- intersect & matrix(as.logical(list_data_all[[min_depth_index[j]]]), ncol = item_number)
  }
  colnames(intersect) <- rownames(intersect) <- names_columns
  # print(mat * 1)
  # hasse(mat) plots the graph from top to bottom, with smallest value at bottom
  # -> change and set arrow to "backward"
  hasse(t(intersect), parameters = list(arrow = "backward", shape = "roundrect"))
}
dev.off()




# library(hasseDiagram) # for hasse
#
# depth_value <- ufg_depth$ufg_depth
# unique_porders <-  unique(list_mat_porders_ml_1)
# saveRDS(depth_value, "depth_values.rds")
#
# print(paste0("The minimal value is ", min(depth_value)))
# print(paste0("The maximal value is ", max(depth_value)))
# print(paste0("The mean value is ", mean(depth_value)))
# print(paste0("The standard deviation is ", sd(depth_value)))
# print(paste0("The median is ", median(depth_value)))
# print(paste0("The number of depth value duplicates are ", length(depth_value) -
#                length(unique(depth_value))))
#
# ### Distribution of Depth Values
# pdf("boxplot_depth.pdf", onefile = TRUE)
# boxplot(depth_value, main = "Boxplot of the depth values")
# dev.off()
#
# ### Maximal Value
# maximal_value <- max(depth_value)
# max_depth_index <- sort(depth_value, index.return = TRUE, decreasing = TRUE)$ix[seq(1, 8)]
# max_depth_index_int <- sort(depth_value, index.return = TRUE, decreasing = TRUE)$ix[1]
# unique_porders[[max_depth_index_int]]
# names_columns <- colnames(unique_porders[[1]])
# item_number <- dim(unique_porders[[1]])[1]
#
# setwd()
# pdf("plots_maximal_values.pdf", onefile = TRUE)
# for (i in max_depth_index) {
#   mat <- matrix(as.logical(unique_porders[[i]]), ncol = item_number)
#   colnames(mat) <- rownames(mat) <- names_columns
#   # print(mat * 1)
#   # hasse(mat) plots the graph from top to bottom, with smallest value at bottom
#   # -> change and set arrow to "backward"
#   hasse(t(mat), parameters = list(arrow = "backward", shape = "roundrect"))
# }
# dev.off()
#
#
# ### Minimaö Values
# minimal_value <- min(depth_value)
# min_depth_index <- sort(depth_value, index.return = TRUE, decreasing = FALSE)$ix[seq(1, 8)]
# names_columns <- colnames(unique_porders[[1]])
# item_number <- dim(unique_porders[[1]])[1]
#
# setwd()
# pdf("plots_minimal_values.pdf", onefile = TRUE)
# for (i in min_depth_index) {
#   mat <- matrix(as.logical(unique_porders[[i]]), ncol = item_number)
#   colnames(mat) <- rownames(mat) <- names_columns
#   # print(mat * 1)
#   # hasse(mat) plots the graph from top to bottom, with smallest value at bottom
#   # -> change and set arrow to "backward"
#   hasse(t(mat), parameters = list(arrow = "backward", shape = "roundrect"))
# }
# dev.off()
#
# # sample
# # sample size von oben verkleinern
# # resampling ansatz
#

library(dplyr)

# 定义树结构
create_tree <- function() {
  list(
    split_feature = NULL,
    split_value = NULL,
    leaf_value = NULL,
    tree_left = NULL,
    tree_right = NULL
  )
}

# 计算预测值
calc_predict_value <- function(tree, dataset) {
  if (!is.null(tree$leaf_value)) {
    return(tree$leaf_value)
  } else if (dataset[[tree$split_feature]] <= tree$split_value) {
    return(calc_predict_value(tree$tree_left, dataset))
  } else {
    return(calc_predict_value(tree$tree_right, dataset))
  }
}

# 描述树结构
describe_tree <- function(tree) {
  if (is.null(tree$tree_left) && is.null(tree$tree_right)) {
    return(paste0("{leaf_value:", tree$leaf_value, "}"))
  }
  left_info <- describe_tree(tree$tree_left)
  right_info <- describe_tree(tree$tree_right)
  paste0("{split_feature:", tree$split_feature, 
         ",split_value:", tree$split_value,
         ",left_tree:", left_info,
         ",right_tree:", right_info, "}")
}

# 随机森林分类器
random_forest_classifier <- function(dataset, targets, n_estimators = 10, max_depth = -1, 
                                     min_samples_split = 2, min_samples_leaf = 1,
                                     min_split_gain = 0.0, colsample_bytree = NULL, 
                                     subsample = 0.8, random_state = NULL) {
  
  if (!is.null(random_state)) set.seed(random_state)
  
  if (is.null(colsample_bytree)) {
    colsample_bytree <- ncol(dataset)
  } else if (colsample_bytree == "sqrt") {
    colsample_bytree <- floor(sqrt(ncol(dataset)))
  } else if (colsample_bytree == "log2") {
    colsample_bytree <- floor(log2(ncol(dataset)))
  }
  
  max_depth <- if(max_depth == -1) Inf else max_depth
  
  random_state_stages <- sample(1:n_estimators, n_estimators)
  
  trees <- list()
  feature_importances <- list()
  
  for (rs in random_state_stages) {
    tree <- build_tree(dataset, targets, max_depth, min_samples_split, min_samples_leaf,
                       min_split_gain, colsample_bytree, subsample, rs)
    trees[[length(trees) + 1]] <- tree
  }
  
  list(trees = trees, feature_importances = feature_importances)
}

# 构建单棵决策树
build_tree <- function(dataset, targets, max_depth, min_samples_split, min_samples_leaf,
                       min_split_gain, colsample_bytree, subsample, random_state) {
  set.seed(random_state)
  
  subcol_index <- sample(colnames(dataset), colsample_bytree)
  sample_indices <- sample(nrow(dataset), size = floor(subsample * nrow(dataset)), replace = FALSE)
  
  dataset_stage <- dataset[sample_indices, subcol_index, drop = FALSE]
  targets_stage <- targets[sample_indices, , drop = FALSE]
  
  build_single_tree(dataset_stage, targets_stage, 0, max_depth, min_samples_split, 
                    min_samples_leaf, min_split_gain)
}

# 递归构建单棵决策树
build_single_tree <- function(dataset, targets, depth = 0, max_depth, min_samples_split, 
                              min_samples_leaf, min_split_gain, node_type = "root") {
  tree <- create_tree()
  
  node_info <- paste(replicate(depth, "  "), collapse = "") # 缩进
  node_info <- paste0(node_info, "Depth ", depth, " (", node_type, "): ")
  node_info <- paste0(node_info, "Samples = ", nrow(dataset), ", ")
  node_info <- paste0(node_info, "Class counts: ", 
                      paste(table(targets$label), collapse = ", "))
  
  print(node_info)
  
  if (length(unique(targets$label)) <= 1 || nrow(dataset) <= min_samples_split) {
    tree$leaf_value <- calc_leaf_value(targets$label)
    print(paste0(node_info, " => Leaf node with value: ", tree$leaf_value))
    return(tree)
  }
  
  if (depth < max_depth) {
    split_info <- choose_best_feature(dataset, targets, min_samples_leaf, min_split_gain)
    
    if (is.null(split_info$feature) || 
        split_info$left_size <= min_samples_leaf || 
        split_info$right_size <= min_samples_leaf ||
        split_info$gain <= min_split_gain) {
      tree$leaf_value <- calc_leaf_value(targets$label)
      print(paste0(node_info, " => Leaf node with value: ", tree$leaf_value))
    } else {
      tree$split_feature <- split_info$feature
      tree$split_value <- split_info$value
      print(paste0(node_info, " => Split on ", split_info$feature, 
                   " <= ", round(split_info$value, 4)))
      
      left_indices <- dataset[[split_info$feature]] <= split_info$value
      tree$tree_left <- build_single_tree(
        dataset[left_indices, , drop = FALSE],
        targets[left_indices, , drop = FALSE],
        depth + 1, max_depth, min_samples_split, min_samples_leaf, min_split_gain,
        node_type = "left"
      )
      tree$tree_right <- build_single_tree(
        dataset[!left_indices, , drop = FALSE],
        targets[!left_indices, , drop = FALSE],
        depth + 1, max_depth, min_samples_split, min_samples_leaf, min_split_gain,
        node_type = "right"
      )
    }
  } else {
    tree$leaf_value <- calc_leaf_value(targets$label)
    print(paste0(node_info, " => Leaf node (max depth) with value: ", tree$leaf_value))
  }
  
  return(tree)
}

# 选择最佳特征
choose_best_feature <- function(dataset, targets, min_samples_leaf, min_split_gain) {
  best_split <- list(gain = Inf)
  
  for (feature in colnames(dataset)) {
    unique_values <- if (length(unique(dataset[[feature]])) <= 100) {
      sort(unique(dataset[[feature]]))
    } else {
      quantile(dataset[[feature]], probs = seq(0, 1, length.out = 100))
    }
    
    for (split_value in unique_values) {
      left_indices <- dataset[[feature]] <= split_value
      left_targets <- targets$label[left_indices]
      right_targets <- targets$label[!left_indices]
      
      if (length(left_targets) < min_samples_leaf || length(right_targets) < min_samples_leaf) next
      
      split_gain <- calc_gini(left_targets, right_targets)
      
      if (split_gain < best_split$gain) {
        best_split <- list(
          feature = feature,
          value = split_value,
          gain = split_gain,
          left_size = length(left_targets),
          right_size = length(right_targets)
        )
      }
    }
    print(paste("Feature:", feature, "Best gain:", split_gain))
  }
  
  best_split
}

# 计算叶子节点值
calc_leaf_value <- function(targets) {
  names(which.max(table(targets)))
}

# 计算基尼指数
calc_gini <- function(left_targets, right_targets) {
  calc_gini_single <- function(targets) {
    prob <- table(targets) / length(targets)
    1 - sum(prob^2)
  }
  
  (length(left_targets) * calc_gini_single(left_targets) + 
     length(right_targets) * calc_gini_single(right_targets)) / 
    (length(left_targets) + length(right_targets))
}

# 预测函数
predict_rf <- function(model, dataset) {
  sapply(1:nrow(dataset), function(i) {
    row <- dataset[i, , drop = FALSE]
    predictions <- sapply(model$trees, function(tree) calc_predict_value(tree, row))
    names(which.max(table(predictions)))
  })
}

# 主函数
main <- function() {
  df <- read.csv("C:/Users/lucian.xu/Desktop/wine.txt",header=T)
  df <- df[df$label %in% c(1, 2), ]
  df <- df[sample(nrow(df)), ]
  
  feature_list <- c("Alcohol", "Malic.acid", "Ash")
  
  train_count <- floor(1 * nrow(df))
  
  model <- random_forest_classifier(
    df[1:train_count, feature_list], 
    df[1:train_count, "label", drop = FALSE],
    n_estimators = 1,
    max_depth = 3,
    min_samples_split = 2,
    min_samples_leaf = 1,
    min_split_gain = 0.0,
    colsample_bytree = "1",
    subsample = 1,
    random_state = 66
  )
  #print(model)
  
  #train_pred <- predict_rf(model, df[1:train_count, feature_list])
  #test_pred <- predict_rf(model, df[(train_count+1):nrow(df), feature_list])
  
  #print(paste("Train accuracy:", mean(train_pred == df$label[1:train_count])))
  #print(paste("Test accuracy:", mean(test_pred == df$label[(train_count+1):nrow(df)])))
}

# 运行主函数
main()
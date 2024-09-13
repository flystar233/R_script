library(dplyr)
# 随机森林函数
random_forest <- function(X, y, n_trees = 100, max_depth = Inf, min_samples_split = 2, min_samples_leaf = 1, mtry = floor(sqrt(ncol(X)))) {
  forest <- list()
  n_features <- ncol(X)
  for (i in 1:n_trees) {
    # 有放回抽样（Bootstrap）
    sample_indices <- sample(1:nrow(X), nrow(X), replace = TRUE)
    X_sample <- X[sample_indices, ]
    y_sample <- y[sample_indices,, drop=FALSE]
    # 随机选择特征子集
    feature_indices <- sample(1:n_features, mtry)
    X_sample_subset <- X_sample[, feature_indices, drop = FALSE]
    # 构建决策树
    tree <- build_decision_tree(X_sample_subset, y_sample, max_depth, min_samples_split, min_samples_leaf)
    
    # 存储树和对应的特征索引
    forest[[i]] <- list(tree = tree, feature_indices = feature_indices)
  }
  
  return(forest)
}

# 随机森林预测函数
predict_random_forest <- function(forest, new_data) {
  predictions <- sapply(forest, function(tree_info) {
    tree <- tree_info$tree
    feature_indices <- tree_info$feature_indices
    new_data_subset <- new_data[, feature_indices, drop = FALSE]
    predict_tree(tree, new_data_subset)
  })
  
  # 多数投票
  return(names(which.max(table(predictions))))
}

# 计算随机森林的预测准确率
calculate_accuracy <- function(forest, X_test, y_test) {
  predictions <- sapply(1:nrow(X_test), function(i) predict_random_forest(forest, X_test[i, , drop = FALSE]))
  accuracy <- sum(predictions == y_test) / length(y_test)
  return(accuracy)
}

# 主函数：构建决策树
build_decision_tree <- function(X, y, max_depth = Inf, min_samples_split = 2, min_samples_leaf = 1) {
  data <- cbind(X, y)
  features <- colnames(X)
  target <- colnames(y)[1]
  
  build_tree_recursive <- function(data, depth = 0) {
    # 检查停止条件
    if (nrow(data) < min_samples_split || depth == max_depth || length(unique(data[[target]])) == 1 || nrow(data) <= min_samples_leaf * 2) {
      return(list(
        type = "leaf",
        class = names(which.max(table(data[[target]]))),
        prob = max(table(data[[target]])) / nrow(data),
        samples = nrow(data)
      ))
    }
    
    # 寻找最佳分割
    best_split <- find_best_split(data, features, target, min_samples_leaf)
    
    if (is.null(best_split)) {
      return(list(
        type = "leaf",
        class = names(which.max(table(data[[target]]))),
        prob = max(table(data[[target]])) / nrow(data),
        samples = nrow(data)
      ))
    }
    
    # 分割数据
    left_data <- data[data[[best_split$feature]] <= best_split$value, ]
    right_data <- data[data[[best_split$feature]] > best_split$value, ]
    
    # 递归构建左右子树
    left_branch <- build_tree_recursive(left_data, depth + 1)
    right_branch <- build_tree_recursive(right_data, depth + 1)
    
    # 返回节点信息
    return(list(
      type = "node",
      feature = best_split$feature,
      value = best_split$value,
      gini = best_split$gini,
      samples = nrow(data),
      left = left_branch,
      right = right_branch
    ))
  }
  
  return(build_tree_recursive(data))
}

# 辅助函数：寻找最佳分割点
find_best_split <- function(data, features, target, min_samples_leaf) {
  best_gini <- Inf
  best_split <- NULL
  
  for (feature in features) {
    if (is.numeric(data[[feature]])) {
      # 对连续变量进行排序并找到中点
      sorted_values <- sort(unique(data[[feature]]))
      split_points <- (sorted_values[-1] + sorted_values[-length(sorted_values)]) / 2
      
      for (split in split_points) {
        left <- data[[target]][data[[feature]] <= split]
        right <- data[[target]][data[[feature]] > split]
        
        # 检查分割后的子节点是否满足最小样本数要求
        if (length(left) < min_samples_leaf || length(right) < min_samples_leaf) {
          next
        }
        
        gini <- (length(left) * calculate_gini(left) + length(right) * calculate_gini(right)) / nrow(data)
        
        if (gini < best_gini) {
          best_gini <- gini
          best_split <- list(feature = feature, value = split, gini = gini)
        }
      }
    } else {
      # 对分类变量，考虑所有可能的二分法
      levels <- unique(data[[feature]])
      for (level in levels) {
        left <- data[[target]][data[[feature]] == level]
        right <- data[[target]][data[[feature]] != level]
        
        # 检查分割后的子节点是否满足最小样本数要求
        if (length(left) < min_samples_leaf || length(right) < min_samples_leaf) {
          next
        }
        
        gini <- (length(left) * calculate_gini(left) + length(right) * calculate_gini(right)) / nrow(data)
        
        if (gini < best_gini) {
          best_gini <- gini
          best_split <- list(feature = feature, value = level, gini = gini)
        }
      }
    }
  }
  
  return(best_split)
}

# 辅助函数：计算基尼不纯度
calculate_gini <- function(y) {
  if (length(y) == 0) return(0)
  p <- table(y) / length(y)
  return(1 - sum(p^2))
}

# 打印决策树结构
print_tree <- function(tree, indent = "") {
  if (tree$type == "leaf") {
    cat(sprintf("%sLeaf: class = %s, probability = %.2f, samples = %d\n", 
                indent, tree$class, tree$prob, tree$samples))
  } else {
    cat(sprintf("%sNode: feature = %s, split value = %.2f, gini = %.4f, samples = %d\n", 
                indent, tree$feature, tree$value, tree$gini, tree$samples))
    cat(sprintf("%sLeft branch:\n", indent))
    print_tree(tree$left, paste0(indent, "  "))
    cat(sprintf("%sRight branch:\n", indent))
    print_tree(tree$right, paste0(indent, "  "))
  }
}

# 预测函数
predict_tree <- function(tree, new_data) {
  if (tree$type == "leaf") {
    return(tree$class)
  }
  
  if (is.numeric(new_data[[tree$feature]])) {
    if (new_data[[tree$feature]] <= tree$value) {
      return(predict_tree(tree$left, new_data))
    } else {
      return(predict_tree(tree$right, new_data))
    }
  } else {
    if (new_data[[tree$feature]] == tree$value) {
      return(predict_tree(tree$left, new_data))
    } else {
      return(predict_tree(tree$right, new_data))
    }
  }
}

data = iris|>select(Sepal.Length,Sepal.Width,Species)|>filter(Species!="setosa")
forest <- random_forest(data[1:2],data[3], n_trees = 2, max_depth = 5, min_samples_split = 2, min_samples_leaf = 1)
print_tree(forest[[1]]$tree)
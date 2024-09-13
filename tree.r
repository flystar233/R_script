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

# 使用示例
# X <- data.frame(feature1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#                 feature2 = c("A", "B", "A", "B", "A", "B", "A", "B", "A", "B"),
#                 feature3 = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
# y <- data.frame(target = c("Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No"))
# tree <- build_decision_tree(X, y, max_depth = 3, min_samples_split = 2, min_samples_leaf = 2)
# print_tree(tree)
# 
# # 预测新数据
# new_data <- data.frame(feature1 = 5.5, feature2 = "B", feature3 = 0.55)
# prediction <- predict_tree(tree, new_data)
# print(paste("Prediction:", prediction))

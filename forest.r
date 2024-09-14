library(dplyr)
# 随机森林函数
random_forest <- function(X, 
                          y,
                          n_trees = 100,
                          max_depth = NULL,
                          min_samples_split = 2,
                          min_samples_leaf = 1,
                          mtry = NULL,
                          subsample = ifelse(replace, 1, 0.632),
                          replace = TRUE,
                          seed = NULL) {
  forest <- list()
  n_features <- ncol(X)
  n_samples <- nrow(X)
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if(is.null(max_depth)){
    max_depth <- Inf
  }
  if (is.null(mtry)) {
    mtry <- floor(sqrt(n_features))
  } else if (mtry < 1) {
    mtry <- 1
  } else if (mtry > n_features) {
    mtry <- n_features
  }
  if(!is.data.frame(y)){
    y <- as.data.frame(y)
    colnames(y) <- "target"
  }
  samples_per_tree <- floor(n_samples * subsample)
  
  # 初始化OOB预测矩阵
  oob_predictions <- matrix(NA, nrow = n_samples, ncol = n_trees)
  
  for (i in 1:n_trees) {
    if (!is.null(seed)) {
      set.seed(seed + i)
    }
    
    # 有放回抽样（Bootstrap）
    sample_indices <- sample(1:n_samples, samples_per_tree, replace = replace)
    oob_indices <- setdiff(1:n_samples, unique(sample_indices))
    
    X_sample <- X[sample_indices, ]
    y_sample <- y[sample_indices,, drop=FALSE]
    
    # 随机选择特征子集
    feature_indices <- sample(1:n_features, mtry)
    X_sample_subset <- X_sample[, feature_indices, drop = FALSE]
    
    # 构建决策树
    tree <- build_decision_tree(X_sample_subset, y_sample, max_depth, min_samples_split, min_samples_leaf)
    
    # 存储树和对应的特征索引
    forest[[i]] <- list(tree = tree, feature_indices = feature_indices)
    
    # 对OOB样本进行预测
    if (length(oob_indices) > 0) {
      X_oob <- X[oob_indices, feature_indices, drop = FALSE]
      oob_predictions[oob_indices, i] <- predict_tree(tree, X_oob)
    }
  }
  
  # 计算OOB误差
  oob_error <- mean(sapply(1:n_samples, function(i) {
    row <- oob_predictions[i, ]
    if (all(is.na(row))) {
      return(NA)  # 如果该样本从未作为OOB样本，返回NA
    }
    pred <- names(which.max(table(row[!is.na(row)])))
    return(pred != y[i, 1])
  }), na.rm = TRUE)
  
  return(list(forest = forest,
              oob_error = oob_error,
              mtry = mtry,
              n_trees = n_trees,
              max_depth = max_depth,
              min_samples_split = min_samples_split,
              min_samples_leaf = min_samples_leaf,
              subsample = subsample,
              replace = replace,
              seed = seed))
}

# 随机森林预测函数
predict_random_forest <- function(forest, new_data) {
  # 对每个样本进行预测
  predictions <- sapply(1:nrow(new_data), function(i) {
    sample_predictions <- sapply(forest, function(tree_info) {
      tree <- tree_info$tree
      feature_indices <- tree_info$feature_indices
      new_data_subset <- new_data[i, feature_indices, drop = FALSE]
      predict_tree(tree, new_data_subset)
    })
    # 对每个样本进行多数投票
    names(which.max(table(sample_predictions)))
  })
  
  return(predictions)
}

# 计算随机森林的预测准确率
calculate_accuracy <- function(forest, X_test, y_test) {
  if(!is.data.frame(y_test)){
    y_test <- as.data.frame(y_test)
    colnames(y_test) <- "target"
  }
  predictions <- predict_random_forest(forest, X_test)
  accuracy <- sum(predictions == y_test) / nrow(y_test)
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
tree_info <- function(object, tree_number = 1) {
  if (tree_number > object$n_trees){
    stop(paste("Error: Requesting tree",tree_number, ",but the tree number in this forest is", object$n_trees))
  }
  tree <- object$forest[[tree_number]]$tree
  print_tree <- function(tree, indent = "") {
  if (tree$type == "leaf") {
    cat(sprintf("%s|-- Leaf: class = %s, probability = %.2f, samples = %d\n", 
                indent, tree$class, tree$prob, tree$samples))
  } else {
    cat(sprintf("%s|-- Node: feature = %s, split value = %.2f, gini = %.4f, samples = %d\n", 
                indent, tree$feature, tree$value, tree$gini, tree$samples))
    print_tree(tree$left, paste0(indent, "|   "))
    print_tree(tree$right, paste0(indent, "|   "))
    }
  }
  print_tree(tree)
}

# 预测函数
predict_single <- function(tree, new_data) {
  if (tree$type == "leaf") {
    return(tree$class)
  }
  
  if (is.numeric(new_data[[tree$feature]])) {
    if (new_data[[tree$feature]] <= tree$value) {
      return(predict_single(tree$left, new_data))
    } else {
      return(predict_single(tree$right, new_data))
    }
  } else {
    if (new_data[[tree$feature]] == tree$value) {
      return(predict_single(tree$left, new_data))
    } else {
      return(predict_single(tree$right, new_data))
    }
  }
}

# 预测函数（可处理单个样本或整个数据集）
predict_tree <- function(tree, new_data) {
  if (is.data.frame(new_data) && nrow(new_data) > 1) {
    return(apply(new_data, 1, function(row) predict_single(tree, as.data.frame(t(row)))))
  } else {
    return(predict_single(tree, new_data))
  }
}

#test iris
#data = iris|>select(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width,Species)|>filter(Species!="setosa")
#forest <- random_forest(data[1:4],data[,5], n_trees = 100, max_depth = 5, min_samples_split = 2, min_samples_leaf = 1,replace = TRUE,seed = 42)

# test wine
data = read.csv("wine.txt",header = T)
forest <- random_forest(data[1:13],data[,14], n_trees = 100, max_depth = 5, min_samples_split = 2, min_samples_leaf = 1,replace = TRUE,seed = 42)

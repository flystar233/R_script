# 加载 dplyr 包
library(dplyr)

# 计算叶子节点值
calc_leaf_value <- function(targets) {
    label_counts <- table(targets)
    major_label <- names(which.max(label_counts))
    return(major_label)
}

# 计算基尼指数
calc_gini <- function(left_targets, right_targets) {
    split_gain <- 0
    for (targets in list(left_targets, right_targets)) {
        gini <- 1
        label_counts <- table(targets)
        for (key in names(label_counts)) {
            prob <- label_counts[key] / length(targets)
            gini <- gini - prob ^ 2
        }
        split_gain <- split_gain + (length(targets) / (length(left_targets) + length(right_targets))) * gini
    }
    return(split_gain)
}

# 划分数据集
split_dataset <- function(dataset, targets, split_feature, split_value) {
    left_dataset <- dataset |> filter(!!sym(split_feature) <= split_value)
    left_targets <- targets[dataset |> pull(split_feature) <= split_value]
    right_dataset <- dataset |> filter(!!sym(split_feature) > split_value)
    right_targets <- targets[dataset |> pull(split_feature) > split_value]
    return(list(left_dataset = left_dataset, right_dataset = right_dataset,
                left_targets = left_targets, right_targets = right_targets))
}

# 选择最佳特征
choose_best_feature <- function(dataset, targets) {
    best_split_gain <- 1
    best_split_feature <- NULL
    best_split_value <- NULL
    for (feature in colnames(dataset)) {
        if (length(unique(dataset |> pull(feature))) <= 100) {
            unique_values <- sort(unique(dataset |> pull(feature)))
        } else {
            unique_values <- unique(sapply(seq(0, 100, by = 1), function(x) {
                quantile(dataset |> pull(feature), x / 100)
            }))
        }
        for (split_value in unique_values) {
            print(split_value)
            left_targets <- targets[dataset |> pull(feature) <= split_value,]
            print(left_targets)
            right_targets <- targets[dataset |> pull(feature) > split_value,]
            print(right_targets)
            split_gain <- calc_gini(left_targets, right_targets)
            if (split_gain < best_split_gain) {
                best_split_feature <- feature
                best_split_value <- split_value
                best_split_gain <- split_gain
            }
        }
    }
    return(list(best_split_feature = best_split_feature, best_split_value = split_value,
                best_split_gain = best_split_gain))
}

# 构建单棵决策树
build_single_tree <- function(dataset, targets, depth, max_depth, min_samples_split, min_samples_leaf,
                              min_split_gain, feature_importances) {
    if (n_distinct((targets)) <= 1 || nrow(dataset) <= min_samples_split) {
        leaf_value <- calc_leaf_value(targets)
        return(list(split_feature = NULL, split_value = NULL, leaf_value = leaf_value,
                    tree_left = NULL, tree_right = NULL))
    }
    if (depth < max_depth) {
        best_split <- choose_best_feature(dataset, targets)
        best_split_feature <- best_split$best_split_feature
        best_split_value <- best_split$best_split_value
        best_split_gain <- best_split$best_split_gain
        split_res <- split_dataset(dataset, targets, best_split_feature, best_split_value)
        left_dataset <- split_res$left_dataset
        right_dataset <- split_res$right_dataset
        left_targets <- split_res$left_targets
        right_targets <- split_res$right_targets
        if (nrow(left_dataset) <= min_samples_leaf ||
            nrow(right_dataset) <= min_samples_leaf ||
            best_split_gain <= min_split_gain) {
            leaf_value <- calc_leaf_value(targets)
            return(list(split_feature = NULL, split_value = NULL, leaf_value = leaf_value,
                        tree_left = NULL, tree_right = NULL))
        } else {
            if (is.null(feature_importances[best_split_feature])) {
                feature_importances[best_split_feature] <- 1
            } else {
                feature_importances[best_split_feature] <- feature_importances[best_split_feature] + 1
            }
            tree_left <- build_single_tree(left_dataset, left_targets, depth + 1, max_depth, min_samples_split,
                                           min_samples_leaf, min_split_gain, feature_importances)
            tree_right <- build_single_tree(right_dataset, right_targets, depth + 1, max_depth, min_samples_split,
                                            min_samples_leaf, min_split_gain, feature_importances)
            return(list(split_feature = best_split_feature, split_value = best_split_value,
                        leaf_value = NULL, tree_left = tree_left, tree_right = tree_right))
        }
    } else {
        leaf_value <- calc_leaf_value(targets)
        return(list(split_feature = NULL, split_value = NULL, leaf_value = leaf_value,
                    tree_left = NULL, tree_right = NULL))
    }
}

# 并行构建树的辅助函数
parallel_build_trees <- function(dataset, targets, random_state, max_depth, min_samples_split,
                                 min_samples_leaf, min_split_gain, colsample_bytree, subsample) {
    set.seed(random_state)
    subcol_index <- sample(colnames(dataset), colsample_bytree)
    set.seed(random_state)
    random_sample <- sample(1:nrow(dataset), size = floor(subsample * nrow(dataset)), replace = TRUE)
    dataset_stage <- dataset |> slice(random_sample) |> select(all_of(subcol_index))
    targets_stage <- targets|> slice(random_sample)
    feature_importances <- list()
    tree <- build_single_tree(dataset_stage, targets_stage, 0, max_depth, min_samples_split, min_samples_leaf,
                              min_split_gain, feature_importances)
    return(tree)
}

# 随机森林训练
random_forest_fit <- function(dataset, targets, n_estimators, max_depth, min_samples_split, min_samples_leaf,
                              min_split_gain, colsample_bytree, subsample, random_state) {
    if (n_distinct(targets)!= 2) {
        stop("There must be two classes for targets!")
    }
    if (!is.null(random_state)) {
        set.seed(random_state)
    }
    random_state_stages <- sample(1:n_estimators, n_estimators)
    if (colsample_bytree == "sqrt") {
        colsample_bytree <- floor(sqrt(ncol(dataset)))
    } else if (colsample_bytree == "log2") {
        colsample_bytree <- floor(log(ncol(dataset)))
    } else {
        colsample_bytree <- ncol(dataset)
    }
    trees <- lapply(random_state_stages, function(random_state) {
        parallel_build_trees(dataset, targets, random_state, max_depth, min_samples_split, min_samples_leaf,
                             min_split_gain, colsample_bytree, subsample)
    })
    return(trees)
}

# 预测函数
predict <- function(dataset, trees) {
    res <- list()
    for (i in 1:nrow(dataset)) {
        row <- dataset[i, ]
        pred_list <- list()
        for (tree in trees) {
            pred_list <- c(pred_list, get_predict_value(row, tree))
        }
        pred_label_counts <- table(pred_list)
        pred_label <- names(which.max(pred_label_counts))
        res <- c(res, pred_label)
    }
    return(unlist(res))
}

# 辅助函数，用于递归计算预测值
get_predict_value <- function(row, tree) {
    if (!is.null(tree$leaf_value)) {
        return(tree$leaf_value)
    } else if (row[tree$split_feature] <= tree$split_value) {
        return(get_predict_value(row, tree$tree_left))
    } else {
        return(get_predict_value(row, tree$tree_right))
    }
}
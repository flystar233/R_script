#' @title Calculate the distance from leaf to root per tree.
#' @description Distance of each terminal node of a single tree in a ranger model.
#' @param df the tree info of ranger return by function 'treeInfo'.
#' @return A dataframe with two columns: 'nodeID', 'distance'.
#' @examples
#' \dontrun{
#'   rf = ranger::ranger(Species ~ ., data = iris)
#'   calculate_distance_per_tree(ranger::treeInfo(rf, 1))
#' }
calculate_distance_per_tree <- function(df) {
  # 创建一个数据框来存储每个叶子节点的nodeID和到根节点的距离
  leaf_distances <- data.frame(nodeID = integer(), distance = integer())
  
  # 遍历数据框的每一行
  for (i in 1:nrow(df)) {
    # 如果是叶子节点（terminal == TRUE）
    if (df$terminal[i]) {
      distance <- 0
      current_node <- i
      
      # 向上遍历直到根节点（nodeID == 0）
      while (df$nodeID[current_node] != 0) {
        # 找到当前节点的父节点
        parent_node <- which(df$leftChild == df$nodeID[current_node] | df$rightChild == df$nodeID[current_node])
        
        # 如果没有找到父节点，说明数据可能有误
        if (length(parent_node) == 0) {
          stop("Error: No parent node found for nodeID ", df$nodeID[current_node])
        }
        
        # 更新当前节点为父节点，并增加距离
        current_node <- parent_node
        distance <- distance + 1
      }
      
      # 将计算出的距离和nodeID存储在数据框中
      leaf_distances <- rbind(leaf_distances, data.frame(nodeID = df$nodeID[i], distance = distance))
    }
  }
  
  # 返回包含每个叶子节点的nodeID和到根节点距离的数据框
  return(leaf_distances)
}
#' @title Calculate the distance from leaf to root.
#' @description Distance of each terminal node of all trees in a ranger model is
#'   returned as a three column dataframe with column names: 'nodeID',
#'   'distance', 'treeID'. Note that root node has the node_id = 0.
#' @param model A ranger model
#' @return A dataframe with three columns: 'nodeID', 'distance', 'treeID'.
#' @examples
#' rf = ranger::ranger(Species ~ ., data = iris, num.trees = 100)
#' calculate_leaf_to_root_distance(rf)
#' @export
calculate_leaf_to_root_distance<-function(model) {
    lrd <- lapply(1:model$num.trees, function(x) {
        calculate_distance_per_tree(ranger::treeInfo(model, x))
    })
    lrd_with_id <- lapply(1:length(lrd), function(i) {
      mutate(lrd[[i]], treeID = i)
    })
    lrd_result <- bind_rows(lrd_with_id)
    return(lrd_result)
}
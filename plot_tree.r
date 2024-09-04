library(data.tree)
library(randomForest)
library(ranger)
collapse <- function(x){
    x<-sub(" ","_",x)
    
    return(x)
}

build_tree <- function(df) {
    df$row <- as.numeric(rownames(df))
    colnames(df)<-sapply(colnames(df),collapse)
    # 初始化一个空的列表来存储节点
    nodes <- vector("list", nrow(df))
    
    # 遍历数据框的每一行
    for (i in 1:nrow(df)) {
        # 创建一个新的节点
        if (df$status[i] == -1) {
            nodes[[i]] <- Node$new(name = as.character(df$prediction[i]))
        } else {
            nodes[[i]] <- Node$new(name = paste(df$split_var[i], "<=",df$split_point[i]))
        }
    }
    
    # 再次遍历数据框的每一行，将子节点添加到父节点
    for (i in 1:nrow(df)) {
        if (df$left_daughter[i] != 0) {
            nodes[[i]]$AddChildNode(nodes[[df$left_daughter[i]]])
        }
        if (df$right_daughter[i] != 0) {
            nodes[[i]]$AddChildNode(nodes[[df$right_daughter[i]]])
        }
    }
    
    # 返回根节点
    return(nodes[[1]])
}
main <-function(tree,tree_from="ranger"){
    if (tree_from=="ranger"){
        tree_data<- data.frame("left_daughter" = tree$leftChild+1,
                    "right_daughter" = tree$rightChild+1,
                    "split_var" = tree$splitvarName,
                    "split_point" = tree$splitval,
                    "status"=ifelse(tree$terminal, -1, 1),
                    "prediction"=tree$prediction)

        tree_data$left_daughter[is.na(tree_data$left_daughter)] <- 0
        tree_data$right_daughter[is.na(tree_data$right_daughter)] <- 0
        tree_plot<- build_tree(tree_data)
    }else if (tree_from =="randomForest"){
        tree_plot<- build_tree(tree)
    }
    else{
        stop("tree_from must be ranger or randomForest!")
    }
    return(tree_plot)
}
# test randomForest
tree1 <-getTree(randomForest(iris[,-5], iris[,5], ntree=10), k=3, labelVar=TRUE)
tree_randomForest <- main(tree1,tree_from="randomForest")
SetGraphStyle(tree_randomForest, rankdir = "TB")
SetEdgeStyle(tree_randomForest, arrowhead = "vee", color = "grey35", penwidth = 2)
SetNodeStyle(tree_randomForest, style = "filled,rounded", shape = "box", 
            fontname = "helvetica", tooltip = GetDefaultTooltip)
print(tree_randomForest)
plot(tree_randomForest)

# test ranger
tree2 <- treeInfo(ranger(Species ~ ., data = iris),tree=1)
tree_ranger <- main(tree2,tree_from="ranger")
SetGraphStyle(tree_ranger, rankdir = "TB")
SetEdgeStyle(tree_ranger, arrowhead = "vee", color = "grey35", penwidth = 2)
SetNodeStyle(tree_ranger, style = "filled,rounded", shape = "box", 
            fontname = "helvetica", tooltip = GetDefaultTooltip)
print(tree_ranger)
plot(tree_ranger)

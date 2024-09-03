library(data.tree)
library(randomForest)
collapse <- function(x){
    x<-sub(" ","_",x)
    
    return(x)
}

build_tree <- function(df) {
    
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

# test 
tree <-getTree(randomForest(iris[,-5], iris[,5], ntree=10), 3, labelVar=TRUE)
tree$row <- as.numeric(rownames(tree))
colnames(tree)<-sapply(colnames(tree),collapse)
root <- build_tree(tree)
print(root)
plot(root)

isolationForest<- function(data,
                           num_trees=100,
                           sample_size = min(nrow(data), 256L)
                           max_depth = ceiling(log2(sample_size)),
                           mtry = NULL,
                           num.threads = NULL,
                           seed = NULL,
                           ){

    # Initial check
    if (!is.data.frame(data)) {
        data <- as.data.frame(data)
    }
    if (is.null(seed)) {
        set.seed(as.numeric(Sys.time()))
    } else {
        set.seed(seed)
    }
    column_names  = colnames(data)
    nr = nrow(data)
    sample_fraction = sample_size/nr
    fake_feature = sample.int(nrow(data))

    model <- ranger::ranger(x=data,
                   y=fake_feature,
                   num.trees = num_trees,
                   sample.fraction =sample_fraction,
                   max.depth = max_depth,
                   mtry = mtry,
                   num.threads= num.threads,
                   seed = seed,
                   min.node.size =1L,
                   num.random.splits =1L,
                   splitrule = "extratrees",
                   replace = FALSE
                   )
    terminal_nodes_depth = calculate_leaf_to_root_distance(model)

    phi =sample_size
    tnm = stats::predict(model, data,type = "terminalNodes" num.threads = num.threads)[["predictions"]]
    tnm <- as.data.frame(tnm)
    colnames(tnm) <- as.character(1:ncol(tnm))
    tnm$id <- 1:nrow(tnm)
    tnm <- pivot_longer(tnm, cols = -id, names_to = "id_tree", values_to = "id_node")
    tnm$id_tree <- as.integer(tnm$id_tree)
    tnm$id_node <- as.integer(tnm$id_node)
    obs_depth <- inner_join(terminal_nodes_depth,tnm, by = c("id_tree", "id_node"))



}
#set.seed(123)
#a = data.frame(matrix(rnorm(10),5,2))
#colnames(a) = LETTERS[1:ncol(a)]
#rownames(a) = LETTERS[1:nrow(a)]
#a = cbind(a, xx=c("a","b","a","b","a"))
#c = c(1:10)
#d = setNames(3, LETTERS[5])
#nodes = list(a=a, b=list(c=c, d=d))

.jp = function(...) gsub("//", "/", paste(..., sep="/"))

.cleandf = function(df) {
    df = c(as.list(df), list(stringsAsFactors=F, row.names = rownames(df)))

    do.call(data.frame, lapply(df, function(x) {
        ulx = unlist(x)
        if (is.factor(ulx))
            as.character(ulx)
        else
            ulx
    }))
}

.dimnames = function(X) {
    if (is.data.frame(X))
        list(rownames(X), colnames(X))
    else if (is.vector(X))
        names(X)
    else if (is.null(dimnames(X)))
        rep(list(NULL), length(dim(X)))
    else
        dimnames(X)
}

h5save = function(X, file) {
    node2group = function(file, path, node) {
        if (is.list(node) && !is.data.frame(node)) {
            rhdf5::h5createGroup(file, path)
            for (j in seq_along(node))
                node2group(file, .jp(path, names(node)[j]), node[j])
        } else {
            if (is.data.frame(node))
                node = .cleandf(node)

            rhdf5::h5write(node, file, .jp(path, "value"))

            dn = .dimnames(node)
            for (j in 1:length(dn))
               if (!is.null(dn[[j]]))
                   rhdf5::h5write(dn[[j]], file, .jp(path, paste0("names_", j)))
        }
    }

    rhdf5::h5createFile(file)
    node2group(file, "", X)
}

h5load = function(file, path="/", index=NULL) {
    group2node = function(path, index=NULL) {
        # get all objects in the current path
        objs = file_ls$name[file_ls$group==path]

        if ('value' %in% objs) {
            # read all dimnames
            dim_objs = grep("^names_", objs, value=T)
            dim_idx = sub("^names_", "", dim_objs)
            dim_names = setNames(lapply(dim_objs, function(name) {
                rhdf5::h5read(file, .jp(path, name))
            }), dim_idx)

            # convert character index to numerical
            if (!is.null(index)) {
                num_idx = lapply(seq_along(index), function(i) {
                    name = paste0("names_", i)
                    if (is.character(index[[i]]))
                        match(index[[i]], dim_names[[as.character(i)]])
                    else if (is.numeric(index[[i]]))
                        index[[i]]
                    else if (is.null(index[[i]]))
                        NULL
                    else
                        stop("index needs to be integer, character, or NULL")
                })
            } else
                num_idx = NULL
            
            # load value object w/ numerical index
            if ('COMPOUND' %in% file_ls$dclass[file_ls$group==path]) {
                if (is.null(num_idx[2][[1]]))
                    index2 = TRUE#NULL??
                else
                    index2 = num_idx[[2]]
                value = suppressWarnings(rhdf5::h5read(file, .jp(path, 'value'),
                                         index=num_idx[1])[,index2])
            } else
                value = rhdf5::h5read(file, .jp(path, 'value'), index=num_idx)

            # convert NULL=all from rhdf5 to TRUE=all for R normally
            if (!is.null(index)) {
                length(num_idx) = length(dim(value))
                num_idx = lapply(num_idx, function(x) {
                    if (!is.null(x))
                        x
                    else
                        TRUE
                })

                # subset names with indices
                set_names = lapply(seq_along(dim(value)), function(i) {
                    if (as.character(i) %in% names(dim_names))
                        dim_names[[as.character(i)]][num_idx[i][[1]]]
                })
            } else {
                set_names = lapply(seq_along(dim(value)), function(i) {
                    if (as.character(i) %in% names(dim_names))
                        dim_names[[as.character(i)]]
                })
            }

            # apply indexed subset of dimnames
            if (is.vector(value))
                setNames(value, set_names[[1]])
            else {
                dimnames(value) = set_names
                value
            }

            # check index: no NAs, dimension should be right
            # (maybe rhdf5 error checking is good enough though)

        } else {
            paths = sapply(objs, function(o) .jp(path, o))

            setNames(lapply(seq_along(paths), function(i)
                            group2node(paths[i], index)), objs)
        }
    }

    if (!is.null(index) && !is.list(index))
        index = list(index)

    file_ls = rhdf5::h5ls(file)
    group2node(path, index)
}

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

h5s_save = function(X, file) {
    library(rhdf5)
    node2group = function(file, path, node) {
        if (is.list(node) && !is.data.frame(node)) {
            h5createGroup(file, path)
            for (j in seq_along(node))
                node2group(file, .jp(path, names(node)[j]), node[j])
        } else {
            if (is.data.frame(node))
                node = .cleandf(node)

            h5write(node, file, .jp(path, "value"))

            dn = .dimnames(node)
            for (j in 1:length(dn))
               if (!is.null(dn[[j]]))
                   h5write(dn[[j]], file, .jp(path, paste0("names_", j)))
        }
    }

    h5createFile(file)
    node2group(file, "", X)
}

h5s_load = function(file, path="/", index=NULL) {
    library(rhdf5)
    group2node = function(path, index=NULL) {
        if ('value' %in% file_ls$name[file_ls$group==path]) {
            if (is.character(index)) {
                names_1 = h5read(file, .jp(path, 'names_1'))
                index = match(index, names_1)
            }

            # check index: no NAs, dimension should be right
            # (maybe rhdf5 error checking is good enough though)

            value = h5read(file, .jp(path, 'value'), index=index)
            name_i = function(i) {
                name = paste0("names_", i)
                if (name %in% file_ls$name[file_ls$group==path])
                    h5read(file, .jp(path, name))
                else
                    NULL
            }

            dim_names = lapply(1:length(dim(value)), name_i)
            if (length(dim_names) == 1)
                setNames(value, dim_names[[1]])
            else {
                dimnames(value) = dim_names
                value
            }

        } else {
            name = file_ls$name[file_ls$group==path]
            paths = sapply(name, function(s) .jp(path, s))

            setNames(lapply(seq_along(paths), function(i)
                            group2node(paths[i], index)), name)
        }
    }

    file_ls = h5ls(file)
    group2node(path, index)
}

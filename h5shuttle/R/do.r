#set.seed(123)
#a = data.frame(matrix(rnorm(10),5,2))
#colnames(a) = LETTERS[1:ncol(a)]
#rownames(a) = LETTERS[1:nrow(a)]
#a = cbind(a, xx=c("a","b","a","b","a"))
#c = c(1:10)
#d = setNames(3, LETTERS[5])
#nodes = list(a=a, b=list(c=c, d=d))

.jp = function(...) paste(..., sep="/")

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
    if (is.matrix(X) || is.data.frame(X))
        list(rownames(X), colnames(X))
    else if (is.vector(X))
        names(X)
    else {
        if (is.null(dimnames(X)))
            rep(list(NULL), length(dim(X)))
        else
            dimnames(X)
    }
}

h5s_save = function(X, file) {
    library(rhdf5)
    node2group = function(file, path, node) {
        for (i in seq_along(node)) {
            nval = node[[i]]
            npath = .jp(path, names(node)[i])
            h5createGroup(file, npath)

            if (is.list(nval) && !is.data.frame(nval)) {
                for (j in seq_along(nval))
                    node2group(file, npath, nval[j])
            } else {
                if (is.data.frame(nval))
                    nval = .cleandf(nval)

                h5write(nval, file, .jp(npath,"value"))

                dn = .dimnames(nval)
                for (j in 1:length(dn))
                   if (!is.null(dn[[j]]))
                       h5write(dn[[j]], file, .jp(npath,paste0("names_",j)))
            }
        }
    }

    if (!is.list(X) || is.data.frame(X))
        X = list(root=X)

    h5createFile(file)
    node2group(file, "", X)
}

h5s_load = function(file, path="/") {
    library(rhdf5)
    group2node = function(node) {
        if (is.null(node$value))
            lapply(node, group2node)
        else if (is.null(dim(node$value)))
            setNames(node$value, node$names_1)
        else {
            nidx = sapply(1:length(dim(node$value)),
                          function(i) paste0("names_", i))
            dimnames(node$value) = unname(node[nidx])
            node$value
        }
    }
    group2node(h5read(file, path))
}


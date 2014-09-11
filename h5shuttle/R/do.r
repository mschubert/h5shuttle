#set.seed(123)
#a = data.frame(matrix(rnorm(10),5,2))
#colnames(a) = LETTERS[1:ncol(a)]
#rownames(a) = LETTERS[1:nrow(a)]
#c = c(1:10)
#d = setNames(3, LETTERS[5])
#nodes = list(a=a, b=list(c=c, d=d))

.jp = function(...) paste(..., sep="/")

h5s_save = function(X, file) {
library(rhdf5)
    node2group = function(file, path, node) {
        for (i in seq_along(node)) {
            nval = node[[i]]
            npath = .jp(path, names(node)[i])
            h5createGroup(file, npath)

            if (is.data.frame(nval)) #TODO: we do not support data.frames yet
                nval = as.matrix(nval)

            if (is.list(nval)) {
                for (j in seq_along(nval))
                    node2group(file, npath, nval[j])
            } else {
                nval = as.array(nval)
                h5write(nval, file, .jp(npath,"value"))
                for (j in 1:length(dim(nval)))
                   if (!is.null(dimnames(nval)[[j]]))
                       h5write(dimnames(nval)[[j]], file, .jp(npath,paste0("names_",j)))
            }
        }
    }

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


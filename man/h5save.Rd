\name{h5save}
\alias{h5save}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    h5save
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
h5save(X, file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{file}{
%%     ~~Describe \code{file} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (X, file) 
{
    node2group = function(file, path, node) {
        for (i in seq_along(node)) {
            nval = node[[i]]
            npath = .jp(path, names(node)[i])
            h5createGroup(file, npath)
            if (is.data.frame(nval)) 
                nval = as.matrix(nval)
            if (is.list(nval)) {
                for (j in seq_along(nval)) node2group(file, npath, 
                  nval[j])
            }
            else {
                nval = as.array(nval)
                h5write(nval, file, .jp(npath, "value"))
                for (j in 1:length(dim(nval))) if (!is.null(dimnames(nval)[[j]])) 
                  h5write(dimnames(nval)[[j]], file, .jp(npath, 
                    paste0("names_", j)))
            }
        }
    }
    h5createFile(file)
    node2group(file, "", X)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

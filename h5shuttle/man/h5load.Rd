\name{h5load}
\alias{h5load}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    h5load
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
h5load(file, path = "/")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{path}{
%%     ~~Describe \code{path} here~~
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
function (file, path = "/") 
{
    group2node = function(node) {
        if (is.null(node$value)) 
            lapply(node, group2node)
        else if (is.null(dim(node$value))) 
            setNames(node$value, node$names_1)
        else {
            nidx = sapply(1:length(dim(node$value)), function(i) paste0("names_", 
                i))
            dimnames(node$value) = unname(node[nidx])
            node$value
        }
    }
    group2node(h5read(file, path))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{simp_lin_cpp}
\alias{simp_lin_cpp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simple linear regression in Rcpp
}
\description{
Perform simple linear regression and return a list of statistics
}
\usage{
simp_lin_cpp(x,y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
   A predictor vector of length n
  }
  \item{y}{
   A response vector of length n
  }
}

\value{
%% ~Describe the value returned
Return estimate of coefficients,predicted responses, residuals, standard errors of coefficients and 95 percents confidence intervals of coefficients.
}

\references{
%% ~put references to the literature/web site here ~
}
\author{
Bowen Li
}
\note{
%%  ~~further notes~~
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

\name{pcdcaap}
\alias{pcdcaap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Derive periodontal disease prevalence according to the CDC/AAP case definitions
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pcdcaap(x,y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
This is a dataset of the Recession measured in the prescribed way. Important to note is that the dataset is transposed such that the columns are form study units the while the rows form the variables.However the initial 3 columns should bear the column headings - tooth, side and site. Please see the sample dataset; samplerecession for better understanding.
}
  \item{y}{
This is a dataset of the probing depth measured in the prescribed way. Important to note is that the dataset is transposed such that the columns are form study units the while the rows form the variables.However the initial 3 columns should bear the column headings - tooth, side and site. Please see the sample dataset; samplepd for better understanding.

}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
pcdcaap  returns a text resport with the counts and percentages of the disease cases and the count and the [ercentages of the non-disease.

%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Edwin M. Kagereki

}
\note{
http://www.biomedcentral.com/content/pdf/s12862-014-0170-7.pdf
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
recession<-data(samplerecession)
pd<-data(samplepd)
pcdcaap(recession,pd)

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

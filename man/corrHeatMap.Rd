\name{corrHeatMap}
\alias{corrHeatMap}
\title{Correlation Heat Map}
\usage{
corrHeatMap(df,roundto=2,na.rm=FALSE)
}
\description{
Creates heatmap from data frame or matrix. If data frame, then dummy variables are automatically created.
}
\examples{
hello()
}
heatMap(data(mtcars),roundto=2,na.rm=FALSE,coefText=TRUE)

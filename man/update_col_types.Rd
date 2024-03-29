% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_1_input.R
\name{update_col_types}
\alias{update_col_types}
\title{Update column types}
\usage{
update_col_types(df, types_df)
}
\arguments{
\item{df}{dataframe, containing some columns as specified in types_df}

\item{types_df}{dataframe, containing columns \code{column_name} and \code{column_type}}
}
\value{
dataframe, dataframe with updated column types (by default columns are updated to \code{character} type)
}
\description{
Update column type for each column in dataframe
}
\keyword{column}
\keyword{types}
\keyword{update}

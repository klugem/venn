\name{venn}

\alias{venn}

\title{Draw and display a Venn diagram}

\description{
This function uses a variety of input data to draw and display a Venn diagram with
up to 7 sets.
}

\usage{
venn(x = "", size = 15, ilabels = FALSE, counts = FALSE, snames = c(""),
     zcolor = c(""), transparency = 1, ...)
}

\arguments{
  \item{x}{A single number (of sets), or a metacommand (see details), or a
           list of set values, or dataset containing boolean values.}
  \item{size}{Plot size, in centimeters.}
  \item{ilabels}{Logical, print the labels for each intersection.}
  \item{counts}{Logical, print the counts for each intersection.}
  \item{snames}{An optional parameter containing the names for each set.}
  \item{zcolor}{A vector of colors for the custom zones.}
  \item{transparency}{Transparency for the color specified with \code{zcol}}
  \item{...}{Additional parameters, mainly for the outer borders of the sets}
}

\details{
    
The argument \code{x} can be either:\cr
- a single number of sets, between 1 and 7\cr
- a metacommand to draw custom intersection zones\cr
- a list of values for the different sets: each component is a set,
  and only the first 7 components are processed.\cr
- a dataset of boolean values.\cr
         
A "zone" is a union of set intersections. There are exactly \code{2^k} intersections
in a Venn diagram, where \code{k} is the number of sets. To highlight an entire set,
we need a union of all possible intersections which form that set.

For example, in a 3 sets diagram, the (overall) first set is composed by four
intersections:\cr
\code{100} for what is in the first set but outside sets 2 and outside set 3\cr
\code{101} for the intersection between sets 1 and 3, outside set 2\cr
\code{110} for the intersection between sets 1 and 2, outside set 3\cr
\code{111} for the intersection between all three sets.

A meta-language can be used to define these intersections, using the values of
\code{1} for what is inside the set, \code{0} for what is outside the set, and
\code{-} when its either inside or outside of the set.

The command \code{"1--"} is translated as "display only the first, entire set" is
equivalent with the union of the four intersections \code{"100 + 101 + 110 + 111"}.

The parameter \code{setnames} should have the same length as the number of sets
specified by the parameter \code{x}.

When the parameter \code{x} is used as a metacommand, the number of sets is calculated
as the number of characters in each intersection of the metacommand. One such
character command is \code{"100 + 101 + 110 + 111"} or \code{"1--"}, and all
intersections have exactly three characters.

Argument \code{counts} is used when \code{x} is a list or a dataframe. When a list,
it counts the number of common values for each intersection. When \code{x} is a
dataframe (comprised of exclusively boolean values 0 and 1), it counts the number of
similar rows, for each intersection. \code{counts} is activated by default when \code{x}
is a list or a dataframe.

If \code{x} is a number or a command, argument \code{snames} establishes the names for
the different sets, or in its absence it is taken from \code{LETTERS}. When \code{x} is
a list or a dataframe, \code{snames} is taken from their names.

Via \code{...}, users can specify additional parameters, mainly for the outer borders
of the sets, as specified by \code{par}. All of them are feeded to the function
\code{lines()} which is responsible with the borders.

For up to 3 sets, the shapes can be circular. For more than 3 sets, the shape cannot be
circular, in the case of 4 and 5 sets they are ellipses. For more than 5 sets the
shapes cannot be continous (they might be monotone, but not continous). The 7 sets
diagram is called "Adelaide" (Ruskey, 2005).

The most challenging diagram is the one with 6 sets, where for many years it was
thought a Venn diagram didn't even exist. All diagrams are symetric, except for the
one with 6 sets, where some of the sets have different shapes. The diagram in this
package is an adaptation from Mamakani, K., Myrvold W. and F. Ruskey (2011), 


}


\references{

Ruskey, F. and M. Weston. 2005. \emph{Venn diagrams}. Electronic Journal of Combinatorics,
Dynamic Survey DS vol.5.

Mamakani, K., Myrvold W. and F. Ruskey. 2011. \emph{Generating all Simple Convexly-drawable
Polar Symmetric 6-Venn Diagrams}. International Workshop on Combinatorial Algorithms, Victoria.
LNCS, 7056, 275-286.

}

\examples{

# A simple Venn diagram with 3 sets
venn(3)

# display the first whole set
venn("1--")

# an equivalent command
venn("100 + 110 + 101 + 111")

# another equivalent command
venn(c("100","110","101","111"))

# adding the labels for the intersections
venn("1--", ilab = TRUE)

# using different parameters for the borders
venn(4, lty = 5, col = "navyblue")

# a 5 sets Venn diagram
venn(5)

# a union of two sets
venn("1---- + ----1")

# with different colors
venn("1---- + ----1", zcol = c("red", "blue"), transp = 0.3)

# 6 sets diagram
venn(6)

# 7 sets "Adelaide"
venn(7)

# first set colored
venn("1------", ilab = TRUE)

# artistic version
venn(c("1000000", "0100000", "0010000", "0001000",
       "0000100", "0000010", "0000001", "1111111"))

# when x is a list
set.seed(12345)
x <- list(First = 1:20, Second = 10:30, Third = sample(25:50, 15))
venn(x)

# when x is a dataframe
set.seed(12345)
x <- as.data.frame(matrix(sample(0:1, 150, replace=TRUE), ncol=5))
venn(x)

}


\keyword{functions}

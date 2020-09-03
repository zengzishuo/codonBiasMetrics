\name{calculate_SCUO}
\alias{calculate_SCUO}
\title{calculate SCUO}
\description{
Given a DNA coding sequence, calculate SCUO (Synonymous Codon Usage Order) of the sequence.

Wan, X., et al. (2003). "A new informatics method for measuring synonymous codon usage bias." Intelligent engineering systems through artificial neural networks Volume 13.
}
\usage{
calculate_SCUO(input_sequence)
}
\arguments{
  \item{input_sequence}{a DNA coding sequence}
}
\examples{
my_sequence = generate_a_sequence(20)
calculate_SCUO(input_sequence = my_sequence)

}

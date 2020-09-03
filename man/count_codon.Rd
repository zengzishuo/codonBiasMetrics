\name{count_codon}
\alias{count_codon}
\title{count_codon}
\description{
Given a DNA coding sequence, count the occurrence of each codon and return a 64-element-long vector.
The order of this vector aligns with the list of 64 codons ordered alphabetically.

}
\usage{
count_codon(input_sequence)
}
\arguments{
  \item{input_sequence}{a DNA coding sequence}
}
\examples{
my_sequence = generate_a_sequence(20)
count_codon(input_sequence = my_sequence)

}

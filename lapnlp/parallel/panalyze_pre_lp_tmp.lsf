#!/bin/bash
source ~/.bashrc
shopt -s expand_aliases
# Here is to define the job array
cname=~a
#BSUB -J panalyze_pre_lp_~a_[1-~a]
#BSUB -q normal
#BSUB -o panalyze_pre_lp_~a_%J.%I.out
#BSUB -e panalyze_pre_lp_~a_%J.%I.err

# how many CPU for each job element
#BSUB -n 1
#BSUB -N

#BSUB -R "swp > 1000 && mem > 2500"
##BSUB -R "swp > 1000 && mem > 500 && ut < 25"

# getting to the working dir
workdir=~a
cd $workdir


# you can use $LSB_JOBINDEX to control how to use proprams,  input file or output
# file in the array. The following example shows the first ten jobs will execute
# getargument and the next ten job will execute getargument2, each processor will
# get each different input argument. In your real application, you can use $LSB_JOBINDEX
# to identify your different arguments or input files

./panalyze_pre_lp_"$cname"_$LSB_JOBINDEX.cl


# end of job script

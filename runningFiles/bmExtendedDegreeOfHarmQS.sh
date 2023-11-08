#!/bin/bash -l
#NOTE the -l flag!
#
# Name of the job 
#SBATCH -J bayesian

# Standard out and Standard Error output files
#SBATCH -o logs/%j_%x.output
#SBATCH -e logs/%j_%x.error

#To send emails, set the adcdress below and remove one of the "#" signs.
#SBATCH --mail-user=slack:@al3170

# notify on state change: BEGIN, END, FAIL or ALL
#SBATCH --mail-type=ALL

# Request 5 hours run time MAX, anything over will be KILLED
#SBATCH -t 3-15:0:0

# Put the job in the "work" partition and request FOUR cores
# "work" is the default partition so it can be omitted without issue.
#SBATCH -p tier3 -n 5 -A safe-llm	

# Job memory requirements in MB
#SBATCH --mem=10g

# Job gpu requirements
##SBATCH --gres=gpu:1

# Load R
spack load r@4.2.2

# Your job script goes below this line.  
#
R < CodeRun/ExtendedDegreeOfHarmQS.R --no-save

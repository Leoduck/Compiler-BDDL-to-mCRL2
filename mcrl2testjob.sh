#!/bin/bash -l
#SBATCH --job-name=mcrl2tests
#SBATCH --partition=q40
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=1:00:00
#SBATCH --mem=0
#SBATCH --array=1-103



echo "========= Job started  at `date` =========="

#cd $SLURM_SUBMIT_DIR
cd "translated"

# Get the file to compile for this array task
FILE_TO_COMPILE=$(awk NR==2 "index.txt")
#FILE_TO_COMPILE=$(awk NR==$SLURM_ARRAY_TASK_ID "index.txt")

echo "Compiling $FILE_TO_COMPILE"
# Get the first line of file where the .mcf is specified 
FIRST_LINE=$(head -n 1 "$FILE_TO_COMPILE")
WINMCF1="$(echo "$FIRST_LINE" | awk '{sub(".",""); print $1}').mcf"

# Get the boardsize from filename
COORDS="$(echo "$FILE_TO_COMPILE" |  cut -d '/' -f 3 | cut -c1-3 )"
XMAX="$(echo "$COORDS" | cut -d 'x' -f 1)"
YMAX="$(echo "$COORDS" | cut -d 'x' -f 2)"

#test prints
echo "xmax: $XMAX"
echo "ymax: $YMAX"
echo $WINMCF1

#solving with found mcf and timing - writing results to file
TIMEFORMAT='%R seconds'
time {
mcrl22lps $FILE_TO_COMPILE |lpsparunfold -sBoard -n$YMAX |lpsparunfold -sRow -n$XMAX |lpssuminst | lpsconstelm -s -t | lps2pbes  -f "$WINMCF1"| pbessolve -s2 -zbreadth-first
}
echo "========= Job finished at `date` =========="


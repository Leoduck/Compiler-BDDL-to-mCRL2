#!/bin/bash -l
#SBATCH --job-name=arraytest
#SBATCH --partition=qtest
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=0:01:00
#SBATCH --mem=1G
#SBATCH --array=1-10

echo "========= Job started  at `date` =========="

cd $SLURM_SUBMIT_DIR
#cd "translated"

# Get the file to compile for this array task
#FILE_TO_COMPILE=$(awk NR==2 "index.txt")
echo $SLURM_ARRAY_TASK_ID
FILE_TO_COMPILE=$(awk NR==$SLURM_ARRAY_TASK_ID "index.txt")

echo "Compiling $FILE_TO_COMPILE"
# Get the boardsize from filename
COORDS="$(echo "$FILE_TO_COMPILE" |  cut -d '/' -f 3 | cut -c1-3 )"
XMAX="$(echo "$COORDS" | cut -d 'x' -f 1)"
YMAX="$(echo "$COORDS" | cut -d 'x' -f 2)"

#test prints
echo "xmax: $XMAX"
echo "ymax: $YMAX"

echo "========= Job finished at `date` =========="


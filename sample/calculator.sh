# !/bin/bash 
  
echo -- Program started at $(date +"%T")

echo "Enter Two numbers : "
read a 

echo -- Read first number at $(date +"%T")

read b 
echo -- Read second number at $(date +"%T")

# Had to *do* something
res=`echo $a + $b | bc` 
echo "Result: $res"

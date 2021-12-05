# statistics_file.py
# 
# Compute statistics from an input file

# get filename from user
filename = input("Enter input filename:")

# open input filehandle
infile = open(filename)

# running count of user entered values
running_count = 0
# running sum of user entered values
running_sum = 0
# running min of user entered values
# marked as empty with None
running_min = None
# running max of user entered values
# marked as empty with None
running_max = None

# while loop reads numbers from the file
while True:
    in_str = infile.readline()
    # done reading file when we see an empty input
    if in_str == "":
        break
    # strip whitespace from end of input string
    in_str = in_str.strip()

    # convert to floating point
    in_num = float(in_str)

    running_count = running_count + 1
    running_sum = running_sum + in_num
    # if this is the first value entered it
    # is the minimum value seen so far
    if running_min == None:
        running_min = in_num
    # if it is not the first value entered
    # update running_min if it is less than running_min 
    elif in_num < running_min:
        running_min = in_num
    # short version of similar logic for running_max
    # you can't compare a number to None, but the
    # or operator will return True without evaluating
    # the right if the left is True
    if running_max == None or in_num > running_max:
        running_max = in_num

# This protects against dividine by zero
print("Count",running_count)
if running_count > 0:
    print("Sum",running_sum)
    print("Max",running_max)
    print("Min",running_min)
    ave = running_sum / running_count
    print("Ave",ave)

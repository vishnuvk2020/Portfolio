# starbucks_menu.py
#
# Find the most caloric item and the item with the
# fewest carbohydrates from the starbucks menu

# floating point; running max of calories
max_cals = None
# string; maximum calorie item
max_item = "Not found"
# floating point; running min of carbs
min_carbs = None
# string; minimum carb item
min_item = "Not found"

# open input file
infile = open("Starbucks.txt")

# ignore the first line of the file
infile.readline()

# for loop reads line from the input file
for line in infile:
    line = line.rstrip()
    # split the line on the tab character
    (item, cals, carbs) = line.split("\t")
    # convert cals and carbs to integers
    cals = int(cals)
    carbs = int(carbs)

    if max_cals == None or cals > max_cals:
        max_cals = cals
        max_item = item
    if min_carbs == None or carbs < min_carbs:
        min_carbs = carbs
        min_item = item

# Report the items with the minimum carbs and maximum calories
print("Max Cals:", max_item, max_cals)
print("Min Carbs:", min_item, min_carbs)

# Close file
infile.close()
        

from process import count_cdr
import json

grid = count_cdr()

print ("Done reading!")

with open('/Users/myeong/git/DSSG/with.json', 'w') as outfile:
	json.dump(grid, outfile)
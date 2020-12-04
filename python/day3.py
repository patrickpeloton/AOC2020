f = open("input3.txt", "r")
treeMap = []

for x in f:
    x = x.rstrip('\n')
    y = list(x)
    treeMap.append(y)

def findTrees(treeMap, rightSlope, downSlope):
    treeCount = 0
    x = 0
    y = 0
    numCols = len(treeMap[0])
    numRows = len(treeMap)
    while x < len(treeMap):
        if (treeMap[x][y] == '#'):
            treeCount += 1
        if (y+rightSlope <= numCols):
            y += rightSlope
        if (y+rightSlope > numCols-1):
            y = (y - numCols)
        x += downSlope
    print("for" + str(rightSlope) + "," + str(downSlope)+ ": " + str(treeCount))
    return treeCount

# part 1
print(findTrees(treeMap, 3, 1))

#part 2
t1 = findTrees(treeMap, 1, 1)
t2 = findTrees(treeMap, 3, 1)
t3 = findTrees(treeMap, 5, 1)
t4 = findTrees(treeMap, 7, 1)
t5 = findTrees(treeMap, 1, 2)

print(t1*t2*t3*t4*t5)
f = open("input.txt", "r")
numArray = []
# part 1
for x in f:
    numArray.append(int(x))


for y in numArray:
    res = 2020 - y
    if (numArray.count(res) > 0):
        print(res*y)

# part 2
for z in numArray:
    rez = 2020 - z
    for a in numArray[z:]:
        subrez = rez - a
        if (numArray.count(subrez) > 0):
            print(z*a*subrez)
#part 1
f = open("input2.txt", "r")
res = 0
for x in f:
    x = x.rstrip('\n')
    s = x.split(" ")
    num1 = int(s[0].split("-")[0])
    num2 = int(s[0].split("-")[1])
    character = s[1].split(":")[0]
    string = list(s[2])
    check = string.count(character)
    if (check >= num1 and check <= num2):
        res += 1

print(res)

#part 2
f = open("input2.txt", "r")
res2 = 0
for y in f:
    y = y.rstrip('\n')
    s = y.split(" ")
    num1 = int(s[0].split("-")[0]) - 1
    num2 = int(s[0].split("-")[1]) - 1
    character = s[1].split(":")[0]
    string = list(s[2])
    if (string[num1] == character and string[num2] != character):
        res2 += 1
    elif (string[num2] == character and string[num1] != character):
        res2 += 1

print(res2)
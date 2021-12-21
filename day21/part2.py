from itertools import groupby, product

p1s = int(input().split(': ')[1])
p2s = int(input().split(': ')[1])

dp = {}

ds = [1,2,3]
rolls = [(len(list(v)),k) for k,v in groupby(sorted([sum(x) for x in product(ds,ds,ds)]))]

def update_p(p,d):
    return (((p-1) + d) % 10) + 1

def solve(p1,p2,s1,s2,turn):
    if s1 >= 21:
        return (1,0)
    if s2 >= 21:
        return (0,1)


    w = dp.get((p1,p2,s1,s2,turn))

    if w == None:
        p1w = 0
        p2w = 0
        for (m,i) in rolls:
            p1b = p1
            p2b = p2
            s1b = s1
            s2b = s2
            if turn:
                p1b = update_p(p1,i)
                s1b = s1 + p1b
            else:
                p2b = update_p(p2,i)
                s2b = s2 + p2b

            turnb = not turn
            (w1,w2) = solve(p1b,p2b,s1b,s2b,turnb)
            p1w += m*w1
            p2w += m*w2
            dp[(p1b,p2b,s1b,s2b,turnb)] = (w1,w2)
        return (p1w,p2w)

    return w

(a,b) = solve(p1s,p2s,0,0,True)
print(max(a,b))

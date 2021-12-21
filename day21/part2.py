import sys

p1s = input().split(': ')[1]
p2s = input().split(': ')[1]

print(p1,p2)

dp = {}

def solve(p1,p2,s1,s2,turn):
    if s1 >= 21:
        return (1,0)
    if s2 >= 21:
        return (0,1)


    w = dp.get(p1,p2,s1,s2,turn)
    if w == None:
        print("not cached")
        p1w = 0
        p2w = 0
        for (m,i) in rolls:
            p1b = p1
            p2b = p2
            s1b = s1
            s2b = s2
            if turn:
                p1b = updateP(p1,i)
                s1b = s1 + p1b
            else:
                p2b = updateP(p2,i)
                s2b = s2 + p2b

            turnb = not turn
            (w1,w2) = solve(p1b,p2b,s1b,s2b,turnb)
            dp[p1b,p2b,s1b,s2b,turnb] = (w1,w2)
        return
    else:
        return w

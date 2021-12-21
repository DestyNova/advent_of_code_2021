use std::collections::HashMap;
use std::io::{self, BufRead};

fn main() {
    let stdin = io::stdin();
    let mut iterator = stdin.lock().lines();

    let p1 = split_line(&mut iterator);
    let p2 = split_line(&mut iterator);

    let mut dp = HashMap::new();
    let (a, b) = solve(p1, p2, 0, 0, true, &mut dp);
    println!("{}", std::cmp::max(a, b));
}

fn get_rolls() -> Vec<(usize, usize)> {
    vec![(1, 3), (3, 4), (6, 5), (7, 6), (6, 7), (3, 8), (1, 9)]
}

fn split_line(iterator: &mut io::Lines<io::StdinLock>) -> usize {
    iterator
        .next()
        .unwrap()
        .unwrap()
        .split(": ")
        .collect::<Vec<&str>>()
        .get(1)
        .map(|s| s.parse::<usize>().unwrap())
        .unwrap()
}

fn solve(
    p1: usize,
    p2: usize,
    s1: usize,
    s2: usize,
    turn: bool,
    dp: &mut HashMap<(usize, usize, usize, usize, bool), (usize, usize)>,
) -> (usize, usize) {
    if s1 >= 21 {
        (1, 0)
    } else if s2 >= 21 {
        (0, 1)
    } else {
        match dp.get(&(p1, p2, s1, s2, turn)) {
            Some(w) => *w,

            None => {
                let mut p1w = 0;
                let mut p2w = 0;

                get_rolls().into_iter().for_each(|(m, i)| {
                    let mut p1b = p1;
                    let mut p2b = p2;
                    let mut s1b = s1;
                    let mut s2b = s2;

                    if turn {
                        p1b = update_p(p1, i);
                        s1b = s1 + p1b;
                    } else {
                        p2b = update_p(p2, i);
                        s2b = s2 + p2b;
                    }

                    let turnb = !turn;
                    let (w1, w2) = solve(p1b, p2b, s1b, s2b, turnb, dp);
                    p1w += m * w1;
                    p2w += m * w2;
                    dp.insert((p1b, p2b, s1b, s2b, turnb), (w1, w2));
                });
                (p1w, p2w)
            }
        }
    }
}

fn update_p(p: usize, d: usize) -> usize {
    (((p - 1) + d) % 10) + 1
}
/*


def solve(p1,p2,s1,s2,turn):
turnb = not turn
(w1,w2) = solve(p1b,p2b,s1b,s2b,turnb)
p1w += m*w1
p2w += m*w2
dp[(p1b,p2b,s1b,s2b,turnb)] = (w1,w2)
return (p1w,p2w)

return w

(a,b) = solve(p1s,p2s,0,0,True)
print(max(a,b))
*/

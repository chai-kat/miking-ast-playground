// f expr cont
// cont = f1 expr cont1 ...
// do expr, call cont? 

int main() {
    int x = 2;
    int y = 2;

    //cont0
        if (x == 2) {
            //cont00
                if (x == 3) {
                    return 1;
                } 
            //cont01
                else {
                    return 0;
                }
        }
    //cont1
        else {
            return 0;
        }

        if (y == 2) {
            return 1;
        } else {
            printf("y is not 2\n");
        }
}

// container lambda: 
    //cont0 = if true: cont1 else: cont2
        //cont1 = if true: cont3 else: cont4
        //cont2 = if true: cont5 else: cont6
    // if cont0 returns value: return value, else go to next statement? but is this even CPS 
    // (sort of what we were doing in the earlier examples


int fib(int n) {
    int value = 1;
    while(n != 1) {
        value *= n;
    }
    return value;
}

let fib = lam n.
    let value = 1 in
        let rec loop = lam n.
            if match condition with true then
                value
            else
                value = value * n;
                loop (n - 1)
        in
        loop n
    in
    value


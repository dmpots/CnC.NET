
<fib> :: (fib);
<fibSum> :: (fibSum);

(fib: n) -> <fib: pred(n)>, <fib: ppred(n)>, <fibSum: n>;
[fibs: pred(n)], [fibs: ppred(n)] -> (fibSum: n) -> [fibs: n];

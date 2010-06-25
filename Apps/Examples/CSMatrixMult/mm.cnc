// Declarations
[double A: int row, int col];
[double B: int row, int col];
[double C: int row, int col];
[double scans: int row, int col, int step]; // intermediate results
[int N];

<initTags: int row, int col>;
<computeTags: int row, int col, int step>;

// Step Prescriptions
<initTags> :: (init);
<computeTags> :: (compute);

// Input from the caller
env -> [A: row, col];
env -> [B: row, col];
env -> <initTags: row, col>;
env -> [N];

// Step Executions
(init: i, j) -> <computeTags: i, j, k>, [scans: i, j, k]; // would like to have the constant 0 here
[A: i,k], [B: k,j], [scans: i, j, k], [N] -> (compute: i, j, k) -> <computeTags: i, j, succ(k)>, [scans: i, j, succ(k)], [C: i,j];


// Output to the caller
[C: row, col] -> env;



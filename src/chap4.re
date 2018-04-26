let rec inner_length = (n: int, l: list(int)) =>
  switch l {
  | [] => n
  | [_, ...t] => inner_length(n + 1, t)
  };

let length = inner_length(0);

let rec sum = (l) =>
  switch l {
  | [] => 0
  | [h, ...t] => h + sum(t)
  };

let rec odds = (l) =>
  switch l {
  | [a, _, ...t] => [a, ...odds(t)]
  | _ => l
  };

let rec append = (a, b) =>
  switch a {
  | [] => b
  | [h, ...t] => [h, ...append(t, b)]
  };

let rec rev = (l) =>
  switch l {
  | [] => []
  | [h, ...tail] => rev(tail) @ [h]
  };

let rec take = (n, l) =>
  switch l {
  | [h, ...t] when n > 0 => [h, ...take(n - 1, t)]
  | _ => []
  };

let rec drop = (n, l) =>
  switch l {
  | [_, ...t] when n > 0 => drop(n - 1, t)
  | _ => l
  };

let rec count_true = (l: list(bool)) =>
  switch l {
  | [] => 0
  | [h, ...t] when h => 1 + count_true(t)
  | [_, ...t] => count_true(t)
  };

let rec drop_last = (l) =>
  switch l {
  | [] => []
  | [_] => []
  | [h, ...t] => [h, ...drop_last(t)]
  };

let rec member = (m, l) =>
  switch l {
  | [] => false
  | [h, ..._] when m === h => true
  | [_, ...t] => member(m, t)
  };

let rec makeSet = (l) =>
  switch l {
  | [] => []
  | [h, ...t] when ! member(h, t) => [h, ...makeSet(t)]
  | [_, ...t] => makeSet(t)
  };
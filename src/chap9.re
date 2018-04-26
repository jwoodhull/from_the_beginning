let rec any = (f, l) =>
  switch l {
  | [] => false
  | [h, ..._] when f(h) => true
  | [_, ...t] => any(f, t)
  };

let rec for_all = (f, l) =>
  switch l {
  | [] => true
  | [h, ...t] => f(h) && for_all(f, t)
  };

let member = (m) => any((i) => i === m);

let member_all = (m) => for_all(member(m));

let devideBy = (y, x) => x / y;


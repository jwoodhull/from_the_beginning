let fst = ((x, _)) => x;

let snd = ((_, y)) => y;

let census = [(1, 4), (2, 2), (3, 2), (4, 3), (5, 1), (6, 2)];

let rec lookup = (x, l) =>
  switch l {
  | [] => raise(Not_found)
  | [(k, v), ..._] when k === x => v
  | [h, ...t] => lookup(x, t)
  };

let rec add = (k, v, d) =>
  switch d {
  | [] => [(k, v)]
  | [(k', v'), ...t] =>
    if (k === k') {
      [(k, v), ...t]
    } else {
      [(k', v'), ...add(k, v, t)]
    }
  };

let rec remove = (k, d) =>
  switch d {
  | [] => []
  | [(k', _), ...t] when k === k' => remove(k, t)
  | [h, ...t] => [h, ...remove(k, t)]
  };

let key_exists = (k, d) =>
  try {
    let _ = lookup(k, d);
    true
  } {
  | Not_found => false
  };

let rec uniqueKeys = (d) =>
  switch d {
  | [] => 0
  | [(k, _), ...t] when ! key_exists(k, t) => 1 + uniqueKeys(t)
  | [_, ...t] => uniqueKeys(t)
  };

let rec replace = (k, v, d) =>
  switch d {
  | [] => raise(Not_found)
  | [(k', v'), ...t] =>
    if (k === k') {
      [(k, v), ...t]
    } else {
      [(k', v'), ...replace(k, v, t)]
    }
  };

let rec inverse = (d) =>
  switch d {
  | [] => ([], [])
  | [(k, v), ...t] =>
    let (kl, vl) = inverse(t);
    ([k, ...kl], [v, ...vl])
  };

let rec toDict = (pairs, ~dictionary as d=[], ()) =>
  switch pairs {
  | [] => d
  | [(k, v), ...t] when ! key_exists(k, d) => toDict(t, ~dictionary=add(k, v, d), ())
  | [_, ...t] => toDict(t, ~dictionary=d, ())
  };

let union = (a, b) => {
  let joined = a @ b;
  toDict(joined, ())
};
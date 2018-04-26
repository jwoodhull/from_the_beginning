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

let rec insert = (x, l) =>
  switch l {
  | [] => [x]
  | [h, ...t] =>
    if (x < h) {
      [x, h, ...t]
    } else {
      [h, ...insert(x, t)]
    }
  };

let rec merge = (x, y) =>
  switch (x, y) {
  | ([], l) => l
  | (l, []) => l
  | ([hx, ...tx], [hy, ...ty]) =>
    if (hx < hy) {
      [hx, ...merge(tx, [hy, ...ty])]
    } else {
      [hy, ...merge([hx, ...tx], ty)]
    }
  };

let rec msort = l =>
  switch l {
      | [] => []
      | [x] => [x]
      | _ =>
        let left = take(List.length(l) / 2, l);
        let right = drop(List.length(l) / 2, l);
        merge(msort(left), msort(right));
  };

let rec sort = (l) =>
  switch l {
  | [] => []
  | [x] => [x]
  | [h, ...t] => insert(h, sort(t))
  };
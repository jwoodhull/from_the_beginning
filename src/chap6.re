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

let rec double = (l) =>
  switch l {
  | [] => []
  | [h, ...t] => [h * 2, ...double(t)]
  };

let rec map = (f, l) =>
  switch l {
  | [] => []
  | [h, ...t] => [f(h), ...map(f, t)]
  };

let halve = (v) => v / 2;

let greater = (>=);

let rec merge = (cmp, x, y) =>
  switch (x, y) {
  | ([], l) => l
  | (l, []) => l
  | ([hx, ...tx], [hy, ...ty]) =>
    if (cmp(hx, hy)) {
      [hx, ...merge(cmp, tx, y)]
    } else {
      [hy, ...merge(cmp, x, ty)]
    }
  };

let rec msort = (cmp, l) =>
  switch l {
  | [] => []
  | [x] => [x]
  | _ =>
    let half = List.length(l) / 2;
    let left = take(half, l);
    let right = drop(half, l);
    merge(cmp, msort(cmp, left), msort(cmp, right))
  };

let rec calm = (l) =>
  switch l {
  | [] => []
  | [h, ...t] when h === '!' => ['.', ...calm(t)]
  | [h, ...t] => [h, ...calm(t)]
  };

let calm2 = map((c) => c === '!' ? '.' : c);

let clip = (n) =>
  switch n {
  | n when n <= 1 => 1
  | n when n >= 10 => 10
  | n => n
  };

let clipList = map(clip);

let rec apply = (f, c, s) =>
  switch c {
  | c when c <= 0 => s
  | _ => f(apply(f, c - 1, s))
  };

let rec filter = (f, l) =>
  switch l {
  | [] => []
  | [h, ...t] when f(h) => [h, ...filter(f, t)]
  | [_, ...t] => filter(f, t)
  };

let rec for_all = (f, l) =>
  switch l {
  | [] => true
  | [h, ...t] => f(h) && for_all(f, t)
  };

let rec mapl = (f, l) =>
  switch l {
  | [] => []
  | [h, ...t] => [map(f, h), ...mapl(f, t)]
  };
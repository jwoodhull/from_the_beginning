type colour =
  | Red
  | Blue
  | Green
  | Yellow
  | RGB(int, int, int);

let component = (c) =>
  switch c {
  | Red => (255, 0, 0)
  | Blue => (0, 0, 255)
  | Green => (0, 255, 0)
  | Yellow => (255, 255, 0)
  | RGB(r, g, b) => (r, g, b)
  };

let rec lookup_opt = (x, l) =>
  switch l {
  | [] => None
  | [(k, v), ...t] =>
    if (x == k) {
      Some(v)
    } else {
      lookup_opt(x, t)
    }
  };

let dic = [('a', 'e'), ('b', 'r')];

type sequence('a) =
  | Nil
  | Cons('a, sequence('a));

let rec length = (l) =>
  switch l {
  | Nil => 0
  | Cons(_, t) => 1 + length(t)
  };

let rec append = (a, b) =>
  switch a {
  | Nil => b
  | Cons(h, t) => Cons(h, append(t, b))
  };

let rec take = (l, s) =>
  if (l == 0) {
    Nil
  } else {
    switch s {
    | Nil => s
    | Cons(h, t) => Cons(h, take(l - 1, t))
    }
  };

let rec map = (f, s) =>
  switch s {
  | Nil => s
  | Cons(h, t) => Cons(f(h), map(f, t))
  };

let list = Cons(1, Cons(2, Cons(3, Nil)));

type expr =
  | Num(int)
  | Add(expr, expr)
  | Subtract(expr, expr)
  | Multiply(expr, expr)
  | Divide(expr, expr);

let rec evaluate = (e) =>
  switch e {
  | Num(x) => x
  | Add(e, e') => evaluate(e) + evaluate(e')
  | Subtract(e, e') => evaluate(e) - evaluate(e')
  | Multiply(e, e') => evaluate(e) * evaluate(e')
  | Divide(e, e') => evaluate(e) / evaluate(e')
  };

let exp = Add(Num(1), Multiply(Num(2), Num(3)));

type rect =
  | Rect(int, int)
  | Square(int);

let area = (r) =>
  switch r {
  | Square(x) => x * x
  | Rect(x, y) => x * y
  };

let rotate = (r) =>
  switch r {
  | Square(_) => r
  | Rect(x, y) =>
    if (x > y) {
      Rect(y, x)
    } else {
      r
    }
  };

let getX = (r) =>
  switch r {
  | Square(x) => x
  | Rect(x, _) => x
  };

let sortTuple = (r, r') => (getX(r), getX(r'));

let compareList = (r, r') => {
  let t = sortTuple(r, r');
  switch t {
  | (x, x') when x == x' => 0
  | (x, x') when x > x' => 1
  | (_, _) => (-1)
  }
};

let totalWidth = (l) => List.map(rotate, l) |> List.sort(compareList);

let rects = [Square(3), Rect(4, 2), Rect(1, 2)];
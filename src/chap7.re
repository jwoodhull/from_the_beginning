let rec take = (n, l) =>
  switch l {
  | [] =>
    if (n === 0) {
      []
    } else {
      raise(Invalid_argument("take"))
    }
  | [h, ...t] =>
    if (n < 0) {
      raise(Invalid_argument("take"))
    } else if (n === 0) {
      []
    } else {
      [h, ...take(n - 1, t)]
    }
  };

let safe_devide = (x, y) =>
  try (x / y) {
  | Division_by_zero => 0
  };
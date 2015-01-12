function add(n, total) {
  return n + total;
}

function fold(f, end, list) {
  if(list.length == 0) {
    return end;                                        // <1>
  }

  return f(list[0], fold(f, end, list.slice(1)));      // <2>
}

console.log(fold(add, 0, [1, 2, 3, 4, 5])) // prints 15

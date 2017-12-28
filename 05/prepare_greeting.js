function prepare_greeting_1(name) {
  if (true) {
    greeting = "Hello, "
  }  else {
    greeting = 5
  }
  return greeting.concat(name, "!")
}

console.log(prepare_greeting_1("Chris"))
function prepare_greeting_2(name : string) : string {
  let greeting
  if (true) {
    greeting = "Hello, "
  }  else {
    greeting = 5
  }
  return greeting.concat(name, "!")
}

console.log(prepare_greeting_2("Chris"))
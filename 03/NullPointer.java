public class NullPointer {

  static class Greeter {
    String greet(int n) {                   // <1>
      if (n > 0) {
        return "Hello";                     // <2>
      } else {
        return null;                        // <3>
      }
    }
  }

  static class Responder {
    String answerGreeting(Greeter klass) {
      String greeting = klass.greet(0);     // <4>
      if (greeting.equals("Hello")) {
        return "Nice to meet you!";
      } else {
        return "Um, Hello?";
      }
    }
  }

  public static void main(String args[]) {
    Greeter greeter = new Greeter();
    Responder other = new Responder();

    String s = other.answerGreeting(greeter);  // <5>

    //int x = null;  // not okay, this is a compile error
    System.out.println(s);
  }
}

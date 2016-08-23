class Greeter
  def greet(n)
    return 'Hello' if n > 0       # <1>
  end
end

class Responder
  def answer_greeting(klass)
    greeting = klass.greet(0)
    if greeting.match(/[Hh]ello/) # <2>
      'Nice to meet you!'
    else
      'Um, Hello?'
    end
  end
end

class NullPointer
  def self.run
    greeter = Greeter.new
    s = Responder.new.answer_greeting(greeter)
    puts s
  end
end

NullPointer.run

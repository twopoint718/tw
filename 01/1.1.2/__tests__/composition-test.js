jest.dontMock('../composition');

var m = require('../composition');

describe('foo1 chains bar, baz, and quux', function() {
  it('should equal 7', function() {
    expect(m.foo1(1)).toBe(7);
  });
});

describe('foo2 explicitly chains bar, baz, and quux via parameters', function() {
  it('should equal 7', function() {
    expect(m.foo2(1, m.bar, m.baz, m.quux)).toBe(7);
  });
});

describe('foo3 chains bar, baz, and quux & omits intermediate variables', function() {
  it('should equal 7', function() {
    expect(m.foo3(1, m.bar, m.baz, m.quux)).toBe(7);
  });
});

describe('foo4 composes bar, baz, and quux', function() {
  it('should equal 7', function() {
    expect(m.foo4(1)).toBe(7);
  });
});

var assert = require('assert');
var w = require('./5.2/what_vs_how.js');
var z = require('./5.5/zip.js');
var f = require('./5.6/fold.js');


function checkInactive(users) {
  for (var i = 0; i < users.length; i++) {
    if (users[i].active) {
      return false;
    }
  }
  return true;
}

var active = function(usr) { return usr.active; }

describe('Chapter 5', function() {

  describe('What vs. how', function() {
    var expected = [false, false, false];
    var active = function(usr) { return usr.active; }

    it('a loop updates in-place', function () {
      w.deactivateUsersLoop(w.users);
      assert.deepEqual(expected, w.users.map(active));
    });

    it('map retuns a new collection', function() {
      assert.deepEqual(expected, w.deactivateUsersMap(w.users).map(active));
    });
  });

  describe('Zip', function() {
    var
      l1 = [1, 2, 3],
      l2 = ['a', 'b', 'c'];
      expected = [[1, 'a'], [2, 'b'], [3, 'c']];

    it('zip pairs two lists', function () {
      assert.deepEqual(expected, z.zip(l1, l2));
    });

    it('curriedZip behaves the same as zip', function () {
      assert.deepEqual(expected, z.curriedZip(l1, l2));
    });

    it('curry with map "lifts" functions to lists', function () {
      var inc = function (x) { return x + 1 };
      var map = function (f, xs) { return xs.map(f) };

      assert.deepEqual([2, 3, 4], z.curry(map)(inc)([1, 2, 3]));
    });
  });

  describe('Folds', function() {
    it('should behave the same as built-in map', function () {
      var inc = function (x) { return x + 1 };
      var input = [1, 2, 3];

      assert.deepEqual(input.map(inc), f.myMap(inc, input));
    });

    it('should concatenate a list of strings', function () {
      var expected = 'this is a string';
      assert.deepEqual(expected, f.fold(f.strConcat, '', ['this ', 'is ', 'a ', 'string']));
    });
  });

});

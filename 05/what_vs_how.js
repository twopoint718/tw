// tag::deactivateLoop[]
function deactivateUsersLoop(users) {
  for (var i = 0; i < users.length; i++) {
    var user = users[i];                      // <1>
    user.active = false;                      // <2>
  }
}
// end::deactivateLoop[]

// tag::deactivateMap[]
function deactivateUsersMap(users) {
  var deactivate = function(user) {           // <1>
    return { uid: user.uid, active: false };  // <2>
  }
  return users.map(deactivate);               // <3>
}
// end::deactivateMap[]

function checkInactive(users) {
  for (var i = 0; i < users.length; i++) {
    if (users[i].active) {
      return false;
    }
  }
  return true;
}

var users = [
  {uid: 1, active: true},
  {uid: 2, active: false},
  {uid: 3, active: true}
];

// Tests
function assert(condition, message) { return condition || console.log("Error: " + message); }

assert(!checkInactive(users), "Precondition failed");

// tag::deactivatingUsers[]
var inactiveUsers = deactivateUsersMap(users); // <1>
deactivateUsersLoop(users);                    // <2>
// end::deactivatingUsers[]

assert(checkInactive(inactiveUsers), "'deactivateUsersMap' failed to deactivate");
assert(users !== inactiveUsers, "Did not create new object");
assert(!checkInactive(users), "Users should remain unchanged");

assert(checkInactive(users), "'deactivateUsersLoop' should have modified 'users'");

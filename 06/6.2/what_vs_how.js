var users = [
  {uid: 1, active: true},
  {uid: 2, active: false},
  {uid: 3, active: true}
];

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

// tag::deactivatingUsers[]
var inactiveUsers = deactivateUsersMap(users); // <1>
deactivateUsersLoop(users);                    // <2>
// end::deactivatingUsers[]

module.exports = {
  users: users,
  deactivateUsersLoop: deactivateUsersLoop,
  deactivateUsersMap: deactivateUsersMap
}

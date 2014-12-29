if (Meteor.isClient) {
  myRide = 5;                               // <1>
  myRideDependency = new Deps.Dependency;   // <2>

  setMyRide = function(dist) {
    myRide = dist;
    myRideDependency.changed();             // <3>
  }

  getMyRide = function() {
    myRideDependency.depend();              // <4>
    return myRide;
  }

  Deps.autorun(function () {                // <5>
    console.log("Well, I also rode " + getMyRide() + " miles.");
  });
}

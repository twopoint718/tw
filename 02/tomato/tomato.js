// tag::commonCode[]
Rides = new Meteor.Collection("rides");
// end::commonCode[]

// tag::clientCode[]
if (Meteor.isClient) {

  Meteor.subscribe("rides");
// end::clientCode[]

  // tag::helperFunctions[]
  var currentEmail = function() {
    return Meteor.user().emails[0].address;
  }

  var submitDistance = function(evt, tmpl) {
    distance = parseFloat(tmpl.find('#distance').value) || 0.0; // <1>
    user_email = currentEmail();
    Rides.insert({rider: user_email, distance: distance});
  }
  // end::helperFunctions[]

  // tag::templateHandlerMainRides[]
  Template.main.rides = function() {
    return Rides.find();                                        // <1>
  };
  // end::templateHandlerMainRides[]

  // tag::templateHandlerMainEvents[]
  Template.main.events({
    'click .delete': function(evt, tmpl) {                      // <1>
      if (Rides.findOne(this._id).rider == currentEmail()) {    // <2>
        Rides.remove(this._id);                                 // <3>
      } else {
        console.log('can only delete your records!');           // <4>
      }
    }
  });
  // end::templateHandlerMainEvents[]

  // tag::templateHandlerInputFormEvents[]
  Template.inputform.events({
    'click button[name="submit"]': submitDistance,             // <1>
    'keydown input#distance': function(evt, tmpl) {            // <2>
      if (evt.keyCode == 13) {                                 // <3>
        submitDistance(evt, tmpl);                             // <4>
      }
    }
  });
  // end::templateHandlerInputFormEvents[]
}

// tag::serverSide[]
if (Meteor.isServer) {
  Meteor.publish("rides", function () {
    return Rides.find();
  });

  var verifyUser = function (uid, ride) {                  // <1>
    var user = Meteor.users.findOne({_id: uid});
    return user && user.emails[0].address == ride.rider
  }

  Rides.allow({
    insert: verifyUser,
    remove: verifyUser
  });
}
// end::serverSide[]

/*
  formSubmit.js
  Used to allow a user to press ENTER on their keyboard in order to submit forms (ie login form).
  Input ids identified here are specfic to the cafe-predicting dashboard's login and sign up forms.
*/ 

$(document).keyup(function(event) {
  // Login form
  if (($("#loginUsername").is(":focus") || $("#loginPassword").is(":focus")) && (event.keyCode == 13)) {
    $("#loginButton").click();
  }
  // Sign up form
  else if (($("#signupUsername").is(":focus") || $("#signupPassword1").is(":focus") || $("#signupPassword2").is(":focus"))
      && (event.keyCode == 13)) {
    $("#signupButton").click();
  }
});
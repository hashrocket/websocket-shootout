// Place all the behaviors and hooks related to the matching controller here.
// All this logic will automatically be available in application.js.

//= require cable
//= require_self
//= require_tree .

this.App = {};

App.cable = ActionCable.createConsumer();

Feature: I can create a plot point
  As a Game Master
  I want to create a plot point
  So that I can easily create a game for people to play

  Background:
    Given a user registered with username = "chester@tester.com", password = "thisisthepassword"
    And I have logged in

  Scenario: I can create a basic plot
    Given I am ready to create a plot point
    And I have provided a plot point name of "The Plot Point"
    And I have provided a plot point description of "This is the plot point description"
    And I have provided a plot point brief description of "This is the plot point brief description"
    When I save the plot point
    Then the save was successful
    And the plot point is in the database
    And the plot point name  is "The Plot Point"
    And the plot point description is "This is the plot point description"
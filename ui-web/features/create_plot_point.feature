# Created by jim at 3/26/19
Feature: As a plot point creator
  I want to create a plot point
  So that I can use it to run a game

  Background:
    Given I have started the application
    And I'm presented with an authentication challenge
    And I provide a username of "ChesterTester"
    And I provide a password of "ChesterTester1!"
    When I authenticate
    Then I am directed to the list of plot points

  Scenario: I can create a minimal plot point
    Given I want to add a plot point
    And a plot point name of "This is a plot point"
    And a plot point description of "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
    When I save the plot point
    Then the operation is successful
    And the plot point is in the data store

  Scenario: I can create a plot point with basic rules
    Given I want to add a plot point
    And a plot point name of "This is a plot point"
    And a plot point description of "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
    And I want the maximum attribute points to be 100
    And I want the maximum number of major hindrances to be 200
    And I want the maximum number of minor hindrances to be 300
    And I want the maximum skill points to be 400
    When I save the plot point
    Then the operation is successful
    And the plot point is in the data store
    And the maximum attribute points are 100
    And the maximum number of major hindrances is 200
    And the maximum number of minor hindrances is 300
    And the maximum skill points is 400

  Scenario: I can create a plot point with setting rules
    Given I want to add a plot point
    And a plot point name of "This is a plot point"
    And a plot point description of "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
    And I want to add these setting rules:
      | Setting rule 1 | Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus sapien leo, |
    When I save the plot point
    Then the operation is successful
    And the plot point is in the data store
    And the setting rules are in the datastore

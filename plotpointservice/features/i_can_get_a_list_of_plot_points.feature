Feature: As a user
  I can get a list of plot plot_points
  So that i can choose one to use, or edit or otherwise user

  Background:
    Given a user registered with username = "chester@tester.com", password = "thisisthepassword"
    And I have logged in

  Scenario: I can get a list of plot points and all data
    Given a plot point exists in the database
    When I query for all plot points
    Then I get a list of all plot points in the database

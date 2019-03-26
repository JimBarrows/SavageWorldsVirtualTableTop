# Created by jim at 3/26/19
Feature: As a plot point creator
  I want to create a plot point
  So that I can use it to run a game

  Scenario: I can create a minimal plot point
    Given I want to add a plot point
    And a plot point name of "This is a plot point"
    And a plot point description of "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum id bibendum est, id suscipit arcu. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Integer sit amet eros eu lectus volutpat commodo sed fermentum urna. Sed accumsan ut felis pretium vestibulum. Vivamus dignissim, tortor id malesuada aliquam, odio nisi laoreet orci, lacinia euismod est lorem vel felis. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam vel arcu quam. Ut hendrerit aliquam ligula, eu tincidunt lacus elementum a. Pellentesque dictum dui elementum lacinia dictum. Ut mattis blandit orci non blandit. Sed non eros quis est auctor ornare. Curabitur tincidunt risus ut egestas aliquam."
    When I save the plot point
    Then the operation is successful
    And the plot point is in the data store
    And I can continue to edit the plot point


Feature: As a plot point creator
  I want to add powers to my plot point
  So that I can define the magical abilities available in my game

  Background:
    Given I have started the application
    And I'm presented with an authentication challenge
    And I provide a username of "ChesterTester"
    And I provide a password of "ChesterTester1!"
    When I authenticate
    Then I am directed to the list of plot points

  Scenario: Create a plot point with default powers
    Given I want to add a plot point
    And a plot point name of "Magical World"
    And a plot point description of "A world full of magic and powers"
    When I save the plot point
    Then the operation is successful
    And the plot point is in the data store
    And the plot point has default powers

  Scenario: Add a basic power to a plot point
    Given I have a plot point named "Magical World"
    And I want to add a power to the plot point
    And the power has a name of "Bolt"
    And the power has a description of "Hurls a bolt of energy at target"
    And the power has power points of 1
    And the power has a range of "Smarts x2"
    And the power has a duration of "Instant"
    And the power has trappings of "Fire, Lightning, Ice"
    When I save the power
    Then the operation is successful
    And the power is saved to the plot point

  Scenario: Add a power with modifiers
    Given I have a plot point named "Magical World"
    And I want to add a power to the plot point
    And the power has a name of "Armor"
    And the power has a description of "Grants magical protection"
    And the power has power points of 2
    And the power has a range of "Touch"
    And the power has a duration of "5 rounds"
    And the power has trappings of "Shimmering field, Stone skin"
    And I add a modifier with name "Additional Recipient" and description "Affects one additional target" and power point modifier of 1
    And I add a modifier with name "Heavy Armor" and description "Increases armor bonus" and power point modifier of 2
    When I save the power
    Then the operation is successful
    And the power is saved to the plot point
    And the power has 2 modifiers

  Scenario: Power validation - power points must be positive
    Given I have a plot point named "Magical World"
    And I want to add a power to the plot point
    And the power has a name of "Invalid Power"
    And the power has a description of "This should fail"
    And the power has power points of 0
    When I try to save the power
    Then the operation fails
    And I see an error message "Power points must be greater than 0"

  Scenario: Default powers are included in new plot points
    Given I create a new plot point named "Adventure World"
    Then the plot point should have the following default powers:
      | Name       | Power Points | Range        | Duration    |
      | Arcane Protection | 1    | Touch        | 5 rounds    |
      | Bolt       | 1           | Smarts x2    | Instant     |
      | Boost/Lower Trait | 2    | Smarts       | 5 rounds    |
      | Detect/Conceal Arcana | 2 | Sight      | 5 rounds    |
      | Healing    | 3           | Touch        | Instant     |
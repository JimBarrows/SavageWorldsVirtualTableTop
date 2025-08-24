Feature: Character Leveling System
  As a Game Master or Player
  I want to level up my character  
  So that they can advance and become more powerful

  Background:
    Given I am logged in as a Game Master
    And I have created a plot point named "Test Campaign"
    And I have created a character named "Test Hero" with starting attributes

  Scenario: View current character experience and rank
    Given my character "Test Hero" has 0 experience points
    When I view the character sheet for "Test Hero"
    Then I should see the character's current rank as "Novice"
    And I should see 0 experience points displayed
    And I should see advances available as 0

  Scenario: Add experience points to a character
    Given my character "Test Hero" has 0 experience points
    When I edit the character "Test Hero"
    And I add 5 experience points
    And I save the character
    Then the character should have 5 experience points
    And the character should have 1 advance available

  Scenario: Character advances from Novice to Seasoned
    Given my character "Test Hero" has 15 experience points
    When I edit the character "Test Hero"
    And I add 5 experience points
    And I save the character
    Then the character should have 20 experience points
    And the character's rank should be "Seasoned"
    And the character should have 4 advances available

  Scenario: Apply an advance to increase an attribute
    Given my character "Test Hero" has 5 experience points
    And my character has 1 advance available
    When I edit the character "Test Hero"
    And I choose to spend an advance
    And I select to increase Agility from d4 to d6
    And I save the character
    Then the character's Agility should be d6
    And the character should have 0 advances available
    And the advance history should show "Increased Agility to d6"

  Scenario: Apply an advance to add a new Edge
    Given my character "Test Hero" has 10 experience points
    And my character has 2 advances available
    And my character meets the requirements for "Quick" edge
    When I edit the character "Test Hero"
    And I choose to spend an advance
    And I select to add the "Quick" edge
    And I save the character
    Then the character should have the "Quick" edge
    And the character should have 1 advance available
    And the advance history should show "Added Edge: Quick"

  Scenario: Apply an advance to increase skills
    Given my character "Test Hero" has 5 experience points
    And my character has 1 advance available
    When I edit the character "Test Hero"
    And I choose to spend an advance
    And I select to increase two skills by one die type each
    And I increase Fighting from d4 to d6
    And I increase Notice from d4 to d6
    And I save the character
    Then the character's Fighting skill should be d6
    And the character's Notice skill should be d6
    And the character should have 0 advances available
    And the advance history should show "Increased Fighting to d6, Notice to d6"

  Scenario: View rank progression requirements
    When I view the character advancement rules
    Then I should see the following rank requirements:
      | Rank       | Experience Points | Advances |
      | Novice     | 0-19             | 0-3      |
      | Seasoned   | 20-39            | 4-7      |
      | Veteran    | 40-59            | 8-11     |
      | Heroic     | 60-79            | 12-15    |
      | Legendary  | 80+              | 16+      |

  Scenario: Track advance history for a character
    Given my character "Test Hero" has the following advance history:
      | Advance # | Type      | Details                    |
      | 1         | Attribute | Increased Vigor to d6      |
      | 2         | Skills    | Fighting d6, Shooting d6   |
      | 3         | Edge      | Added Brawny              |
    When I view the character sheet for "Test Hero"
    Then I should see the complete advance history
    And each advance should show when it was taken

  Scenario: Validate advance restrictions by rank
    Given my character "Test Hero" is rank "Novice"
    And my character has 1 advance available
    When I edit the character "Test Hero"
    And I choose to spend an advance
    Then I should not be able to select Veteran edges
    And I should not be able to increase attributes beyond d12
    And I should see only valid advancement options for Novice rank

  Scenario: Remove an advance from character history
    Given my character "Test Hero" has applied advances
    And I am viewing the character's advance history
    When I select to undo the last advance
    And I confirm the action
    Then the advance should be removed from history
    And the character's stats should revert
    And the advance should be available again
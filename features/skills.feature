Feature: Default Skills
  As a Game Master
  I want plot points to be created with default skills
  So that I don't have to manually add common skills each time

  Background:
    Given I am logged in as "test_user@example.com" with password "TestPassword123!"
    And I navigate to the plot point creation page

  Scenario: New plot point includes default skills
    When I enter plot point name "Test Campaign"
    Then the plot point should have the following default skills:
      | name         | attribute | description                                           |
      | Athletics    | Agility   | Covers running, jumping, swimming, throwing, and catching |
      | Common Knowledge | Smarts    | General knowledge of the world                           |
      | Notice       | Smarts    | General awareness and perception                         |
      | Persuasion   | Spirit    | Ability to convince others                              |
      | Stealth      | Agility   | The ability to hide and move quietly                    |

  Scenario: Default skills are editable
    When I enter plot point name "Editable Skills Test"
    And I edit the "Athletics" skill
    And I change its attribute to "Strength"
    And I save the changes
    Then the "Athletics" skill should have attribute "Strength"

  Scenario: Default skills can be deleted
    When I enter plot point name "Deletable Skills Test"
    And I delete the "Stealth" skill
    And I save the changes
    Then the plot point should not have a "Stealth" skill

  Scenario: Additional skills can be added to default skills
    When I enter plot point name "Additional Skills Test"
    And I add a new skill named "Fighting" with attribute "Agility"
    And I save the changes
    Then the plot point should have 6 skills
    And the plot point should have a "Fighting" skill with attribute "Agility"
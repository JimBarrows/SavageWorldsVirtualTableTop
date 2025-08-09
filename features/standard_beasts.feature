# Feature: Standard Beasts in Plot Points
Feature: As a game master
  I want plot points to be created with standard beasts
  So that I have immediate access to common creatures from the Savage Worlds bestiary

  Background:
    Given I have started the application
    And I'm presented with an authentication challenge
    And I provide a username of "ChesterTester"
    And I provide a password of "ChesterTester1!"
    When I authenticate
    Then I am directed to the list of plot points

  Scenario: Creating a new plot point includes standard beasts
    Given I want to add a plot point
    And a plot point name of "Adventure with Standard Beasts"
    And a plot point description of "A plot point that should have standard beasts"
    When I save the plot point
    Then the operation is successful
    And the plot point is in the data store
    And the plot point has standard beasts included
    And the plot point contains these standard beasts:
      | name           | description                                           |
      | Alligator      | A large reptilian predator                          |
      | Bear           | A powerful woodland predator                        |
      | Dog/Wolf       | Canine hunters that often travel in packs          |
      | Ghost          | Spectral undead with supernatural abilities        |
      | Giant Spider   | Oversized arachnid with venomous bite             |
      | Goblin         | Small, cunning humanoids                           |
      | Horse          | Common riding and draft animal                     |
      | Lion           | King of beasts, powerful feline predator          |
      | Orc            | Brutish humanoid warriors                         |
      | Skeleton       | Animated bones of the dead                        |
      | Swarm          | Collection of small creatures acting as one       |
      | Zombie         | Walking dead, slow but relentless                 |

  Scenario: Existing plot points maintain their beast configuration
    Given I have an existing plot point with custom beasts
    When I view the plot point
    Then the custom beasts are preserved
    And standard beasts are not retroactively added

  Scenario: Standard beasts can be modified after creation
    Given I have a plot point with standard beasts
    When I edit a standard beast's attributes
    And I save the beast changes
    Then the operation is successful
    And the modified beast attributes are preserved

  Scenario: Standard beasts can be removed if not needed
    Given I have a plot point with standard beasts
    When I remove a standard beast from the plot point
    And I save the plot point
    Then the operation is successful
    And the removed beast is no longer in the plot point
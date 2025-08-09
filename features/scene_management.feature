# Feature: Scene Management
Feature: As a game master
  I want to manage scenes and their dramatis personae
  So that I can organize characters for different scenes in my game

  Background:
    Given I have started the application
    And I'm presented with an authentication challenge
    And I provide a username of "ChesterTester"
    And I provide a password of "ChesterTester1!"
    When I authenticate
    Then I am directed to the list of plot points

  Scenario: I can create a new scene
    Given I want to add a scene
    And a scene name of "Opening Tavern Scene"
    And a scene description of "The heroes gather at the Red Dragon Inn to begin their adventure"
    When I save the scene
    Then the operation is successful
    And the scene is in the data store

  Scenario: I can add characters to a scene's dramatis personae
    Given I have a scene named "Opening Tavern Scene"
    And I have characters available:
      | name      | description           |
      | Sir Gareth| A noble knight        |
      | Mara      | A cunning thief       |
      | Grimjaw   | An orc bartender      |
    When I add "Sir Gareth" to the scene's dramatis personae
    And I add "Mara" to the scene's dramatis personae  
    And I add "Grimjaw" to the scene's dramatis personae
    And I save the scene
    Then the operation is successful
    And the scene has 3 characters in its dramatis personae
    And "Sir Gareth" is in the scene's dramatis personae
    And "Mara" is in the scene's dramatis personae
    And "Grimjaw" is in the scene's dramatis personae

  Scenario: I can update a character's role in a scene
    Given I have a scene named "Opening Tavern Scene"
    And "Sir Gareth" is already in the scene's dramatis personae
    When I update "Sir Gareth" in the dramatis personae with description "The party leader and main protagonist"
    And I save the scene
    Then the operation is successful
    And "Sir Gareth" has description "The party leader and main protagonist" in the dramatis personae

  Scenario: I can remove a character from a scene's dramatis personae
    Given I have a scene named "Opening Tavern Scene"
    And the scene has characters in its dramatis personae:
      | name      | description           |
      | Sir Gareth| A noble knight        |
      | Mara      | A cunning thief       |
      | Grimjaw   | An orc bartender      |
    When I remove "Grimjaw" from the scene's dramatis personae
    And I save the scene
    Then the operation is successful
    And the scene has 2 characters in its dramatis personae
    And "Sir Gareth" is in the scene's dramatis personae
    And "Mara" is in the scene's dramatis personae
    And "Grimjaw" is not in the scene's dramatis personae

  Scenario: I can delete a scene entirely
    Given I have a scene named "Opening Tavern Scene"
    When I delete the scene "Opening Tavern Scene"
    Then the operation is successful
    And the scene "Opening Tavern Scene" is not in the data store
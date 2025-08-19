Feature: Dramatic Personae Appendix for Plot Points
  As a Game Master
  I want to see an appendix with all dramatic personae in my plot point
  So that I can quickly reference characters during gameplay

  Background:
    Given I am a Game Master managing a plot point
    And my plot point contains multiple characters

  Scenario: Game Master views dramatic personae appendix
    Given I have a plot point with name "The Haunted Mansion" 
    And the plot point contains the following characters:
      | Name          | Description                | Role        |
      | Lord Ashworth | The mansion's former owner | Antagonist  |
      | Sarah Mills   | A local historian          | Ally        |
      | Ghost Butler  | Undead servant             | Minor NPC   |
    When I navigate to the "Dramatic Personae" section
    Then I should see an appendix titled "Dramatic Personae"
    And I should see all 3 characters listed with their key details
    And I should see "Lord Ashworth" listed as "The mansion's former owner"
    And I should see "Sarah Mills" listed as "A local historian"
    And I should see "Ghost Butler" listed as "Undead servant"

  Scenario: Empty dramatic personae appendix
    Given I have a plot point with name "Empty Adventure"
    And the plot point contains no characters
    When I navigate to the "Dramatic Personae" section
    Then I should see an appendix titled "Dramatic Personae"
    And I should see a message "No characters added to this plot point"

  Scenario: Navigation to dramatic personae appendix
    Given I am editing any plot point
    When I look at the navigation menu
    Then I should see a "Dramatic Personae" navigation option
    When I click the "Dramatic Personae" navigation option
    Then I should be taken to the dramatic personae appendix view

  Scenario: Character details in appendix are read-only
    Given I have a plot point with characters
    And I am viewing the "Dramatic Personae" appendix
    When I look at the character details
    Then the character information should be displayed for reference only
    And I should not be able to edit characters directly from the appendix
    And I should see a link or button to edit characters in the Characters section
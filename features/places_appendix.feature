Feature: Story Places Appendix
  As a Game Master
  I want to see an appendix of all places mentioned in my story
  So that I can have a comprehensive view of all locations involved in my Plot Point

  Background:
    Given I am logged in as a Game Master
    And I have access to the Plot Point management system

  Scenario: View places appendix for story with multiple locations
    Given I have a Plot Point with the story text:
      """
      The heroes start their journey in the bustling city of Waterdeep. 
      They must travel through the dangerous Sword Coast to reach the ancient ruins of Undermountain.
      Along the way, they will stop at the peaceful village of Red Larch before facing the perils of the Underdark.
      """
    When I view the Plot Point details
    Then I should see a "Places Appendix" section
    And the appendix should contain the following places:
      | Place Name    |
      | Waterdeep     |
      | Sword Coast   |
      | Undermountain |
      | Red Larch     |
      | Underdark     |
    And each place should appear only once in the appendix

  Scenario: View places appendix for story with no location references
    Given I have a Plot Point with the story text:
      """
      The heroes must solve a complex riddle that tests their wisdom and intelligence.
      They need to think carefully about the clues provided.
      """
    When I view the Plot Point details
    Then I should see a "Places Appendix" section
    And the appendix should display "No places mentioned in this story"

  Scenario: View places appendix with duplicate location references
    Given I have a Plot Point with the story text:
      """
      The adventure begins in Neverwinter. The heroes explore Neverwinter's districts.
      After leaving Neverwinter, they return to Neverwinter for supplies.
      """
    When I view the Plot Point details
    Then I should see a "Places Appendix" section
    And the appendix should contain:
      | Place Name  |
      | Neverwinter |
    And "Neverwinter" should appear only once in the appendix

  Scenario: Places appendix updates when story content changes
    Given I have a Plot Point with story text mentioning "Baldur's Gate"
    And I am viewing the Plot Point details
    When I edit the story to remove "Baldur's Gate" and add "Candlekeep"
    And I save the plot point changes
    Then the places appendix should no longer contain "Baldur's Gate"
    And the places appendix should contain "Candlekeep"

  Scenario: Places appendix is case-insensitive for duplicate detection
    Given I have a Plot Point with the story text:
      """
      The heroes visit Waterdeep in the morning. Later, they explore WATERDEEP's markets.
      In the evening, they leave waterdeep for their next destination.
      """
    When I view the Plot Point details
    Then the places appendix should contain:
      | Place Name |
      | Waterdeep  |
    And "Waterdeep" should appear only once in the appendix
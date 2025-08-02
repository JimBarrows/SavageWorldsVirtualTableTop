Feature: User Logout
  As a logged-in user
  I want to log out of my account
  So that I can secure my session and allow others to use the application

  Background:
    Given I have started the application
    And I am logged in as "ChesterTester"
    And I am on the plot points list page

  Scenario: Successful logout from navigation menu
    Given I can see the logout option in the navigation menu
    When I click on the logout button
    Then I am logged out successfully
    And I am redirected to the login page
    And I see a message "You have been logged out successfully"
    And I no longer see the authenticated navigation options

  Scenario: Logout clears session data
    Given I have plot points loaded in my session
    When I click on the logout button
    Then I am logged out successfully
    And all session data is cleared
    When I navigate back to the plot points page
    Then I am redirected to the login page
    And I cannot access protected resources

  Scenario: Logout with unsaved changes warning
    Given I am editing a plot point
    And I have made unsaved changes
    When I click on the logout button
    Then I see a warning "You have unsaved changes. Are you sure you want to logout?"
    When I confirm the logout
    Then I am logged out successfully
    And unsaved changes are discarded

  Scenario: Cancel logout with unsaved changes
    Given I am editing a plot point
    And I have made unsaved changes
    When I click on the logout button
    Then I see a warning "You have unsaved changes. Are you sure you want to logout?"
    When I cancel the logout
    Then I remain logged in
    And I stay on the current page
    And my unsaved changes are preserved

  Scenario: Automatic logout after session timeout
    Given my session timeout is set to 30 minutes
    And I have been inactive for 30 minutes
    When I try to perform any action
    Then I see a message "Your session has expired"
    And I am automatically logged out
    And I am redirected to the login page

  Scenario: Logout from multiple browser tabs
    Given I am logged in on multiple browser tabs
    When I click logout in one tab
    Then I am logged out in all tabs
    And all tabs redirect to the login page

  Scenario: Remember me disabled after logout
    Given I previously logged in with "Remember me" checked
    When I click on the logout button
    Then I am logged out successfully
    And the "Remember me" preference is cleared
    When I return to the login page
    Then my credentials are not pre-filled
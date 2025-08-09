Feature: Branding Banners on Auth Pages
  As a user visiting authentication pages
  I want to see clear branding banners
  So that I know what application I'm logging into

  Scenario: Login page displays branding banner
    Given I navigate to the login page
    Then I see a branding banner at the top
    And the banner contains "Savage Worlds Virtual Table Top"
    And the banner has consistent styling with the marketing page

  Scenario: Signup page displays branding banner
    Given I navigate to the signup page  
    Then I see a branding banner at the top
    And the banner contains "Savage Worlds Virtual Table Top"
    And the banner has consistent styling with the marketing page

  Scenario: Password reset page displays branding banner
    Given I navigate to the password reset page
    Then I see a branding banner at the top
    And the banner contains "Savage Worlds Virtual Table Top"
    And the banner has consistent styling with the marketing page

  Scenario: Banner navigation from login page
    Given I am on the login page
    When I click on the branding banner
    Then I am redirected to the marketing landing page

  Scenario: Banner navigation from signup page
    Given I am on the signup page
    When I click on the branding banner
    Then I am redirected to the marketing landing page
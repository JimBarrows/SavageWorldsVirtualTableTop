import { Given, When, Then } from 'cucumber';
import { By } from 'selenium-webdriver';
import { expect } from 'chai';

// Background steps for plot point setup
Given('I am a Game Master managing a plot point', async function () {
  // Navigate to plot point creation/editing page
  const browser = this.browser;
  await browser.get('http://localhost:3000/plot_point/test-plot-point/edit');
  await browser.sleep(1000);
});

Given('my plot point contains multiple characters', async function () {
  // This is handled by individual scenario character setup
  this.plotPointHasCharacters = true;
});

// Character setup steps
Given('I have a plot point with name {string}', async function (plotPointName) {
  this.plotPointName = plotPointName;
  // Set the plot point name in the form
  const nameField = await this.browser.findElement(By.css('input[placeholder="Name"]'));
  await nameField.clear();
  await nameField.sendKeys(plotPointName);
});

Given('the plot point contains the following characters:', async function (dataTable) {
  this.expectedCharacters = dataTable.hashes();
  
  // Navigate to Characters section to add characters
  const browser = this.browser;
  const charactersNavItem = await browser.findElement(By.css('a[href*="Characters"], button:contains("Characters")'));
  await charactersNavItem.click();
  await browser.sleep(500);
  
  // Add each character from the data table
  for (const character of this.expectedCharacters) {
    // Click add character button
    const addButton = await this.browser.findElement(By.css('button:contains("Add"), button[title*="Add"]'));
    await addButton.click();
    await this.browser.sleep(500);
    
    // Fill character details
    const nameField = await this.browser.findElement(By.css('input[placeholder*="Name"], input[name*="name"]'));
    await nameField.sendKeys(character.Name);
    
    if (character.Description) {
      const descField = await this.browser.findElement(By.css('textarea[placeholder*="Description"], textarea[name*="description"]'));
      await descField.sendKeys(character.Description);
    }
    
    if (character.Role) {
      const roleField = await this.browser.findElement(By.css('input[placeholder*="Role"], input[name*="background"]'));
      await roleField.sendKeys(character.Role);
    }
  }
});

Given('the plot point contains no characters', async function () {
  this.expectedCharacters = [];
  // Ensure we're starting with an empty characters list
  const charactersNavItem = await this.browser.findElement(By.css('a[href*="Characters"], button:contains("Characters")'));
  await charactersNavItem.click();
  await this.browser.sleep(500);
});

// Navigation steps
When('I navigate to the {string} section', async function (sectionName) {
  const navItem = await this.browser.findElement(By.css(`a:contains("${sectionName}"), button:contains("${sectionName}")`));
  await navItem.click();
  await this.browser.sleep(500);
});

When('I look at the navigation menu', async function () {
  // Just ensure the navigation is visible
  const navigation = await this.browser.findElement(By.css('nav, .nav, .navigation'));
  expect(navigation).to.exist;
});

When('I click the {string} navigation option', async function (navOptionName) {
  const navItem = await this.browser.findElement(By.css(`a:contains("${navOptionName}"), button:contains("${navOptionName}")`));
  await navItem.click();
  await this.browser.sleep(500);
});

// Appendix content verification steps
Then('I should see an appendix titled {string}', async function (expectedTitle) {
  const title = await this.browser.findElement(By.css('h1, h2, h3, .appendix-title'));
  const titleText = await title.getText();
  expect(titleText).to.include(expectedTitle);
});

Then('I should see all {int} characters listed with their key details', async function (expectedCount) {
  const characterElements = await this.browser.findElements(By.css('.character-summary, .character-item, [data-testid*="character"]'));
  expect(characterElements).to.have.length(expectedCount);
});

Then('I should see {string} listed as {string}', async function (characterName, characterDescription) {
  const characterElement = await this.browser.findElement(By.xpath(`//*[contains(text(), "${characterName}")]`));
  const parentElement = await characterElement.findElement(By.xpath('./..'));
  const elementText = await parentElement.getText();
  expect(elementText).to.include(characterDescription);
});

Then('I should see a message {string}', async function (expectedMessage) {
  const messageElement = await this.browser.findElement(By.xpath(`//*[contains(text(), "${expectedMessage}")]`));
  expect(messageElement).to.exist;
});

Then('I should see a {string} navigation option', async function (navOptionName) {
  const navItem = await this.browser.findElement(By.css(`a:contains("${navOptionName}"), button:contains("${navOptionName}")`));
  expect(navItem).to.exist;
});

Then('I should be taken to the dramatic personae appendix view', async function () {
  // Verify we're in the appendix section by checking for appendix-specific content
  const appendixContent = await this.browser.findElement(By.css('.appendix-content, [data-testid="dramatic-personae-appendix"]'));
  expect(appendixContent).to.exist;
});

// Read-only verification steps
Then('the character information should be displayed for reference only', async function () {
  // Verify no editable form fields are present in appendix view
  const editableFields = await this.browser.findElements(By.css('input:not([readonly]), textarea:not([readonly]), select:not([disabled])'));
  expect(editableFields).to.have.length(0);
});

Then('I should not be able to edit characters directly from the appendix', async function () {
  // Verify no edit buttons or editable content in appendix
  const editButtons = await this.browser.findElements(By.css('button:contains("Edit"), button:contains("Save"), button:contains("Delete")'));
  expect(editButtons).to.have.length(0);
});

Then('I should see a link or button to edit characters in the Characters section', async function () {
  const editLink = await this.browser.findElement(By.css('a:contains("Edit Characters"), button:contains("Edit Characters"), a[href*="Characters"]'));
  expect(editLink).to.exist;
});
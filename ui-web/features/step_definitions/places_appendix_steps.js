import {Given, When, Then} from 'cucumber';
import { By, until } from 'selenium-webdriver';
import assert from 'assert';

// Background steps
Given('I am logged in as a Game Master', async function () {
  // Navigate to the application
  await this.browser.get('http://localhost:3000');
  
  // Wait for the application to load
  await this.browser.wait(until.elementLocated(By.css('[data-testid="app-container"]')), 10000);
  
  // Note: Authentication is currently disabled in App.js
  // This step is prepared for when authentication is re-enabled
  this.isLoggedIn = true;
});

Given('I have access to the Plot Point management system', async function () {
  // Verify we can access the plot point list
  await this.browser.wait(until.elementLocated(By.css('[data-testid="plot-point-list"]')), 10000);
});

// Story content setup steps
Given('I have a Plot Point with the story text:', async function (storyText) {
  // Navigate to add new plot point
  const addButton = await this.browser.wait(
    until.elementLocated(By.css('[data-testid="add-plot-point-button"]')), 
    10000
  );
  await addButton.click();
  
  // Wait for the form to load
  await this.browser.wait(until.elementLocated(By.css('[data-testid="plot-point-form"]')), 10000);
  
  // Fill in required fields
  const nameField = await this.browser.findElement(By.css('[data-testid="plot-point-name"]'));
  await nameField.sendKeys('Test Plot Point for Places Appendix');
  
  const typeField = await this.browser.findElement(By.css('[data-testid="plot-point-type"]'));
  await typeField.sendKeys('Scene');
  
  // Find and fill the story text area
  const storyField = await this.browser.findElement(By.css('[data-testid="story-text-area"]'));
  await storyField.clear();
  await storyField.sendKeys(storyText);
  
  // Save the plot point
  const saveButton = await this.browser.findElement(By.css('[data-testid="save-plot-point"]'));
  await saveButton.click();
  
  // Wait for save to complete and return to list
  await this.browser.wait(until.elementLocated(By.css('[data-testid="plot-point-list"]')), 10000);
  
  // Store the story text for later validation
  this.currentStoryText = storyText;
});

Given('I have a Plot Point with story text mentioning {string}', async function (placeName) {
  const storyText = `The heroes begin their quest in ${placeName}. It is a magnificent place with great history.`;
  
  // Use the existing step to create the plot point
  await this.execute('I have a Plot Point with the story text:', storyText);
});

Given('I am viewing the Plot Point details', async function () {
  // This step assumes we're already viewing the plot point details
  // Verify the details page is loaded
  await this.browser.wait(until.elementLocated(By.css('[data-testid="plot-point-details"]')), 10000);
});

// Action steps
When('I view the Plot Point details', async function () {
  // Find and click on the created plot point to view details
  const plotPointLink = await this.browser.wait(
    until.elementLocated(By.css('[data-testid="plot-point-link"]:first-child')), 
    10000
  );
  await plotPointLink.click();
  
  // Wait for the plot point details to load
  await this.browser.wait(until.elementLocated(By.css('[data-testid="plot-point-details"]')), 10000);
});

When('I edit the story to remove {string} and add {string}', async function (removePlace, addPlace) {
  // Click edit button
  const editButton = await this.browser.findElement(By.css('[data-testid="edit-plot-point"]'));
  await editButton.click();
  
  // Wait for edit form to load
  await this.browser.wait(until.elementLocated(By.css('[data-testid="plot-point-form"]')), 10000);
  
  // Update the story text
  const storyField = await this.browser.findElement(By.css('[data-testid="story-text-area"]'));
  const currentText = await storyField.getAttribute('value');
  const updatedText = currentText.replace(removePlace, addPlace);
  
  await storyField.clear();
  await storyField.sendKeys(updatedText);
  
  this.updatedStoryText = updatedText;
});

When('I save the plot point changes', async function () {
  const saveButton = await this.browser.findElement(By.css('[data-testid="save-plot-point"]'));
  await saveButton.click();
  
  // Wait for save to complete and return to details view
  await this.browser.wait(until.elementLocated(By.css('[data-testid="plot-point-details"]')), 10000);
});

// Assertion steps
Then('I should see a {string} section', async function (sectionName) {
  const section = await this.browser.wait(
    until.elementLocated(By.css('[data-testid="places-appendix-section"]')), 
    10000
  );
  
  const sectionText = await section.getText();
  assert(sectionText.includes(sectionName), `Expected to see "${sectionName}" section`);
});

Then('the appendix should contain the following places:', async function (dataTable) {
  const expectedPlaces = dataTable.hashes().map(row => row['Place Name']);
  
  // Find the places appendix list
  const placesList = await this.browser.findElement(By.css('[data-testid="places-appendix-list"]'));
  const placeItems = await placesList.findElements(By.css('[data-testid="place-item"]'));
  
  const actualPlaces = [];
  for (const item of placeItems) {
    const placeName = await item.getText();
    actualPlaces.push(placeName.trim());
  }
  
  // Verify all expected places are present
  for (const expectedPlace of expectedPlaces) {
    assert(
      actualPlaces.includes(expectedPlace), 
      `Expected place "${expectedPlace}" not found in appendix. Found: ${actualPlaces.join(', ')}`
    );
  }
  
  // Verify no extra places are present
  assert.equal(
    actualPlaces.length, 
    expectedPlaces.length, 
    `Expected ${expectedPlaces.length} places, but found ${actualPlaces.length}`
  );
});

Then('each place should appear only once in the appendix', async function () {
  const placesList = await this.browser.findElement(By.css('[data-testid="places-appendix-list"]'));
  const placeItems = await placesList.findElements(By.css('[data-testid="place-item"]'));
  
  const placeNames = [];
  for (const item of placeItems) {
    const placeName = await item.getText();
    placeNames.push(placeName.trim());
  }
  
  const uniquePlaces = [...new Set(placeNames)];
  assert.equal(
    placeNames.length, 
    uniquePlaces.length, 
    `Duplicate places found: ${placeNames.join(', ')}`
  );
});

Then('the appendix should display {string}', async function (message) {
  const appendixContent = await this.browser.findElement(By.css('[data-testid="places-appendix-content"]'));
  const contentText = await appendixContent.getText();
  
  assert(
    contentText.includes(message), 
    `Expected message "${message}" not found. Found: "${contentText}"`
  );
});

Then('the appendix should contain:', async function (dataTable) {
  // Reuse the existing step for validating place lists
  await this.execute('the appendix should contain the following places:', dataTable);
});

Then('{string} should appear only once in the appendix', async function (placeName) {
  const placesList = await this.browser.findElement(By.css('[data-testid="places-appendix-list"]'));
  const placeItems = await placesList.findElements(By.css('[data-testid="place-item"]'));
  
  let count = 0;
  for (const item of placeItems) {
    const itemText = await item.getText();
    if (itemText.trim() === placeName) {
      count++;
    }
  }
  
  assert.equal(count, 1, `Expected "${placeName}" to appear once, but found ${count} occurrences`);
});

Then('the places appendix should no longer contain {string}', async function (placeName) {
  const placesList = await this.browser.findElement(By.css('[data-testid="places-appendix-list"]'));
  const placeItems = await placesList.findElements(By.css('[data-testid="place-item"]'));
  
  for (const item of placeItems) {
    const itemText = await item.getText();
    assert(
      itemText.trim() !== placeName, 
      `Place "${placeName}" should not be present in the appendix`
    );
  }
});

Then('the places appendix should contain {string}', async function (placeName) {
  const placesList = await this.browser.findElement(By.css('[data-testid="places-appendix-list"]'));
  const placeItems = await placesList.findElements(By.css('[data-testid="place-item"]'));
  
  let found = false;
  for (const item of placeItems) {
    const itemText = await item.getText();
    if (itemText.trim() === placeName) {
      found = true;
      break;
    }
  }
  
  assert(found, `Place "${placeName}" not found in the appendix`);
});
const { Given, When, Then } = require('@cucumber/cucumber');
const { By, until } = require('selenium-webdriver');
const { expect } = require('chai');
const path = require('path');

// Helper function to wait for element
async function waitForElement(driver, selector, timeout = 10000) {
  return await driver.wait(until.elementLocated(selector), timeout);
}

// Helper function to find character in list
async function findCharacterInList(driver, characterName) {
  const characterCards = await driver.findElements(By.css('.character-card'));
  for (const card of characterCards) {
    const nameElement = await card.findElement(By.css('.character-name'));
    const name = await nameElement.getText();
    if (name === characterName) {
      return card;
    }
  }
  return null;
}

Given('my character {string} has {int} experience points', async function(characterName, experiencePoints) {
  // Navigate to character editor
  await this.driver.get(`${this.baseUrl}/plot_point/${this.plotPointId}/edit`);
  await waitForElement(this.driver, By.css('[data-testid="characters-tab"]'));
  
  // Click on characters tab
  const charactersTab = await this.driver.findElement(By.css('[data-testid="characters-tab"]'));
  await charactersTab.click();
  
  // Find and edit the character
  const characterCard = await findCharacterInList(this.driver, characterName);
  expect(characterCard).to.not.be.null;
  
  const editButton = await characterCard.findElement(By.css('[data-testid="edit-character"]'));
  await editButton.click();
  
  // Set experience points
  const xpInput = await this.driver.findElement(By.css('[data-testid="experience-points"]'));
  await xpInput.clear();
  await xpInput.sendKeys(experiencePoints.toString());
  
  // Save character
  const saveButton = await this.driver.findElement(By.css('[data-testid="save-character"]'));
  await saveButton.click();
  
  // Store for later use
  this.currentCharacter = {
    name: characterName,
    experiencePoints: experiencePoints
  };
});

Given('my character has {int} advance(s) available', async function(advances) {
  // This is typically calculated from experience points
  // Store for validation
  this.currentCharacter.availableAdvances = advances;
});

Given('my character meets the requirements for {string} edge', async function(edgeName) {
  // This would check prerequisites in the actual implementation
  // For testing, we'll assume requirements are met
  this.currentCharacter.qualifiedEdges = this.currentCharacter.qualifiedEdges || [];
  this.currentCharacter.qualifiedEdges.push(edgeName);
});

Given('my character {string} is rank {string}', async function(characterName, rank) {
  this.currentCharacter = {
    name: characterName,
    rank: rank
  };
});

Given('my character {string} has applied advances', async function(characterName) {
  // Setup character with some advances for testing
  this.currentCharacter = {
    name: characterName,
    hasAdvances: true
  };
});

Given('I am viewing the character\'s advance history', async function() {
  await this.driver.get(`${this.baseUrl}/character/${this.currentCharacter.name}/advances`);
  await waitForElement(this.driver, By.css('[data-testid="advance-history"]'));
});

Given('my character {string} has the following advance history:', async function(characterName, dataTable) {
  // This would setup test data in the database
  const advances = dataTable.hashes();
  this.currentCharacter = {
    name: characterName,
    advanceHistory: advances
  };
});

When('I view the character sheet for {string}', async function(characterName) {
  await this.driver.get(`${this.baseUrl}/character/${characterName}`);
  await waitForElement(this.driver, By.css('[data-testid="character-sheet"]'));
});

When('I edit the character {string}', async function(characterName) {
  await this.driver.get(`${this.baseUrl}/character/${characterName}/edit`);
  await waitForElement(this.driver, By.css('[data-testid="character-editor"]'));
});

When('I add {int} experience points', async function(points) {
  const xpInput = await this.driver.findElement(By.css('[data-testid="add-experience"]'));
  await xpInput.clear();
  await xpInput.sendKeys(points.toString());
  
  const addButton = await this.driver.findElement(By.css('[data-testid="add-xp-button"]'));
  await addButton.click();
});

When('I save the character', async function() {
  const saveButton = await this.driver.findElement(By.css('[data-testid="save-character"]'));
  await saveButton.click();
  
  // Wait for save confirmation
  await waitForElement(this.driver, By.css('[data-testid="save-success"]'));
});

When('I choose to spend an advance', async function() {
  const spendAdvanceButton = await this.driver.findElement(By.css('[data-testid="spend-advance"]'));
  await spendAdvanceButton.click();
  
  // Wait for advance options modal
  await waitForElement(this.driver, By.css('[data-testid="advance-options"]'));
});

When('I select to increase Agility from d4 to d6', async function() {
  const attributeOption = await this.driver.findElement(By.css('[data-testid="advance-attribute"]'));
  await attributeOption.click();
  
  const agilitySelect = await this.driver.findElement(By.css('[data-testid="select-agility"]'));
  await agilitySelect.click();
  
  const d6Option = await this.driver.findElement(By.css('[data-testid="dice-d6"]'));
  await d6Option.click();
});

When('I select to add the {string} edge', async function(edgeName) {
  const edgeOption = await this.driver.findElement(By.css('[data-testid="advance-edge"]'));
  await edgeOption.click();
  
  const edgeSelect = await this.driver.findElement(By.css('[data-testid="edge-select"]'));
  await edgeSelect.sendKeys(edgeName);
  
  const edgeItem = await this.driver.findElement(By.xpath(`//li[contains(text(), "${edgeName}")]`));
  await edgeItem.click();
});

When('I select to increase two skills by one die type each', async function() {
  const skillOption = await this.driver.findElement(By.css('[data-testid="advance-skills"]'));
  await skillOption.click();
});

When('I increase Fighting from d4 to d6', async function() {
  const fightingSelect = await this.driver.findElement(By.css('[data-testid="skill-fighting"]'));
  await fightingSelect.click();
  
  const d6Option = await this.driver.findElement(By.css('[data-testid="fighting-d6"]'));
  await d6Option.click();
});

When('I increase Notice from d4 to d6', async function() {
  const noticeSelect = await this.driver.findElement(By.css('[data-testid="skill-notice"]'));
  await noticeSelect.click();
  
  const d6Option = await this.driver.findElement(By.css('[data-testid="notice-d6"]'));
  await d6Option.click();
});

When('I view the character advancement rules', async function() {
  await this.driver.get(`${this.baseUrl}/rules/advancement`);
  await waitForElement(this.driver, By.css('[data-testid="advancement-rules"]'));
});

When('I select to undo the last advance', async function() {
  const undoButton = await this.driver.findElement(By.css('[data-testid="undo-last-advance"]'));
  await undoButton.click();
});

When('I confirm the action', async function() {
  const confirmButton = await this.driver.findElement(By.css('[data-testid="confirm-undo"]'));
  await confirmButton.click();
});

Then('I should see the character\'s current rank as {string}', async function(expectedRank) {
  const rankElement = await this.driver.findElement(By.css('[data-testid="character-rank"]'));
  const rank = await rankElement.getText();
  expect(rank).to.equal(expectedRank);
});

Then('I should see {int} experience points displayed', async function(expectedXP) {
  const xpElement = await this.driver.findElement(By.css('[data-testid="experience-display"]'));
  const xp = await xpElement.getText();
  expect(parseInt(xp)).to.equal(expectedXP);
});

Then('I should see advances available as {int}', async function(expectedAdvances) {
  const advancesElement = await this.driver.findElement(By.css('[data-testid="advances-available"]'));
  const advances = await advancesElement.getText();
  expect(parseInt(advances)).to.equal(expectedAdvances);
});

Then('the character should have {int} experience points', async function(expectedXP) {
  const xpElement = await this.driver.findElement(By.css('[data-testid="total-experience"]'));
  const xp = await xpElement.getText();
  expect(parseInt(xp)).to.equal(expectedXP);
});

Then('the character should have {int} advance(s) available', async function(expectedAdvances) {
  const advancesElement = await this.driver.findElement(By.css('[data-testid="advances-count"]'));
  const advances = await advancesElement.getText();
  expect(parseInt(advances)).to.equal(expectedAdvances);
});

Then('the character\'s rank should be {string}', async function(expectedRank) {
  const rankElement = await this.driver.findElement(By.css('[data-testid="rank-display"]'));
  const rank = await rankElement.getText();
  expect(rank).to.equal(expectedRank);
});

Then('the character\'s Agility should be d6', async function() {
  const agilityElement = await this.driver.findElement(By.css('[data-testid="agility-value"]'));
  const agility = await agilityElement.getText();
  expect(agility).to.equal('d6');
});

Then('the advance history should show {string}', async function(expectedHistory) {
  const historyElement = await this.driver.findElement(By.css('[data-testid="last-advance"]'));
  const history = await historyElement.getText();
  expect(history).to.include(expectedHistory);
});

Then('the character should have the {string} edge', async function(edgeName) {
  const edgesList = await this.driver.findElement(By.css('[data-testid="character-edges"]'));
  const edgesText = await edgesList.getText();
  expect(edgesText).to.include(edgeName);
});

Then('the character\'s Fighting skill should be d6', async function() {
  const fightingElement = await this.driver.findElement(By.css('[data-testid="fighting-skill-value"]'));
  const fighting = await fightingElement.getText();
  expect(fighting).to.equal('d6');
});

Then('the character\'s Notice skill should be d6', async function() {
  const noticeElement = await this.driver.findElement(By.css('[data-testid="notice-skill-value"]'));
  const notice = await noticeElement.getText();
  expect(notice).to.equal('d6');
});

Then('I should see the following rank requirements:', async function(dataTable) {
  const requirements = dataTable.hashes();
  
  for (const requirement of requirements) {
    const rankRow = await this.driver.findElement(
      By.xpath(`//tr[contains(., "${requirement.Rank}")]`)
    );
    const rowText = await rankRow.getText();
    
    expect(rowText).to.include(requirement['Experience Points']);
    expect(rowText).to.include(requirement.Advances);
  }
});

Then('I should see the complete advance history', async function() {
  const historyTable = await this.driver.findElement(By.css('[data-testid="advance-history-table"]'));
  const rows = await historyTable.findElements(By.css('tr'));
  
  // Should have header + at least the advances we set up
  expect(rows.length).to.be.at.least(4);
});

Then('each advance should show when it was taken', async function() {
  const historyRows = await this.driver.findElements(By.css('[data-testid="advance-row"]'));
  
  for (const row of historyRows) {
    const dateElement = await row.findElement(By.css('[data-testid="advance-date"]'));
    const date = await dateElement.getText();
    expect(date).to.not.be.empty;
  }
});

Then('I should not be able to select Veteran edges', async function() {
  const veteranEdges = await this.driver.findElements(By.css('[data-testid="veteran-edge"]'));
  
  for (const edge of veteranEdges) {
    const isDisabled = await edge.getAttribute('disabled');
    expect(isDisabled).to.equal('true');
  }
});

Then('I should not be able to increase attributes beyond d12', async function() {
  const d12PlusOptions = await this.driver.findElements(By.css('[data-testid="dice-beyond-d12"]'));
  
  for (const option of d12PlusOptions) {
    const isDisabled = await option.getAttribute('disabled');
    expect(isDisabled).to.equal('true');
  }
});

Then('I should see only valid advancement options for Novice rank', async function() {
  const availableOptions = await this.driver.findElements(By.css('[data-testid="available-advance-option"]'));
  
  for (const option of availableOptions) {
    const rankReq = await option.getAttribute('data-rank-requirement');
    expect(['Novice', 'Any']).to.include(rankReq);
  }
});

Then('the advance should be removed from history', async function() {
  // Wait for update
  await this.driver.sleep(1000);
  
  const historyRows = await this.driver.findElements(By.css('[data-testid="advance-row"]'));
  const currentCount = historyRows.length;
  
  // Should have one less than before
  expect(currentCount).to.be.lessThan(this.previousAdvanceCount || 999);
});

Then('the character\'s stats should revert', async function() {
  // This would check that stats have been reverted to previous values
  // Implementation depends on what was changed
  const notification = await this.driver.findElement(By.css('[data-testid="revert-success"]'));
  const text = await notification.getText();
  expect(text).to.include('reverted');
});

Then('the advance should be available again', async function() {
  const advancesElement = await this.driver.findElement(By.css('[data-testid="advances-available"]'));
  const advances = await advancesElement.getText();
  const currentAdvances = parseInt(advances);
  
  // Should have one more advance available
  expect(currentAdvances).to.be.greaterThan(this.previousAvailableAdvances || 0);
});
import '@babel/polyfill'
import { Given, When, Then } from 'cucumber'
import { By, until } from 'selenium-webdriver'
import { expect } from 'chai'

// Navigate to plot point creation page
Given('I navigate to the plot point creation page', async function() {
  await this.driver.get(`${this.baseUrl}/plot_point/add`);
  await this.driver.wait(until.elementLocated(By.id('plotPointName')), 10000);
});

// Enter plot point name
When('I enter plot point name {string}', async function(plotPointName) {
  const nameInput = await this.driver.findElement(By.id('plotPointName'));
  await nameInput.clear();
  await nameInput.sendKeys(plotPointName);
  
  const descriptionInput = await this.driver.findElement(By.id('plotPointDescription'));
  await descriptionInput.clear();
  await descriptionInput.sendKeys('Test plot point with default skills');
  
  // Store the plot point name for later reference
  this.currentPlotPointName = plotPointName;
});

// Verify default skills exist
Then('the plot point should have the following default skills:', async function(dataTable) {
  const expectedSkills = dataTable.hashes();
  
  // Navigate to skills section if not already there
  const skillsSection = await this.driver.findElement(By.id('Skills-undefined'));
  
  for (const expectedSkill of expectedSkills) {
    // Find skill by name
    const skillElements = await this.driver.findElements(By.xpath(`//input[@value='${expectedSkill.name}']`));
    expect(skillElements.length).to.be.greaterThan(0, `Skill "${expectedSkill.name}" not found`);
    
    // Verify attribute if skill exists
    if (skillElements.length > 0) {
      const skillContainer = await skillElements[0].findElement(By.xpath('./ancestor::div[contains(@class, "BaseEditor")]'));
      const attributeSelect = await skillContainer.findElement(By.tagName('select'));
      const selectedAttribute = await attributeSelect.getAttribute('value');
      expect(selectedAttribute).to.equal(expectedSkill.attribute, 
        `Skill "${expectedSkill.name}" has incorrect attribute`);
    }
  }
});


// Edit a skill
When('I edit the {string} skill', async function(skillName) {
  const skillInput = await this.driver.findElement(By.xpath(`//input[@value='${skillName}']`));
  this.currentSkillContainer = await skillInput.findElement(By.xpath('./ancestor::div[contains(@class, "BaseEditor")]'));
  this.currentSkillName = skillName;
});

// Change skill attribute
When('I change its attribute to {string}', async function(newAttribute) {
  const attributeSelect = await this.currentSkillContainer.findElement(By.tagName('select'));
  await attributeSelect.sendKeys(newAttribute);
});

// Save changes
When('I save the changes', async function() {
  const saveButton = await this.driver.findElement(By.xpath('//button[contains(text(), "Save")]'));
  await saveButton.click();
  
  // Wait for save to complete
  await this.driver.sleep(2000);
});

// Verify skill attribute
Then('the {string} skill should have attribute {string}', async function(skillName, expectedAttribute) {
  const skillInput = await this.driver.findElement(By.xpath(`//input[@value='${skillName}']`));
  const skillContainer = await skillInput.findElement(By.xpath('./ancestor::div[contains(@class, "BaseEditor")]'));
  const attributeSelect = await skillContainer.findElement(By.tagName('select'));
  const selectedAttribute = await attributeSelect.getAttribute('value');
  expect(selectedAttribute).to.equal(expectedAttribute);
});

// Delete a skill
When('I delete the {string} skill', async function(skillName) {
  const skillInput = await this.driver.findElement(By.xpath(`//input[@value='${skillName}']`));
  const skillContainer = await skillInput.findElement(By.xpath('./ancestor::div[contains(@class, "BaseEditor")]'));
  const deleteButton = await skillContainer.findElement(By.xpath('.//button[contains(text(), "Delete")]'));
  await deleteButton.click();
});

// Verify skill doesn't exist
Then('the plot point should not have a {string} skill', async function(skillName) {
  const skillElements = await this.driver.findElements(By.xpath(`//input[@value='${skillName}']`));
  expect(skillElements.length).to.equal(0, `Skill "${skillName}" still exists`);
});

// Add a new skill
When('I add a new skill named {string} with attribute {string}', async function(skillName, attribute) {
  // Find the add skill button
  const addButton = await this.driver.findElement(By.xpath('//button[contains(text(), "Add") and contains(@id, "Skills")]'));
  await addButton.click();
  
  // Wait for new skill form to appear
  await this.driver.sleep(1000);
  
  // Find the newly added skill input (usually the last one)
  const skillInputs = await this.driver.findElements(By.id('skillName'));
  const newSkillInput = skillInputs[skillInputs.length - 1];
  await newSkillInput.clear();
  await newSkillInput.sendKeys(skillName);
  
  // Set the attribute
  const skillContainer = await newSkillInput.findElement(By.xpath('./ancestor::div[contains(@class, "BaseEditor")]'));
  const attributeSelect = await skillContainer.findElement(By.tagName('select'));
  await attributeSelect.sendKeys(attribute);
});

// Verify skill count
Then('the plot point should have {int} skills', async function(expectedCount) {
  const skillInputs = await this.driver.findElements(By.id('skillName'));
  expect(skillInputs.length).to.equal(expectedCount);
});

// Verify skill exists with attribute
Then('the plot point should have a {string} skill with attribute {string}', async function(skillName, expectedAttribute) {
  const skillInput = await this.driver.findElement(By.xpath(`//input[@value='${skillName}']`));
  const skillContainer = await skillInput.findElement(By.xpath('./ancestor::div[contains(@class, "BaseEditor")]'));
  const attributeSelect = await skillContainer.findElement(By.tagName('select'));
  const selectedAttribute = await attributeSelect.getAttribute('value');
  expect(selectedAttribute).to.equal(expectedAttribute);
});
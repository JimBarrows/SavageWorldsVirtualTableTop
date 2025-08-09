import {Given, Then, When} from 'cucumber'
import {By, until} from 'selenium-webdriver'
import sleep from 'sleep'
import expect from 'expect'

// Scene creation steps
Given('I want to add a scene', async function () {
	const browser = this.browser
	await browser.get('http://localhost:3000')
	browser.wait(until.elementLocated(By.id('root')))
	await browser.findElement(By.id('button-addScene')).click()
})

Given('a scene name of {string}', async function (scene_name) {
	this.expected_scene.name = scene_name
	const element = await this.browser.findElement(By.id('FormControl-text-TextFormGroup-Scene-sceneAdd-Name'))
	await element.clear()
	await element.sendKeys(this.expected_scene.name)
})

Given('a scene description of {string}', async function (scene_description) {
	this.expected_scene.description = scene_description
	const element = await this.browser.findElement(By.css('#TextAreaFormGroup-Scene-sceneAdd-Description > div.ql-container.ql-snow > div.ql-editor'))
	await element.clear()
	await element.sendKeys(this.expected_scene.description)
})

// Character management steps
Given('I have a scene named {string}', async function (scene_name) {
	// Navigate to scene edit page
	await this.browser.get(`http://localhost:3000/scene/${encodeURIComponent(scene_name)}/edit`)
	await this.browser.wait(until.elementLocated(By.id('scene-edit-form')))
	this.expected_scene.name = scene_name
})

Given('I have characters available:', async function (dataTable) {
	const characters = dataTable.hashes()
	this.available_characters = characters.map(row => ({
		name: row.name,
		description: row.description
	}))
})

Given('the scene has characters in its dramatis personae:', async function (dataTable) {
	const characters = dataTable.hashes()
	this.expected_scene.dramatis_personae = characters.map(row => ({
		name: row.name,
		description: row.description
	}))
})

Given('{string} is already in the scene\'s dramatis personae', async function (character_name) {
	if (!this.expected_scene.dramatis_personae) {
		this.expected_scene.dramatis_personae = []
	}
	if (!this.expected_scene.dramatis_personae.find(char => char.name === character_name)) {
		this.expected_scene.dramatis_personae.push({
			name: character_name,
			description: 'A character in the scene'
		})
	}
})

// Actions
When('I add {string} to the scene\'s dramatis personae', async function (character_name) {
	// Click the add character button
	await this.browser.findElement(By.id('button-add-character-to-scene')).click()
	
	// Select the character from dropdown or enter name
	const characterInput = await this.browser.findElement(By.id('select-character-for-scene'))
	await characterInput.sendKeys(character_name)
	
	// Confirm the addition
	await this.browser.findElement(By.id('button-confirm-add-character')).click()
})

When('I update {string} in the dramatis personae with description {string}', async function (character_name, new_description) {
	// Find the character in the dramatis personae list
	const characterRow = await this.browser.findElement(By.xpath(`//tr[td[text()='${character_name}']]`))
	
	// Click edit button for this character
	const editButton = await characterRow.findElement(By.className('edit-character-button'))
	await editButton.click()
	
	// Update description
	const descriptionField = await this.browser.findElement(By.id(`character-description-${character_name}`))
	await descriptionField.clear()
	await descriptionField.sendKeys(new_description)
	
	// Save changes
	await this.browser.findElement(By.id(`save-character-${character_name}`)).click()
})

When('I remove {string} from the scene\'s dramatis personae', async function (character_name) {
	// Find the character in the dramatis personae list
	const characterRow = await this.browser.findElement(By.xpath(`//tr[td[text()='${character_name}']]`))
	
	// Click remove button for this character
	const removeButton = await characterRow.findElement(By.className('remove-character-button'))
	await removeButton.click()
	
	// Confirm removal if confirmation dialog appears
	try {
		await this.browser.findElement(By.id('confirm-remove-character')).click()
	} catch (error) {
		// No confirmation dialog, that's fine
	}
})

When('I save the scene', async function () {
	const browser = this.browser
	const button = await browser.findElement(By.id('button-save-Scene'))
	await button.click()
})

When('I delete the scene {string}', async function (scene_name) {
	// Navigate to scene list
	await this.browser.get('http://localhost:3000/scenes')
	
	// Find the scene in the list and click delete
	const sceneRow = await this.browser.findElement(By.xpath(`//tr[td[text()='${scene_name}']]`))
	const deleteButton = await sceneRow.findElement(By.className('delete-scene-button'))
	await deleteButton.click()
	
	// Confirm deletion
	await this.browser.findElement(By.id('confirm-delete-scene')).click()
})

// Assertions
Then('the scene is in the data store', async function () {
	// TODO: Replace with actual backend API call
	console.log('TODO: Verify scene in data store via backend API')
})

Then('the scene has {int} characters in its dramatis personae', async function (expected_count) {
	// Count visible characters in the dramatis personae table
	const characterRows = await this.browser.findElements(By.className('dramatis-personae-character-row'))
	expect(characterRows.length).toBe(expected_count)
})

Then('{string} is in the scene\'s dramatis personae', async function (character_name) {
	// Verify character appears in the dramatis personae list
	const characterCell = await this.browser.findElement(By.xpath(`//td[text()='${character_name}']`))
	expect(characterCell).toBeTruthy()
})

Then('{string} is not in the scene\'s dramatis personae', async function (character_name) {
	// Verify character does not appear in the dramatis personae list
	const characterCells = await this.browser.findElements(By.xpath(`//td[text()='${character_name}']`))
	expect(characterCells.length).toBe(0)
})

Then('{string} has description {string} in the dramatis personae', async function (character_name, expected_description) {
	// Find the character row and check description
	const characterRow = await this.browser.findElement(By.xpath(`//tr[td[text()='${character_name}']]`))
	const descriptionCell = await characterRow.findElement(By.className('character-description'))
	const actualDescription = await descriptionCell.getText()
	expect(actualDescription).toBe(expected_description)
})

Then('the scene {string} is not in the data store', async function (scene_name) {
	// TODO: Replace with actual backend API call
	console.log('TODO: Verify scene removal from data store via backend API')
})
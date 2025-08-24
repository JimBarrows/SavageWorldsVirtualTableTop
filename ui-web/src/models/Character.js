import Attribute from './Attribute'
import CharacterAdvancement from './CharacterAdvancement'

export default class Character {
	agility            = new Attribute()
	animalIntelligence = false
	armor              = 0
	background         = ''
	charisma           = 0
	description        = ''
	edges              = []
	hindrances         = []
	name               = ' '
	pace               = 6
	skills             = []
	smarts             = new Attribute()
	spirit             = new Attribute()
	strength           = new Attribute()
	vigor              = new Attribute()
	advancement        = new CharacterAdvancement()

	// Experience and leveling methods
	addExperience(points) {
		this.advancement.addExperience(points)
	}

	getExperience() {
		return this.advancement.experiencePoints
	}

	getRank() {
		return this.advancement.getRank()
	}

	getAvailableAdvances() {
		return this.advancement.getAvailableAdvances()
	}

	getTotalAdvances() {
		return Math.floor(this.advancement.experiencePoints / 5)
	}

	getSpentAdvances() {
		return this.advancement.spentAdvances
	}

	// Attribute advancement
	increaseAttribute(attributeName) {
		const attribute = this[attributeName]
		if (!attribute || !attribute.die) {
			return { success: false, error: 'Invalid attribute' }
		}

		const currentDie = attribute.die
		const nextDie = this.getNextDie(currentDie)

		if (!nextDie) {
			return { success: false, error: 'Attribute cannot increase further' }
		}

		// Check rank limits for attributes
		if (!this.canIncreaseAttributeByRank(attributeName)) {
			return { success: false, error: 'Attribute increase limited by rank' }
		}

		const result = this.advancement.applyAttributeAdvance(
			attributeName,
			currentDie,
			nextDie
		)

		if (result.success) {
			attribute.die = nextDie
		}

		return result
	}

	// Edge management
	addEdgeWithAdvance(edgeName) {
		if (this.edges.includes(edgeName)) {
			return { success: false, error: 'Character already has this edge' }
		}

		const result = this.advancement.applyEdgeAdvance(edgeName)
		
		if (result.success) {
			this.edges.push(edgeName)
		}

		return result
	}

	canTakeEdge(edgeName, requirements) {
		// Check rank requirement
		if (requirements.rank && !this.advancement.canTakeEdge(edgeName, requirements.rank)) {
			return false
		}

		// Check other requirements
		return this.advancement.meetsRequirements(requirements, this)
	}

	hasEdge(edgeName) {
		return this.edges.includes(edgeName)
	}

	// Skill management
	increaseSkills(skillNames) {
		if (skillNames.length > 2) {
			return { success: false, error: 'Can only increase 2 skills per advance' }
		}

		const skillUpdates = []
		for (const skillName of skillNames) {
			const skill = this.getSkill(skillName)
			if (!skill) {
				return { success: false, error: `Skill ${skillName} not found` }
			}

			const nextDie = this.getNextDie(skill.die)
			if (!nextDie) {
				return { success: false, error: `Cannot increase ${skillName} further` }
			}

			// Check if skill can exceed linked attribute
			if (skill.linkedAttribute) {
				const attribute = this[skill.linkedAttribute]
				if (attribute && !this.canSkillExceedAttribute(nextDie, attribute.die)) {
					return { success: false, error: `${skillName} cannot exceed linked attribute` }
				}
			}

			skillUpdates.push({
				name: skillName,
				from: skill.die,
				to: nextDie
			})
		}

		const result = this.advancement.applySkillAdvance(skillUpdates)
		
		if (result.success) {
			for (const update of skillUpdates) {
				const skill = this.getSkill(update.name)
				skill.die = update.to
			}
		}

		return result
	}

	addNewSkill(skillName, die) {
		if (this.getSkill(skillName)) {
			return { success: false, error: 'Skill already exists' }
		}

		const result = this.advancement.applyNewSkillAdvance(skillName, die)
		
		if (result.success) {
			this.skills.push({ name: skillName, die })
		}

		return result
	}

	getSkill(skillName) {
		return this.skills.find(s => s.name === skillName)
	}

	// Advance history
	getAdvanceHistory() {
		return this.advancement.advanceHistory
	}

	getFormattedAdvanceHistory() {
		return this.advancement.advanceHistory.map((advance, index) => ({
			number: index + 1,
			type: advance.type,
			description: advance.description,
			date: advance.timestamp
		}))
	}

	undoLastAdvance() {
		const result = this.advancement.undoLastAdvance()
		
		if (result.success && result.undone) {
			// Revert the change based on type
			this.revertAdvance(result.undone)
		}

		return result
	}

	revertAdvance(advance) {
		switch (advance.type) {
			case 'attribute':
				this[advance.attribute].die = advance.previousValue
				break
			case 'edge': {
				const edgeIndex = this.edges.indexOf(advance.edge)
				if (edgeIndex > -1) {
					this.edges.splice(edgeIndex, 1)
				}
				break
			}
			case 'skills':
				for (const skillUpdate of advance.skills) {
					const skill = this.getSkill(skillUpdate.name)
					if (skill) {
						skill.die = skillUpdate.from
					}
				}
				break
			case 'new_skill': {
				const skillIndex = this.skills.findIndex(s => s.name === advance.skill)
				if (skillIndex > -1) {
					this.skills.splice(skillIndex, 1)
				}
				break
			}
		}
	}

	// Display helpers
	getAdvancementDisplay() {
		const progress = this.advancement.getProgressToNextRank()
		
		return {
			experiencePoints: this.advancement.experiencePoints,
			rank: this.getRank(),
			totalAdvances: this.getTotalAdvances(),
			spentAdvances: this.getSpentAdvances(),
			availableAdvances: this.getAvailableAdvances(),
			nextRankAt: progress.nextRankXP,
			progressToNextRank: `${progress.percentage.toFixed(1)}%`
		}
	}

	// Helper methods
	getNextDie(currentDie) {
		const progression = {
			'd4': 'd6',
			'd6': 'd8',
			'd8': 'd10',
			'd10': 'd12',
			'd12': 'd12+1',
			'd12+1': 'd12+2'
		}
		return progression[currentDie]
	}

	canIncreaseAttributeByRank(attributeName) {
		// Simplified - in full implementation would check how many attributes
		// have been raised this rank
		const rank = this.getRank()
		if (rank === 'Novice') {
			// Check if already raised an attribute this rank
			const attributeAdvances = this.advancement.advanceHistory
				.filter(a => a.type === 'attribute')
			return attributeAdvances.length < 1
		}
		return true
	}

	canSkillExceedAttribute(skillDie, attributeDie) {
		// Skills normally cannot exceed their linked attribute
		const diceOrder = ['d4', 'd6', 'd8', 'd10', 'd12', 'd12+1', 'd12+2']
		const skillIndex = diceOrder.indexOf(skillDie)
		const attrIndex = diceOrder.indexOf(attributeDie)
		
		return skillIndex <= attrIndex
	}

	calculateToughness() {
		// Base toughness is 2 + half Vigor
		const vigorValue = this.getDieValue(this.vigor.die)
		return 2 + Math.floor(vigorValue / 2)
	}

	getDieValue(die) {
		const values = {
			'd4': 4,
			'd6': 6,
			'd8': 8,
			'd10': 10,
			'd12': 12,
			'd12+1': 13,
			'd12+2': 14
		}
		return values[die] || 4
	}
}

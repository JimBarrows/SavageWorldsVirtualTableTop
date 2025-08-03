import Power from './Power';
import Modifier from './Modifier';

export default class Powers {
	arcaneBackgrounds = []
	powers = []

	constructor(data = {}) {
		this.arcaneBackgrounds = data.arcaneBackgrounds || [];
		this.powers = data.powers || [];
	}

	addPower(power) {
		this.powers.push(power);
	}

	removePower(index) {
		if (index >= 0 && index < this.powers.length) {
			this.powers.splice(index, 1);
		}
	}

	static getDefaultPowers() {
		const defaultPowers = [];

		// Arcane Protection
		const arcaneProtection = new Power({
			name: 'Arcane Protection',
			description: 'Opposing powers suffer a penalty',
			powerPoints: 1,
			range: 'Touch',
			duration: '5 rounds',
			trappings: 'Mystical glow, runes, sigils',
			rank: 'Novice'
		});
		defaultPowers.push(arcaneProtection);

		// Bolt
		const bolt = new Power({
			name: 'Bolt',
			description: 'Hurls damaging bolts of energy',
			powerPoints: 1,
			range: 'Smarts x2',
			duration: 'Instant',
			trappings: 'Fire, ice, light, darkness, colored bolts',
			rank: 'Novice'
		});
		defaultPowers.push(bolt);

		// Boost/Lower Trait
		const boostLowerTrait = new Power({
			name: 'Boost/Lower Trait',
			description: 'Increases or decreases a trait',
			powerPoints: 2,
			range: 'Smarts',
			duration: '5 rounds',
			trappings: 'Physical change, glowing aura, potions',
			rank: 'Novice'
		});
		defaultPowers.push(boostLowerTrait);

		// Detect/Conceal Arcana
		const detectConcealArcana = new Power({
			name: 'Detect/Conceal Arcana',
			description: 'Detects or conceals magical effects',
			powerPoints: 2,
			range: 'Sight',
			duration: '5 rounds',
			trappings: 'Waving hands, whispered words',
			rank: 'Novice'
		});
		defaultPowers.push(detectConcealArcana);

		// Healing
		const healing = new Power({
			name: 'Healing',
			description: 'Restores wounds',
			powerPoints: 3,
			range: 'Touch',
			duration: 'Instant',
			trappings: 'Laying on hands, prayer, mystic energy',
			rank: 'Novice'
		});
		defaultPowers.push(healing);

		return defaultPowers;
	}

	toJSON() {
		return {
			arcaneBackgrounds: this.arcaneBackgrounds,
			powers: this.powers.map(p => p.toJSON ? p.toJSON() : p)
		};
	}
}

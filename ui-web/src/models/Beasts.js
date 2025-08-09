import Beast from './Beast'

export default class Beasts {
	static getStandardBeasts() {
		// Return a new array of standard beasts from the Savage Worlds bestiary
		return [
			this.createBeast({
				name: 'Alligator/Crocodile',
				description: 'These large reptiles are usually found in swamps and rivers.',
				agility: { die: 4, modifier: 0 },
				smarts: { die: 4, modifier: 2 }, // Animal intelligence
				spirit: { die: 6, modifier: 0 },
				strength: { die: 10, modifier: 0 },
				vigor: { die: 10, modifier: 0 },
				pace: 3,
				armor: 2,
				animalIntelligence: true,
				skills: [
					{ name: 'Fighting', die: 8 },
					{ name: 'Notice', die: 6 },
					{ name: 'Stealth', die: 8 },
					{ name: 'Swimming', die: 8 }
				],
				specialAbilities: [
					{ name: 'Aquatic', description: 'Pace 5' },
					{ name: 'Armor +2', description: 'Thick skin.' },
					{ name: 'Bite', description: 'Str+d6' },
					{ name: 'Rollover', description: 'Crocodiles and alligators are notorious for grasping their prey in their vice-like jaws and rolling over and over with their flailing victims in their mouth. If one of these large amphibians hits with a raise, it causes an extra 2d4 damage to its prey in addition to its regular Strength damage.' }
				]
			}),

			this.createBeast({
				name: 'Bear',
				description: 'Large bears stand 8\' tall and weigh over 1000 pounds.',
				agility: { die: 6, modifier: 0 },
				smarts: { die: 4, modifier: 2 }, // Animal intelligence
				spirit: { die: 8, modifier: 0 },
				strength: { die: 12, modifier: 4 },
				vigor: { die: 12, modifier: 0 },
				pace: 8,
				armor: 0,
				animalIntelligence: true,
				skills: [
					{ name: 'Fighting', die: 8 },
					{ name: 'Notice', die: 8 },
					{ name: 'Swimming', die: 6 }
				],
				specialAbilities: [
					{ name: 'Bear Hug', description: 'Bears don\'t actually "hug" their victims, but they do attempt to use their weight to pin their prey and rend it with their claws and teeth. A bear that hits with a raise has pinned his foe and causes 2d4 damage to its prey in addition to its regular Strength damage.' },
					{ name: 'Claws', description: 'Str+d6' },
					{ name: 'Size +2', description: 'Bears are large creatures.' }
				]
			}),

			this.createBeast({
				name: 'Dog/Wolf',
				description: 'The stats below are for large attack dogs and wolves.',
				agility: { die: 8, modifier: 0 },
				smarts: { die: 4, modifier: 2 }, // Animal intelligence
				spirit: { die: 6, modifier: 0 },
				strength: { die: 6, modifier: 0 },
				vigor: { die: 6, modifier: 0 },
				pace: 8,
				armor: 0,
				animalIntelligence: true,
				skills: [
					{ name: 'Fighting', die: 6 },
					{ name: 'Notice', die: 10 }
				],
				specialAbilities: [
					{ name: 'Bite', description: 'Str+d4' },
					{ name: 'Fleet-Footed', description: 'Roll a d10 when running instead of a d6' },
					{ name: 'Go for the Throat', description: 'Wolves instinctively go for an opponent\'s soft spots. With a raise on its attack roll, it hits the target\'s most weakly-armored location.' }
				]
			}),

			this.createBeast({
				name: 'Ghost',
				description: 'Spectral beings that haunt specific locations.',
				agility: { die: 6, modifier: 0 },
				smarts: { die: 6, modifier: 0 },
				spirit: { die: 10, modifier: 0 },
				strength: { die: 6, modifier: 0 },
				vigor: { die: 6, modifier: 0 },
				pace: 6,
				armor: 0,
				skills: [
					{ name: 'Fighting', die: 6 },
					{ name: 'Intimidation', die: 12 },
					{ name: 'Notice', die: 12 },
					{ name: 'Stealth', die: 12 },
					{ name: 'Throwing', die: 12 }
				],
				specialAbilities: [
					{ name: 'Ethereal', description: 'Ghosts are immaterial and can only be harmed by magical attacks.' },
					{ name: 'Fear -2', description: 'Ghosts cause Fear checks at -2 when they let themselves be seen.' }
				]
			}),

			this.createBeast({
				name: 'Giant Spider',
				description: 'Giant spiders are the size of large dogs.',
				agility: { die: 10, modifier: 0 },
				smarts: { die: 4, modifier: 2 }, // Animal intelligence
				spirit: { die: 6, modifier: 0 },
				strength: { die: 10, modifier: 0 },
				vigor: { die: 6, modifier: 0 },
				pace: 8,
				armor: 0,
				animalIntelligence: true,
				skills: [
					{ name: 'Fighting', die: 8 },
					{ name: 'Intimidation', die: 10 },
					{ name: 'Notice', die: 8 },
					{ name: 'Shooting', die: 10 },
					{ name: 'Stealth', die: 10 }
				],
				specialAbilities: [
					{ name: 'Bite', description: 'Str+d4' },
					{ name: 'Poison', description: 'The spider\'s bite injects poison if the target is Shaken or wounded. The target must make a Vigor roll or die in 2d6 rounds.' },
					{ name: 'Webbing', description: 'Spiders can cast webs from their thorax that are the size of Small Burst Templates. This is a Shooting roll with a range of 3/6/12. Anything in the web must cut or break their way free (Toughness 7). Webbed characters can still fight, but all physical actions are at -4.' },
					{ name: 'Wall Walker', description: 'Can walk on vertical and inverted surfaces at Pace 8.' }
				]
			}),

			this.createBeast({
				name: 'Goblin',
				description: 'Goblins are small, green-skinned humanoids with big ears, wide mouths, and wicked temperaments.',
				agility: { die: 8, modifier: 0 },
				smarts: { die: 6, modifier: 0 },
				spirit: { die: 6, modifier: 0 },
				strength: { die: 4, modifier: 0 },
				vigor: { die: 6, modifier: 0 },
				pace: 5,
				armor: 0,
				skills: [
					{ name: 'Climbing', die: 6 },
					{ name: 'Fighting', die: 6 },
					{ name: 'Notice', die: 6 },
					{ name: 'Shooting', die: 8 },
					{ name: 'Stealth', die: 10 },
					{ name: 'Taunt', die: 6 },
					{ name: 'Throwing', die: 6 }
				],
				specialAbilities: [
					{ name: 'Infravision', description: 'Goblins halve penalties for dark lighting against living targets (round down).' },
					{ name: 'Size -1', description: 'Goblins are small creatures.' }
				]
			}),

			this.createBeast({
				name: 'Horse, Riding',
				description: 'Horses are common mounts in most fantasy and historical settings.',
				agility: { die: 8, modifier: 0 },
				smarts: { die: 4, modifier: 2 }, // Animal intelligence
				spirit: { die: 6, modifier: 0 },
				strength: { die: 12, modifier: 0 },
				vigor: { die: 8, modifier: 0 },
				pace: 8,
				armor: 0,
				animalIntelligence: true,
				skills: [
					{ name: 'Fighting', die: 4 },
					{ name: 'Notice', die: 6 }
				],
				specialAbilities: [
					{ name: 'Fleet-Footed', description: 'Roll a d8 when running instead of a d6.' },
					{ name: 'Kick', description: 'Str' },
					{ name: 'Size +2', description: 'Riding horses weigh between 800 and 1000 pounds.' }
				]
			}),

			this.createBeast({
				name: 'Lion',
				description: 'The king of beasts, lions are fierce predators.',
				agility: { die: 8, modifier: 0 },
				smarts: { die: 4, modifier: 2 }, // Animal intelligence
				spirit: { die: 10, modifier: 0 },
				strength: { die: 12, modifier: 0 },
				vigor: { die: 8, modifier: 0 },
				pace: 8,
				armor: 0,
				animalIntelligence: true,
				skills: [
					{ name: 'Fighting', die: 8 },
					{ name: 'Notice', die: 8 }
				],
				specialAbilities: [
					{ name: 'Bite or Claw', description: 'Str+d6' },
					{ name: 'Improved Frenzy', description: 'Lions may make two Fighting attacks each action at no penalty.' },
					{ name: 'Pounce', description: 'Lions often pounce on their prey to best bring their mass and claws to bear. It can leap 1d6" to gain +4 to its attack and damage. Its Parry is reduced by -2 until its next action when performing the maneuver however.' },
					{ name: 'Size +1', description: 'Male lions can weigh over 500 pounds.' }
				]
			}),

			this.createBeast({
				name: 'Orc',
				description: 'Orcs are savage, green-skinned humanoids with pig-like features.',
				agility: { die: 6, modifier: 0 },
				smarts: { die: 4, modifier: 0 },
				spirit: { die: 6, modifier: 0 },
				strength: { die: 8, modifier: 0 },
				vigor: { die: 8, modifier: 0 },
				pace: 6,
				armor: 1,
				skills: [
					{ name: 'Fighting', die: 6 },
					{ name: 'Intimidation', die: 8 },
					{ name: 'Notice', die: 6 },
					{ name: 'Shooting', die: 6 },
					{ name: 'Stealth', die: 4 },
					{ name: 'Throwing', die: 6 }
				],
				specialAbilities: [
					{ name: 'Size +1', description: 'Orcs are slightly larger than humans.' }
				]
			}),

			this.createBeast({
				name: 'Skeleton',
				description: 'The animated bones of the dead, skeletons are usually under the control of an evil wizard or priest.',
				agility: { die: 8, modifier: 0 },
				smarts: { die: 4, modifier: 0 },
				spirit: { die: 4, modifier: 0 },
				strength: { die: 6, modifier: 0 },
				vigor: { die: 6, modifier: 0 },
				pace: 7,
				armor: 0,
				skills: [
					{ name: 'Fighting', die: 6 },
					{ name: 'Intimidation', die: 6 },
					{ name: 'Notice', die: 4 },
					{ name: 'Shooting', die: 6 }
				],
				specialAbilities: [
					{ name: 'Bony Claws', description: 'Str+d4' },
					{ name: 'Fearless', description: 'Skeletons are immune to Fear and Intimidation.' },
					{ name: 'Undead', description: '+2 Toughness; +2 to recover from being Shaken; no additional damage from called shots; immune to disease and poison.' }
				]
			}),

			this.createBeast({
				name: 'Swarm',
				description: 'Swarms are composed of hundreds or thousands of creatures. Treat the swarm as a single creature.',
				agility: { die: 10, modifier: 0 },
				smarts: { die: 4, modifier: 2 }, // Animal intelligence
				spirit: { die: 12, modifier: 0 },
				strength: { die: 8, modifier: 0 },
				vigor: { die: 10, modifier: 0 },
				pace: 10,
				armor: 0,
				animalIntelligence: true,
				skills: [
					{ name: 'Notice', die: 6 }
				],
				specialAbilities: [
					{ name: 'Bite or Sting', description: 'Swarms inflict hundreds of tiny bites every round to their victims, hitting automatically and causing 2d4 damage to everyone in the template.' },
					{ name: 'Split', description: 'Some swarms are clever enough to split into two smaller swarms (Small Burst Templates) should their foes split up. The Toughness of these smaller swarms is lowered by -2 (to 5 each).' },
					{ name: 'Swarm', description: 'Parry +2; Because the swarm is composed of scores, hundreds, or thousands of creatures, cutting and piercing weapons do no real damage. Area-effect weapons work normally, and a character can stomp to inflict his damage in Strength each round. Swarms are usually foiled by jumping in water (unless they are aquatic pests, such as piranha).' }
				]
			}),

			this.createBeast({
				name: 'Zombie',
				description: 'Zombies are mindless animated corpses.',
				agility: { die: 6, modifier: 0 },
				smarts: { die: 4, modifier: 0 },
				spirit: { die: 4, modifier: 0 },
				strength: { die: 6, modifier: 0 },
				vigor: { die: 6, modifier: 0 },
				pace: 4,
				armor: 1,
				skills: [
					{ name: 'Fighting', die: 6 },
					{ name: 'Intimidation', die: 6 },
					{ name: 'Notice', die: 4 },
					{ name: 'Shooting', die: 6 }
				],
				specialAbilities: [
					{ name: 'Claws', description: 'Str' },
					{ name: 'Fearless', description: 'Zombies are immune to Fear and Intimidation.' },
					{ name: 'Undead', description: '+2 Toughness; +2 to recover from being Shaken; no additional damage from called shots; immune to disease and poison.' },
					{ name: 'Weakness (Head)', description: 'Shots to a zombie\'s head are +2 damage.' }
				]
			})
		]
	}

	static createBeast(data) {
		const beast = new Beast()
		
		// Set basic properties
		beast.name = data.name || ''
		beast.description = data.description || ''
		beast.pace = data.pace || 6
		beast.armor = data.armor || 0
		beast.animalIntelligence = data.animalIntelligence || false
		beast.charisma = data.charisma || 0
		
		// Set attributes
		if (data.agility) {
			beast.agility.die = data.agility.die || 4
			beast.agility.modifier = data.agility.modifier || 0
		}
		if (data.smarts) {
			beast.smarts.die = data.smarts.die || 4
			beast.smarts.modifier = data.smarts.modifier || 0
		}
		if (data.spirit) {
			beast.spirit.die = data.spirit.die || 4
			beast.spirit.modifier = data.spirit.modifier || 0
		}
		if (data.strength) {
			beast.strength.die = data.strength.die || 4
			beast.strength.modifier = data.strength.modifier || 0
		}
		if (data.vigor) {
			beast.vigor.die = data.vigor.die || 4
			beast.vigor.modifier = data.vigor.modifier || 0
		}
		
		// Set skills and special abilities
		beast.skills = data.skills || []
		beast.specialAbilities = data.specialAbilities || []
		
		return beast
	}
}
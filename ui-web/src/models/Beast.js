import Attribute from './Attribute'

export default class Beast {
	agility            = new Attribute()
	animalIntelligence = false
	armor              = 0
	charisma           = 0
	description        = ''
	name               = ''
	pace               = 6
	skills             = []
	smarts             = new Attribute()
	specialAbilities   = []
	spirit             = new Attribute()
	strength           = new Attribute()
	vigor              = new Attribute()
}

import Attribute from './Attribute'

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
}

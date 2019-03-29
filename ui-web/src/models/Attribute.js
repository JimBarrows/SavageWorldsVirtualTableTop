export default class Attribute {
	dice  = 'd4'
	bonus = 0

	constructor (dice, bonus) {
		this.dice  = dice === undefined ? 'd4' : dice
		this.bonus = bonus === undefined ? 0 : bonus
	}

}

package org.savageworlds.model;

import javax.persistence.Embeddable;

@Embeddable
public class Die {

	private Integer		number;
	private DiceType	dice;
	private Integer		bonus;

	public Integer getNumber() {
		return number;
	}

	public void setNumber(Integer number) {
		this.number = number;
	}

	public DiceType getDice() {
		return dice;
	}

	public void setDice(DiceType dice) {
		this.dice = dice;
	}

	public Integer getBonus() {
		return bonus;
	}

	public void setBonus(Integer bonus) {
		this.bonus = bonus;
	}
}

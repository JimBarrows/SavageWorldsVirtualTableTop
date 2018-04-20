package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.Embeddable;
import java.util.Objects;

@Embeddable
public class DiceCode {

	private int number;
	private Dice dice = Dice.d4;
	private int bonus;

	public DiceCode(final int number, final Dice dice, final int bonus) {
		this.number = number;
		this.dice = dice;
		this.bonus = bonus;
	}

	public DiceCode() {
	}

	@Override
	public int hashCode() {

		return Objects.hash(getNumber(), getDice(), getBonus());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof DiceCode)) return false;
		final DiceCode diceCode = (DiceCode) o;
		return getNumber() == diceCode.getNumber() &&
				getBonus() == diceCode.getBonus() &&
				getDice() == diceCode.getDice();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("number", number)
				.append("dice", dice)
				.append("bonus", bonus)
				.toString();
	}

	public int getNumber() {
		return number;
	}

	public void setNumber(final int number) {
		this.number = number;
	}

	public Dice getDice() {
		return dice;
	}

	public int getBonus() {
		return bonus;
	}

	public void setBonus(final int bonus) {
		this.bonus = bonus;
	}

	public void setDice(final Dice dice) {
		this.dice = dice;
	}
}

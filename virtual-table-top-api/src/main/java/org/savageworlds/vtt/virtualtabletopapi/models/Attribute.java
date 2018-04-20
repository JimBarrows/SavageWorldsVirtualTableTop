package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.Embeddable;
import javax.persistence.Enumerated;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.PositiveOrZero;
import java.util.Objects;

@Embeddable
public class Attribute {

	@NotNull
	@Enumerated
	private Dice dice = Dice.d4;

	@PositiveOrZero
	private int bonus;

	public Attribute(@NotNull final Dice dice, @PositiveOrZero final int bonus) {
		this.dice = dice;
		this.bonus = bonus;
	}

	public Attribute(@NotNull final Dice dice) {
		this.dice = dice;
	}

	public Attribute() {

	}

	@Override
	public int hashCode() {

		return Objects.hash(getDice(), getBonus());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof Attribute)) return false;
		final Attribute attribute = (Attribute) o;
		return getBonus() == attribute.getBonus() &&
				getDice() == attribute.getDice();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("dice", dice)
				.append("bonus", bonus)
				.toString();
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

package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.Embeddable;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Positive;
import java.util.Objects;

@Embeddable
public class VehicleMountedAmmunition {

	@NotNull
	private DiceCode dice;

	@Positive
	@Min(1)
	private int ap;

	public VehicleMountedAmmunition(@NotNull final DiceCode dice, @Positive @Min(1) final int ap) {
		this.dice = dice;
		this.ap = ap;
	}

	@Override
	public int hashCode() {

		return Objects.hash(getDice(), getAp());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof VehicleMountedAmmunition)) return false;
		final VehicleMountedAmmunition that = (VehicleMountedAmmunition) o;
		return getAp() == that.getAp() &&
				Objects.equals(getDice(), that.getDice());
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("dice", dice)
				.append("ap", ap)
				.toString();
	}

	public DiceCode getDice() {
		return dice;
	}

	public int getAp() {
		return ap;
	}

	public void setAp(final int ap) {
		this.ap = ap;
	}

	public void setDice(final DiceCode dice) {
		this.dice = dice;
	}
}

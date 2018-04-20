package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.Entity;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.PositiveOrZero;

@Entity
public class HandWeapon extends Gear {

	private DiceCode damage = new DiceCode(0, Dice.d4, 0);
	private boolean useStrength = true;

	public HandWeapon(final @NotEmpty String name, final String description, @PositiveOrZero final int cost, @PositiveOrZero final int weight, final @NotNull GearCategory category, final String notes, final DiceCode damage, final int bonus, final boolean useStrength, final GearType type) {
		super(name, description, cost, weight, category, notes, type);
		this.damage = damage;
		this.useStrength = useStrength;
	}

	public HandWeapon() {
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("damage", damage)
				.append("useStrength", useStrength)
				.toString();
	}

	public DiceCode getDamage() {
		return damage;
	}

	public void setDamage(final DiceCode damage) {
		this.damage = damage;
	}

	public boolean isUseStrength() {
		return useStrength;
	}

	public void setUseStrength(final boolean useStrength) {
		this.useStrength = useStrength;
	}

}

package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.Entity;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.PositiveOrZero;

@Entity
public class Armor extends Gear {

	@PositiveOrZero
	private int armor = 1;

	public Armor(final @NotEmpty String name, final String description, @PositiveOrZero final int cost, @PositiveOrZero final int weight, final @NotNull GearCategory category, final String notes, final GearType type, @PositiveOrZero final int armor) {
		super(name, description, cost, weight, category, notes, type);
		this.armor = armor;
	}

	public Armor() {
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("armor", armor)
				.toString();
	}

	public int getArmor() {
		return armor;
	}

	public void setArmor(final int armor) {
		this.armor = armor;
	}
}

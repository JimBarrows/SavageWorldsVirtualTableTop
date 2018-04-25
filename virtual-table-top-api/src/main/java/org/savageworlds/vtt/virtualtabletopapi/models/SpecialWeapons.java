package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.Enumerated;
import javax.validation.constraints.*;

@Entity
public class SpecialWeapons extends Gear {

	@Embedded
	private Ranges ranges;

	@Embedded
	private DiceCode damage;

	@Positive
	@Min(1)
	private int rateOfFire = 1;

	@Positive
	private int armorPierce = 0;

	@Enumerated
	private Dice minimumStrength;

	@Enumerated
	private BurstTemplate burst;

	public SpecialWeapons(final @NotEmpty String name, final String description, @PositiveOrZero final int cost, @PositiveOrZero final int weight, final @NotNull GearCategory category, final String notes, final GearType type, final Ranges ranges, final DiceCode damage, @Positive @Min(1) final int rateOfFire, @Positive final int armorPierce, final Dice minimumStrength, final BurstTemplate burst) {
		super(name, description, cost, weight, category, notes, type);
		this.ranges = ranges;
		this.damage = damage;
		this.rateOfFire = rateOfFire;
		this.armorPierce = armorPierce;
		this.minimumStrength = minimumStrength;
		this.burst = burst;
	}

	public SpecialWeapons() {

	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append('ranges', ranges)
				.append('damage', damage)
				.append('rateOfFire', rateOfFire)
				.append('armorPierce', armorPierce)
				.append('minimumStrength', minimumStrength)
				.append('burst', burst)
				.toString();
	}

	public Ranges getRanges() {
		return ranges;
	}

	public void setRanges(final Ranges ranges) {
		this.ranges = ranges;
	}

	public DiceCode getDamage() {
		return damage;
	}

	public void setDamage(final DiceCode damage) {
		this.damage = damage;
	}

	public int getRateOfFire() {
		return rateOfFire;
	}

	public void setRateOfFire(final int rateOfFire) {
		this.rateOfFire = rateOfFire;
	}

	public int getArmorPierce() {
		return armorPierce;
	}

	public void setArmorPierce(final int armorPierce) {
		this.armorPierce = armorPierce;
	}

	public Dice getMinimumStrength() {
		return minimumStrength;
	}

	public void setMinimumStrength(final Dice minimumStrength) {
		this.minimumStrength = minimumStrength;
	}

	public BurstTemplate getBurst() {
		return burst;
	}

	public void setBurst(final BurstTemplate burst) {
		this.burst = burst;
	}
}

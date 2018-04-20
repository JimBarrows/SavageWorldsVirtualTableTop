package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;

@Entity
public class RangedWeapon extends Gear {

	@Embedded
	private Ranges range = new Ranges(5, 10, 20);

	@Embedded
	private DiceCode damage;

	private boolean useStrength = false;

	@Positive
	private int rateOfFire = 1;

	@Positive
	private Integer shots;

	private Dice minimumStrength;

	public RangedWeapon(final @NotEmpty String name, final String description, @PositiveOrZero final int cost, @PositiveOrZero final int weight, final @NotNull GearCategory category, final String notes, final GearType type, final Ranges range, final DiceCode damage, final boolean useStrength, @Positive final int rateOfFire, @Positive final Integer shots, final Dice minimumStrength) {
		super(name, description, cost, weight, category, notes, type);
		this.range = range;
		this.damage = damage;
		this.useStrength = useStrength;
		this.rateOfFire = rateOfFire;
		this.shots = shots;
		this.minimumStrength = minimumStrength;
	}

	public RangedWeapon() {
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("range", range)
				.append("damage", damage)
				.append("useStrength", useStrength)
				.append("rateOfFire", rateOfFire)
				.append("shots", shots)
				.append("minimumStrength", minimumStrength)
				.toString();
	}

	public Ranges getRange() {
		return range;
	}

	public void setRange(final Ranges range) {
		this.range = range;
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

	public int getRateOfFire() {
		return rateOfFire;
	}

	public void setRateOfFire(final int rateOfFire) {
		this.rateOfFire = rateOfFire;
	}

	public Integer getShots() {
		return shots;
	}

	public void setShots(final Integer shots) {
		this.shots = shots;
	}

	public Dice getMinimumStrength() {
		return minimumStrength;
	}

	public void setMinimumStrength(final Dice minimumStrength) {
		this.minimumStrength = minimumStrength;
	}
}

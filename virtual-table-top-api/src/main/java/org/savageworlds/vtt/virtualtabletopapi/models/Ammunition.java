package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.Entity;
import javax.validation.constraints.*;

@Entity
public class Ammunition extends Gear {


	@Positive
	@Min(1)
	/**
	 * Ammunition weight is rated like 1/5, or 1K per 5 units.
	 */
	private long weightCount;

	@Positive
	@Min(1)
	/**
	 * Cost is per number of round.
	 */
	private long costCount;

	public Ammunition(final @NotEmpty String name, final String description, @PositiveOrZero final int cost, @PositiveOrZero final int weight, final @NotNull GearCategory category, final String notes, final GearType type, @Positive @Min(1) final long weightCount, @Positive @Min(1) final long costCount) {
		super(name, description, cost, weight, category, notes, type);
		this.weightCount = weightCount;
		this.costCount = costCount;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("weightCount", weightCount)
				.append("costCount", costCount)
				.toString();
	}
}

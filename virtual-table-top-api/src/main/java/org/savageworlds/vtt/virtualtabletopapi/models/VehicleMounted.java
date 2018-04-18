package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.Embedded;
import javax.validation.constraints.*;

public class VehicleMounted extends Gear {

	@NotNull
	@Embedded
	private Ranges range;

	@Embedded
	private VehicleMountedAmmunition apRounds;

	@Embedded
	private VehicleMountedAmmunition heRounds;

	@Positive
	@Min(1)
	private int rateOfFire;

	private BurstTemplate burstTemplate;

	public VehicleMounted(final @NotEmpty String name, final String description, @PositiveOrZero final int cost, @PositiveOrZero final int weight, final @NotNull GearCategory category, final String notes, final GearType type, @NotNull final Ranges range, final VehicleMountedAmmunition apRounds, final VehicleMountedAmmunition heRounds, @Positive @Min(1) final int rateOfFire, final BurstTemplate burstTemplate) {
		super(name, description, cost, weight, category, notes, type);
		this.range = range;
		this.apRounds = apRounds;
		this.heRounds = heRounds;
		this.rateOfFire = rateOfFire;
		this.burstTemplate = burstTemplate;
	}

	public VehicleMounted() {

	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("range", range)
				.append("apRounds", apRounds)
				.append("heRounds", heRounds)
				.append("rateOfFire", rateOfFire)
				.append("burstTemplate", burstTemplate)
				.toString();
	}

	public BurstTemplate getBurstTemplate() {
		return burstTemplate;
	}

	public void setBurstTemplate(final BurstTemplate burstTemplate) {
		this.burstTemplate = burstTemplate;
	}

	public Ranges getRange() {
		return range;
	}

	public void setRange(final Ranges range) {
		this.range = range;
	}

	public VehicleMountedAmmunition getApRounds() {
		return apRounds;
	}

	public void setApRounds(final VehicleMountedAmmunition apRounds) {
		this.apRounds = apRounds;
	}

	public VehicleMountedAmmunition getHeRounds() {
		return heRounds;
	}

	public void setHeRounds(final VehicleMountedAmmunition heRounds) {
		this.heRounds = heRounds;
	}

	public int getRateOfFire() {
		return rateOfFire;
	}

	public void setRateOfFire(final int rateOfFire) {
		this.rateOfFire = rateOfFire;
	}
}

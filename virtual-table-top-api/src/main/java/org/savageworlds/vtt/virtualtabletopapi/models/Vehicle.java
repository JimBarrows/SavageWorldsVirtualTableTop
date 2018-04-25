package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.*;
import java.util.Objects;

@Entity
public class Vehicle {
	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Version
	private long version;

	@NotEmpty
	@Column(nullable = false)
	private String name = '';

	private String description = '';

	@ManyToOne
	@NotNull
	private VehicleType type;

	@Positive
	@Min(1)
	private int acceleration;

	@Positive
	@Min(1)
	private int topSpeed;

	@Positive
	@Min(1)
	private int toughness;

	@Positive
	@Min(1)
	private int withoutArmor;

	@Positive
	@Min(1)
	private int crew;

	@PositiveOrZero
	private int passengers;

	@PositiveOrZero
	private int minimumCost;

	@PositiveOrZero
	private int maximumCost;

	private String examples;

	private boolean military = false;

	private int climb;

	public Vehicle(final long version, @NotEmpty final String name, final String description, final VehicleType type, @Positive @Min(1) final int acceleration, @Positive @Min(1) final int topSpeed, @Positive @Min(1) final int toughness, @Positive @Min(1) final int withoutArmor, @Positive @Min(1) final int crew, @PositiveOrZero final int passengers, @PositiveOrZero final int minimumCost, @PositiveOrZero final int maximumCost, final String examples, final boolean military, final int climb) {
		this.version = version;
		this.name = name;
		this.description = description;
		this.type = type;
		this.acceleration = acceleration;
		this.topSpeed = topSpeed;
		this.toughness = toughness;
		this.withoutArmor = withoutArmor;
		this.crew = crew;
		this.passengers = passengers;
		this.minimumCost = minimumCost;
		this.maximumCost = maximumCost;
		this.examples = examples;
		this.military = military;
		this.climb = climb;
	}

	@Override
	public int hashCode() {

		return Objects.hash(getId(), getVersion());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof Vehicle)) return false;
		final Vehicle vehicle = (Vehicle) o;
		return getId() == vehicle.getId() &&
				getVersion() == vehicle.getVersion();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append('id', id)
				.append('version', version)
				.append('name', name)
				.append('description', description)
				.append('type', type)
				.append('acceleration', acceleration)
				.append('topSpeed', topSpeed)
				.append('toughness', toughness)
				.append('withoutArmor', withoutArmor)
				.append('crew', crew)
				.append('passengers', passengers)
				.append('minimumCost', minimumCost)
				.append('maximumCost', maximumCost)
				.append('examples', examples)
				.append('military", military)
				.append("climb", climb)
				.toString();
	}

	public long getId() {
		return id;
	}

	public void setId(final long id) {
		this.id = id;
	}

	public long getVersion() {
		return version;
	}

	public void setVersion(final long version) {
		this.version = version;
	}

	public int getClimb() {
		return climb;
	}

	public void setClimb(final int climb) {
		this.climb = climb;
	}

	public boolean isMilitary() {
		return military;
	}

	public void setMilitary(final boolean military) {
		this.military = military;
	}

	public String getName() {
		return name;
	}

	public void setName(final String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(final String description) {
		this.description = description;
	}

	public VehicleType getType() {
		return type;
	}

	public void setType(final VehicleType type) {
		this.type = type;
	}

	public int getAcceleration() {
		return acceleration;
	}

	public void setAcceleration(final int acceleration) {
		this.acceleration = acceleration;
	}

	public int getTopSpeed() {
		return topSpeed;
	}

	public void setTopSpeed(final int topSpeed) {
		this.topSpeed = topSpeed;
	}

	public int getToughness() {
		return toughness;
	}

	public void setToughness(final int toughness) {
		this.toughness = toughness;
	}

	public int getWithoutArmor() {
		return withoutArmor;
	}

	public void setWithoutArmor(final int withoutArmor) {
		this.withoutArmor = withoutArmor;
	}

	public int getCrew() {
		return crew;
	}

	public void setCrew(final int crew) {
		this.crew = crew;
	}

	public int getPassengers() {
		return passengers;
	}

	public void setPassengers(final int passengers) {
		this.passengers = passengers;
	}

	public int getMinimumCost() {
		return minimumCost;
	}

	public void setMinimumCost(final int minimumCost) {
		this.minimumCost = minimumCost;
	}

	public int getMaximumCost() {
		return maximumCost;
	}

	public void setMaximumCost(final int maximumCost) {
		this.maximumCost = maximumCost;
	}

	public String getExamples() {
		return examples;
	}

	public void setExamples(final String examples) {
		this.examples = examples;
	}
}

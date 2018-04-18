package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.PositiveOrZero;
import java.util.Objects;

@Entity
public class Gear {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Version
	private long version;

	@NotEmpty
	@Column(nullable = false)
	private String name = "";

	private String description = "";

	@PositiveOrZero
	private int cost = 1;

	@PositiveOrZero
	private int weight = 1;

	@NotNull
	@ManyToOne
	private GearCategory category;

	@NotNull
	@ManyToOne
	private GearType type;

	private String notes = "";

	public Gear(@NotEmpty final String name, final String description, @PositiveOrZero final int cost, @PositiveOrZero final int weight, @NotNull final GearCategory category, final String notes, final GearType type) {
		this.name = name;
		this.description = description;
		this.cost = cost;
		this.weight = weight;
		this.category = category;
		this.notes = notes;
		this.type = type;
	}

	public Gear() {

	}

	public String getNotes() {
		return notes;
	}

	public void setNotes(final String notes) {
		this.notes = notes;
	}

	@Override
	public int hashCode() {

		return Objects.hash(getId(), getVersion());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof Gear)) return false;
		final Gear gear = (Gear) o;
		return getId() == gear.getId() &&
				getVersion() == gear.getVersion();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("version", version)
				.append("name", name)
				.append("description", description)
				.append("cost", cost)
				.append("weight", weight)
				.append("category", category)
				.append("type", type)
				.append("notes", notes)
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

	public int getCost() {
		return cost;
	}

	public void setCost(final int cost) {
		this.cost = cost;
	}

	public int getWeight() {
		return weight;
	}

	public void setWeight(final int weight) {
		this.weight = weight;
	}

	public GearCategory getCategory() {
		return category;
	}

	public void setCategory(final GearCategory category) {
		this.category = category;
	}

	public GearType getType() {
		return type;
	}

	public void setType(final GearType type) {
		this.type = type;
	}
}

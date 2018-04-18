package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Positive;
import java.util.Objects;

@Entity
public class Power {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Version
	private long version;

	@NotEmpty
	@Column(nullable = false)
	private String name;

	private String description;

	@Enumerated
	private Rank rank = Rank.novice;

	@Min(1)
	private int powerPoints;

	private String range;

	@Positive
	private int duration = 0;

	@Positive
	private int maintenance = 0;

	private String trappings;

	public Power(@NotEmpty final String name, final String description, final Rank rank, @Min(1) final int powerPoints, final String range, @Positive final int duration, @Positive final int maintenance, final String trappings) {
		this.name = name;
		this.description = description;
		this.rank = rank;
		this.powerPoints = powerPoints;
		this.range = range;
		this.duration = duration;
		this.maintenance = maintenance;
		this.trappings = trappings;
	}

	@Override
	public int hashCode() {

		return Objects.hash(getId(), getVersion());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof Power)) return false;
		final Power power = (Power) o;
		return getId() == power.getId() &&
				getVersion() == power.getVersion();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("version", version)
				.append("name", name)
				.append("description", description)
				.append("rank", rank)
				.append("powerPoints", powerPoints)
				.append("range", range)
				.append("duration", duration)
				.append("maintenance", maintenance)
				.append("trappings", trappings)
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

	public Rank getRank() {
		return rank;
	}

	public void setRank(final Rank rank) {
		this.rank = rank;
	}

	public int getPowerPoints() {
		return powerPoints;
	}

	public void setPowerPoints(final int powerPoints) {
		this.powerPoints = powerPoints;
	}

	public String getRange() {
		return range;
	}

	public void setRange(final String range) {
		this.range = range;
	}

	public int getDuration() {
		return duration;
	}

	public void setDuration(final int duration) {
		this.duration = duration;
	}

	public int getMaintenance() {
		return maintenance;
	}

	public void setMaintenance(final int maintenance) {
		this.maintenance = maintenance;
	}

	public String getTrappings() {
		return trappings;
	}

	public void setTrappings(final String trappings) {
		this.trappings = trappings;
	}
}

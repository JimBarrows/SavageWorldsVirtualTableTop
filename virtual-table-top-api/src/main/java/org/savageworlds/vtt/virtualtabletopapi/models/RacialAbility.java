package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.Objects;

@Entity
public class RacialAbility {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Version
	private long version = 0;

	@NotEmpty
	@Column(nullable = false)
	private String name;

	@NotEmpty
	private String description;

	@NotNull
	@Column(nullable = false)
	private int points = 1;

	public RacialAbility() {
	}

	public RacialAbility(@NotEmpty final String name, final String description, final int points) {
		this.name = name;
		this.description = description;
		this.points = points;
	}

	@Override
	public int hashCode() {

		return Objects.hash(id, version);
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof RacialAbility)) return false;
		final RacialAbility that = (RacialAbility) o;
		return id == that.id &&
				version == that.version;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("version", version)
				.append("name", name)
				.append("description", description)
				.append("points", points)
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

	public int getPoints() {
		return points;
	}

	public void setPoints(final int points) {
		this.points = points;
	}
}

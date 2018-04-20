package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Positive;
import java.util.Objects;

@Entity
public class ArcaneBackground {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Version
	private long version;

	@NotEmpty
	@Column(nullable = false)
	private String name;

	private String description;

	@NotNull
	@ManyToOne
	private Skill skill;

	@Positive
	@Min(1)
	private int startingPowers;

	@Positive
	@Min(1)
	private int startingPowerPoints;

	public ArcaneBackground(@NotEmpty final String name, final String description, @NotNull final Skill skill, @Positive @Min(1) final int startingPowers, @Positive @Min(1) final int startingPowerPoints) {
		this.name = name;
		this.description = description;
		this.skill = skill;
		this.startingPowers = startingPowers;
		this.startingPowerPoints = startingPowerPoints;
	}

	@Override
	public int hashCode() {

		return Objects.hash(getId(), getVersion());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof ArcaneBackground)) return false;
		final ArcaneBackground that = (ArcaneBackground) o;
		return getId() == that.getId() &&
				getVersion() == that.getVersion();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("version", version)
				.append("name", name)
				.append("description", description)
				.append("skill", skill)
				.append("startingPowers", startingPowers)
				.append("startingPowerPoints", startingPowerPoints)
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

	public Skill getSkill() {
		return skill;
	}

	public void setSkill(final Skill skill) {
		this.skill = skill;
	}

	public int getStartingPowers() {
		return startingPowers;
	}

	public void setStartingPowers(final int startingPowers) {
		this.startingPowers = startingPowers;
	}

	public int getStartingPowerPoints() {
		return startingPowerPoints;
	}

	public void setStartingPowerPoints(final int startingPowerPoints) {
		this.startingPowerPoints = startingPowerPoints;
	}
}

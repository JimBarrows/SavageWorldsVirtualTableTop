package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

@Entity
public class Race {
	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Version
	private long version;

	@NotEmpty
	@Column(nullable = false)
	private String name;

	private String description;

	@OneToMany(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
	@OrderBy("name")
	private Set<RacialAbility> abilities = new LinkedHashSet<>();

	public Race() {

	}

	public Race(@NotEmpty final String name, final String description, final LinkedHashSet<RacialAbility> abilities) {
		this.name = name;
		this.description = description;
		this.abilities = abilities;
	}

	@Override
	public int hashCode() {

		return Objects.hash(getId(), getVersion());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof Race)) return false;
		final Race race = (Race) o;
		return getId() == race.getId() &&
				getVersion() == race.getVersion();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("version", version)
				.append("name", name)
				.append("description", description)
				.append("abilities", abilities)
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

	public Set<RacialAbility> getAbilities() {
		return abilities;
	}

	public void setAbilities(final Set<RacialAbility> abilities) {
		this.abilities = abilities;
	}
}

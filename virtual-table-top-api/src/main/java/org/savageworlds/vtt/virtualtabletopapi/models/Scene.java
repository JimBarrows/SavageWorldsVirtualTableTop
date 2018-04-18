package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Positive;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

@Entity
public class Scene {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Version
	private long version;

	@NotEmpty
	@Column(nullable = false)
	private String name;

	@NotEmpty
	private String playerDescription;

	@NotEmpty
	private String gmDescription;

	@Positive
	@Min(1)
	private int sequence;

	@ManyToMany(fetch = FetchType.EAGER)
	@OrderBy("name")
	private Set<Persona> cast = new LinkedHashSet<>();

	@ManyToMany(fetch = FetchType.EAGER)
	@OrderBy("name")
	private Set<Beast> animals = new LinkedHashSet<>();

	@ManyToOne
	@NotNull
	private Location location;

	@Override
	public int hashCode() {

		return Objects.hash(getId(), getVersion(), getName());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof Scene)) return false;
		final Scene scene = (Scene) o;
		return getId() == scene.getId() &&
				getVersion() == scene.getVersion() &&
				Objects.equals(getName(), scene.getName());
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("version", version)
				.append("name", name)
				.append("playerDescription", playerDescription)
				.append("gmDescription", gmDescription)
				.append("sequence", sequence)
				.append("cast", cast)
				.append("animals", animals)
				.append("location", location)
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

	public String getPlayerDescription() {
		return playerDescription;
	}

	public void setPlayerDescription(final String playerDescription) {
		this.playerDescription = playerDescription;
	}

	public String getGmDescription() {
		return gmDescription;
	}

	public void setGmDescription(final String gmDescription) {
		this.gmDescription = gmDescription;
	}

	public int getSequence() {
		return sequence;
	}

	public void setSequence(final int sequence) {
		this.sequence = sequence;
	}

	public Set<Persona> getCast() {
		return cast;
	}

	public void setCast(final Set<Persona> cast) {
		this.cast = cast;
	}

	public Set<Beast> getAnimals() {
		return animals;
	}

	public void setAnimals(final Set<Beast> animals) {
		this.animals = animals;
	}

	public Location getLocation() {
		return location;
	}

	public void setLocation(final Location location) {
		this.location = location;
	}
}

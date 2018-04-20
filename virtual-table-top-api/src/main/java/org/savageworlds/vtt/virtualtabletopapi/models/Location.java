package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

@Entity
public class Location {

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

	@ManyToOne
	private Location inside;

	@OneToMany(cascade = CascadeType.ALL, mappedBy = "inside", fetch = FetchType.EAGER)
	@OrderBy("name")
	private Set<Location> contains = new LinkedHashSet<>();

	@OneToMany(fetch = FetchType.EAGER, mappedBy = "startingLocation")
	@OrderBy("name")
	private Set<Story> savageTales = new LinkedHashSet<>();

	@Override
	public int hashCode() {

		return Objects.hash(getId(), getVersion());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof Location)) return false;
		final Location location = (Location) o;
		return getId() == location.getId() &&
				getVersion() == location.getVersion();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("version", version)
				.append("name", name)
				.append("playerDescription", playerDescription)
				.append("gmDescription", gmDescription)
				.append("inside", inside)
				.append("contains", contains)
				.append("savageTales", savageTales)
				.toString();
	}

	public String getGmDescription() {
		return gmDescription;
	}

	public void setGmDescription(final String gmDescription) {
		this.gmDescription = gmDescription;
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

	public Location getInside() {
		return inside;
	}

	public void setInside(final Location inside) {
		this.inside = inside;
	}

	public Set<Location> getContains() {
		return contains;
	}

	public void setContains(final Set<Location> contains) {
		this.contains = contains;
	}

	public Set<Story> getSavageTales() {
		return savageTales;
	}

	public void setSavageTales(final Set<Story> savageTales) {
		this.savageTales = savageTales;
	}
}

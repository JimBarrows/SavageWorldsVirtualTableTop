package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

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

	private String description;

	@ManyToOne
	private Location inside;

	@OneToMany(cascade = CascadeType.ALL)
	private List<Location> contains = new ArrayList<>();

	public Location(@NotEmpty final String name, final String description, final Location inside, final List<Location> contains) {
		this.name = name;
		this.description = description;
		this.inside = inside;
		this.contains = contains;
	}

	public Location() {
	}

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
				.append("description", description)
				.append("inside", inside)
				.append("contains", contains)
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

	public Location getInside() {
		return inside;
	}

	public void setInside(final Location inside) {
		this.inside = inside;
	}

	public List<Location> getContains() {
		return contains;
	}

	public void setContains(final List<Location> contains) {
		this.contains = contains;
	}
}

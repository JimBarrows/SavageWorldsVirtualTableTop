package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.Objects;

@Entity
public class Edge {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Version
	private long version;

	@NotEmpty
	@Column(nullable = false)
	private String name = "";

	private String description = "";

	private String requirements = "";

	private boolean improvable = false;

	@NotNull
	@ManyToOne
	private EdgeCategory category;

	public Edge(@NotEmpty final String name, final String description, final String requirements, final boolean improvable, @NotNull final EdgeCategory category) {
		this.name = name;
		this.description = description;
		this.requirements = requirements;
		this.improvable = improvable;
		this.category = category;
	}

	public Edge() {
	}

	@Override
	public int hashCode() {

		return Objects.hash(getId(), getVersion());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof Edge)) return false;
		final Edge edge = (Edge) o;
		return getId() == edge.getId() &&
				getVersion() == edge.getVersion();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("version", version)
				.append("name", name)
				.append("description", description)
				.append("requirements", requirements)
				.append("improvable", improvable)
				.append("category", category)
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

	public String getRequirements() {
		return requirements;
	}

	public void setRequirements(final String requirements) {
		this.requirements = requirements;
	}

	public boolean isImprovable() {
		return improvable;
	}

	public void setImprovable(final boolean improvable) {
		this.improvable = improvable;
	}

	public EdgeCategory getCategory() {
		return category;
	}

	public void setCategory(final EdgeCategory category) {
		this.category = category;
	}
}

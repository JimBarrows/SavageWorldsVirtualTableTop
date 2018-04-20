package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.Objects;

@Entity
public class Skill {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Version
	private long version;

	@NotEmpty
	@Column(nullable = false)
	private String name;

	@NotEmpty
	private String description;

	@Enumerated
	@NotNull
	private Attributes attribute;

	public Skill() {

	}

	public Skill(@NotEmpty final String name, @NotEmpty final String description, @NotNull final Attributes attribute) {
		this.name = name;
		this.description = description;
		this.attribute = attribute;
	}

	@Override
	public int hashCode() {

		return Objects.hash(getId(), getVersion(), getName());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof Skill)) return false;
		final Skill skill = (Skill) o;
		return getId() == skill.getId() &&
				getVersion() == skill.getVersion() &&
				Objects.equals(getName(), skill.getName());
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("version", version)
				.append("name", name)
				.append("description", description)
				.append("attribute", attribute)
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

	public Attributes getAttribute() {
		return attribute;
	}

	public void setAttribute(final Attributes attribute) {
		this.attribute = attribute;
	}
}

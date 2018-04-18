package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.Objects;

@Entity
public class Hindrance {

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
	private HindranceSeverity severity;

	public Hindrance(@NotEmpty final String name, @NotEmpty final String description, final HindranceSeverity severity) {
		this.name = name;
		this.description = description;
		this.severity = severity;
	}

	public Hindrance() {
	}

	@Override
	public int hashCode() {

		return Objects.hash(getId(), getVersion());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof Hindrance)) return false;
		final Hindrance hindrance = (Hindrance) o;
		return getId() == hindrance.getId() &&
				getVersion() == hindrance.getVersion();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("version", version)
				.append("name", name)
				.append("description", description)
				.append("severity", severity)
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

	public HindranceSeverity getSeverity() {
		return severity;
	}

	public void setSeverity(final HindranceSeverity severity) {
		this.severity = severity;
	}
}


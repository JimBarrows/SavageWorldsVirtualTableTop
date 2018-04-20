package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.util.Objects;

@Entity
public class PersonaHindrance {
	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;
	@Version
	private long version;
	@ManyToOne
	@NotNull
	private Hindrance hindrance;
	private String notes;

	@Override
	public int hashCode() {

		return Objects.hash(getId(), getVersion());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof PersonaHindrance)) return false;
		final PersonaHindrance that = (PersonaHindrance) o;
		return getId() == that.getId() &&
				getVersion() == that.getVersion();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("version", version)
				.append("hindrance", hindrance)
				.append("notes", notes)
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

	public Hindrance getHindrance() {
		return hindrance;
	}

	public void setHindrance(final Hindrance hindrance) {
		this.hindrance = hindrance;
	}

	public String getNotes() {
		return notes;
	}

	public void setNotes(final String notes) {
		this.notes = notes;
	}
}

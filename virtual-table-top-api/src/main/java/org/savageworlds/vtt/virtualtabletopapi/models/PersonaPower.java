package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.Objects;

@Entity
public class PersonaPower {
	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;
	@Version
	private long version;
	@ManyToOne
	@NotNull
	private Power power;
	@NotEmpty
	private String trappings;

	@Override
	public int hashCode() {

		return Objects.hash(getId(), getVersion());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof PersonaPower)) return false;
		final PersonaPower that = (PersonaPower) o;
		return getId() == that.getId() &&
				getVersion() == that.getVersion();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("version", version)
				.append("power", power)
				.append("trappings", trappings)
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

	public Power getPower() {
		return power;
	}

	public void setPower(final Power power) {
		this.power = power;
	}

	public String getTrappings() {
		return trappings;
	}

	public void setTrappings(final String trappings) {
		this.trappings = trappings;
	}
}

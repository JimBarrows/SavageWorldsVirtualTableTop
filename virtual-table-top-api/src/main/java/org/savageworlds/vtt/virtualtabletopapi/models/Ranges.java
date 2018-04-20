package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.Embeddable;
import java.util.Objects;

@Embeddable
public class Ranges {

	private long shortRange;
	private long mediumRange;
	private long longRange;

	public Ranges(final long shortRange, final long mediumRange, final long longRange) {
		this.shortRange = shortRange;
		this.mediumRange = mediumRange;
		this.longRange = longRange;
	}

	@Override
	public int hashCode() {

		return Objects.hash(getShortRange(), getMediumRange(), getLongRange());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof Ranges)) return false;
		final Ranges ranges = (Ranges) o;
		return getShortRange() == ranges.getShortRange() &&
				getMediumRange() == ranges.getMediumRange() &&
				getLongRange() == ranges.getLongRange();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("shortRange", shortRange)
				.append("mediumRange", mediumRange)
				.append("longRange", longRange)
				.toString();
	}

	public long getShortRange() {
		return shortRange;
	}

	public void setShortRange(final long shortRange) {
		this.shortRange = shortRange;
	}

	public long getMediumRange() {
		return mediumRange;
	}

	public void setMediumRange(final long mediumRange) {
		this.mediumRange = mediumRange;
	}

	public long getLongRange() {
		return longRange;
	}

	public void setLongRange(final long longRange) {
		this.longRange = longRange;
	}
}

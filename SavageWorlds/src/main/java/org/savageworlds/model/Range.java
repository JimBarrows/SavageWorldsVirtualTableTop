package org.savageworlds.model;

import javax.persistence.Embeddable;

@Embeddable
public class Range {

	private Integer shortRange;
	
	private Integer mediumRange;
	
	private Integer longRange;

	public Integer getShort() {
		return shortRange;
	}

	public void setShort(Integer shortRange) {
		this.shortRange = shortRange;
	}

	public Integer getMedium() {
		return mediumRange;
	}

	public void setMedium(Integer mediumRange) {
		this.mediumRange = mediumRange;
	}

	public Integer getLong() {
		return longRange;
	}

	public void setLong(Integer longRange) {
		this.longRange = longRange;
	}
}

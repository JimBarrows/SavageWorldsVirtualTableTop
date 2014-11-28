package org.savageworlds.model;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Entity implementation class for Entity: RangedWeaponDescription
 *
 */
@XmlRootElement
@Entity
public class RangedWeaponDescription extends GearDescription implements Serializable {

	private Integer				shortRange;
	private Integer				mediumRange;
	private Integer				longRange;
	private AttributeTypes		attribute;
	private DiceType			dice;
	private Integer				number;
	private Integer				bonus;
	private Integer				rateOfFire;
	private Integer				shots;
	private DiceType			minStrength;
	private static final long	serialVersionUID	= 1L;

	public RangedWeaponDescription() {
		super();
	}

	public Integer getShortRange() {
		return this.shortRange;
	}

	public void setShortRange(Integer shortRange) {
		this.shortRange = shortRange;
	}

	public Integer getMediumRange() {
		return this.mediumRange;
	}

	public void setMediumRange(Integer mediumRange) {
		this.mediumRange = mediumRange;
	}

	public Integer getLongRange() {
		return this.longRange;
	}

	public void setLongRange(Integer longRange) {
		this.longRange = longRange;
	}

	public AttributeTypes getAttribute() {
		return this.attribute;
	}

	public void setAttribute(AttributeTypes attribute) {
		this.attribute = attribute;
	}

	public DiceType getDice() {
		return this.dice;
	}

	public void setDice(DiceType dice) {
		this.dice = dice;
	}

	public Integer getNumber() {
		return this.number;
	}

	public void setNumber(Integer number) {
		this.number = number;
	}

	public Integer getBonus() {
		return this.bonus;
	}

	public void setBonus(Integer bonus) {
		this.bonus = bonus;
	}

	public Integer getRateOfFire() {
		return this.rateOfFire;
	}

	public void setRateOfFire(Integer rateOfFire) {
		this.rateOfFire = rateOfFire;
	}

	public Integer getShots() {
		return this.shots;
	}

	public void setShots(Integer shots) {
		this.shots = shots;
	}

	public DiceType getMinStrength() {
		return this.minStrength;
	}

	public void setMinStrength(DiceType minStrength) {
		this.minStrength = minStrength;
	}

}

package org.savageworlds.model;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Entity implementation class for Entity: HandWeaponDescription
 *
 */
@Entity
@XmlRootElement
public class HandWeaponDescription extends GearDescription implements Serializable {

	private AttributeTypes		attribute;
	private DiceType			dice;
	private Integer				bonus;
	private static final long	serialVersionUID	= 1L;

	public HandWeaponDescription() {
		super();
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

	public Integer getBonus() {
		return this.bonus;
	}

	public void setBonus(Integer bonus) {
		this.bonus = bonus;
	}

}

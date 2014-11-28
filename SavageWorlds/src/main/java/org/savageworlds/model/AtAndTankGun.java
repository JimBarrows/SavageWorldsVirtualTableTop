package org.savageworlds.model;

import java.io.Serializable;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.validation.constraints.NotNull;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Entity implementation class for Entity: AtAndTankGun
 *
 */
@XmlRootElement
@Entity
public class AtAndTankGun extends GearDescription implements Serializable {

	@NotNull
	private Range				range;
	private Integer				rateOfFire = 1;
	@AttributeOverrides({ @AttributeOverride(name = "round", column = @Column(name = "ap_roundType")),
			@AttributeOverride(name = "dice.number", column = @Column(name = "ap_dice_number")),
			@AttributeOverride(name = "dice.dice", column = @Column(name = "ap_dice_type")),
			@AttributeOverride(name = "dice.bonus", column = @Column(name = "ap_dice_bonus")),
			@AttributeOverride(name = "armorPierce", column = @Column(name = "ap_ArmorPierce")),
			@AttributeOverride(name = "template", column = @Column(name = "ap_template")) })
	private Round				armorPiercing;
	@AttributeOverrides({ @AttributeOverride(name = "round", column = @Column(name = "he_roundType")),
			@AttributeOverride(name = "dice.number", column = @Column(name = "he_dice_number")),
			@AttributeOverride(name = "dice.dice", column = @Column(name = "he_dice_type")),
			@AttributeOverride(name = "dice.bonus", column = @Column(name = "he_dice_bonus")),
			@AttributeOverride(name = "armorPierce", column = @Column(name = "he_ArmorPierce")),
			@AttributeOverride(name = "template", column = @Column(name = "he_template")) })
	private Round				highExplosive;
	private static final long	serialVersionUID	= 1L;

	public AtAndTankGun() {
		super();
	}

	public Integer getRateOfFire() {
		return this.rateOfFire;
	}

	public void setRateOfFire(Integer rateOfFire) {
		this.rateOfFire = rateOfFire;
	}

	public Round getArmorPiercing() {
		return armorPiercing;
	}

	public void setArmorPiercing(Round armorPiercing) {
		this.armorPiercing = armorPiercing;
	}

	public Round getHighExplosive() {
		return highExplosive;
	}

	public void setHighExplosive(Round highExplosive) {
		this.highExplosive = highExplosive;
	}

	public Range getRange() {
		return range;
	}

	public void setRange(Range range) {
		this.range = range;
	}

}

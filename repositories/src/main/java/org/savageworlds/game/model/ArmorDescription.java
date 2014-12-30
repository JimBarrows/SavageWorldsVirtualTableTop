package org.savageworlds.game.model;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.xml.bind.annotation.XmlRootElement;

import com.fasterxml.jackson.annotation.JsonRootName;

/**
 * Entity implementation class for Entity: ArmorDescription
 *
 */
@Entity
@XmlRootElement
@JsonRootName("armorDescription")
public class ArmorDescription extends GearDescription implements Serializable {

	private Integer				armor;
	private Integer				vsBullets;
	private static final long	serialVersionUID	= 1L;

	public ArmorDescription() {
		super();
	}

	public Integer getArmor() {
		return this.armor;
	}

	public void setArmor(Integer armor) {
		this.armor = armor;
	}

	public Integer getVsBullets() {
		return this.vsBullets;
	}

	public void setVsBullets(Integer vsBullets) {
		this.vsBullets = vsBullets;
	}

}

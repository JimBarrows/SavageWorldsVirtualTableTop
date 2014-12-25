package org.savageworlds.game.model;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

import jdo.model.BasePersistentModel;

@Entity
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
public class GearDescription extends BasePersistentModel implements Serializable {	

	@NotNull
	private String	name;
	
	@NotNull
	@Min(value=0)
	private Long	weight;
	
	@NotNull
	@Min(value=0)
	private Long	cost;
	
	private String	notes;
	
	private EraType era;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Long getWeight() {
		return weight;
	}

	public void setWeight(Long weight) {
		this.weight = weight;
	}

	public Long getCost() {
		return cost;
	}

	public void setCost(Long cost) {
		this.cost = cost;
	}

	public String getNotes() {
		return notes;
	}

	public void setNotes(String notes) {
		this.notes = notes;
	}

	public GearDescription(Long id, String name, Long weight, Long cost, String notes) {
		super();
		this.setId(id);
		this.name = name;
		this.weight = weight;
		this.cost = cost;
		this.notes = notes;
	}

	public GearDescription() {
		super();		
	}

	public EraType getEra() {
		return era;
	}

	public void setEra(EraType era) {
		this.era = era;
	}
	
	/**
	 * 
	 */
	private static final long	serialVersionUID	= 1L;
}

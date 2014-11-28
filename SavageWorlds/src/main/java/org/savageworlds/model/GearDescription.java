package org.savageworlds.model;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.xml.bind.annotation.XmlRootElement;

@Entity
@XmlRootElement
public class GearDescription implements Serializable {	

	@Id
	@GeneratedValue
	private Long	id;

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
	
	/**
	 * 
	 */
	private static final long	serialVersionUID	= 1L;

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

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
		this.id = id;
		this.name = name;
		this.weight = weight;
		this.cost = cost;
		this.notes = notes;
	}

	public GearDescription() {
		super();
		// TODO Auto-generated constructor stub
	}

	public EraType getEra() {
		return era;
	}

	public void setEra(EraType era) {
		this.era = era;
	}
}

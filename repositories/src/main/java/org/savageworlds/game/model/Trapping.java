package org.savageworlds.game.model;

import javax.persistence.Entity;
import javax.xml.bind.annotation.XmlRootElement;

import jdo.model.BasePersistentModel;

/**
 * Entity implementation class for Entity: Trapping
 *
 */
@Entity
@XmlRootElement
public class Trapping extends BasePersistentModel {
	
	private String				name;
	private String				description;
	private static final long	serialVersionUID	= 1L;

	public Trapping() {
		super();
	}

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return this.description;
	}

	public void setDescription(String description) {
		this.description = description;
	}	

}

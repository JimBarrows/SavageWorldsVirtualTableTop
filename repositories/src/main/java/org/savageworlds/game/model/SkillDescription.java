package org.savageworlds.game.model;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.Version;
import javax.validation.constraints.NotNull;

import jdo.model.BasePersistentModel;

import org.hibernate.validator.constraints.NotEmpty;

import com.fasterxml.jackson.annotation.JsonRootName;

@Entity
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@JsonRootName("skillDescription")
public class SkillDescription extends BasePersistentModel implements Serializable{

	/**
	 * 
	 */
	private static final long	serialVersionUID	= 1L;
	
	@NotEmpty
	private String	name;
	
	@NotNull
	private AttributeTypes attribute;
	
	private String description;	

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public AttributeTypes getAttribute() {
		return attribute;
	}

	public void setAttribute(AttributeTypes attribute) {
		this.attribute = attribute;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}
	
}

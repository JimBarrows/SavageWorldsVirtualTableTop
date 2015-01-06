package org.savageworlds.game.model;

import javax.persistence.Entity;
import javax.persistence.Lob;
import javax.persistence.OrderBy;
import javax.xml.bind.annotation.XmlRootElement;

import jdo.model.BasePersistentModel;

import org.hibernate.validator.constraints.NotEmpty;

import com.fasterxml.jackson.annotation.JsonRootName;

@Entity
@XmlRootElement
@JsonRootName("edgeType")
public class EdgeType extends BasePersistentModel {

	/**
	 * 
	 */
	private static final long	serialVersionUID	= 1L;

	@NotEmpty
	@OrderBy
	private String						name;

	@Lob
	private String						description;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}
}

package org.savageworlds.game.model;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.OneToMany;
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
	private String				name;

	private String				description;
	
	@OneToMany
	private List<EdgeDescription> edges = new ArrayList<EdgeDescription>();
	
	public List<EdgeDescription> getEdges() {
		return edges;
	}

	public void setEdges(List<EdgeDescription> edges) {
		this.edges = edges;
	}

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

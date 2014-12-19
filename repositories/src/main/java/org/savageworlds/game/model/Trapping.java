package org.savageworlds.game.model;

import java.io.Serializable;
import java.lang.String;

import javax.persistence.*;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Entity implementation class for Entity: Trapping
 *
 */
@Entity
@XmlRootElement
public class Trapping implements Serializable {

	@Id
	@GeneratedValue
	private Long				id;
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

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

}

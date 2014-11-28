package org.savageworlds.model;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.validation.constraints.NotNull;
import javax.xml.bind.annotation.XmlRootElement;

@Entity
@XmlRootElement
public class HindranceDescription implements Serializable{

	/**
	 * 
	 */
	private static final long	serialVersionUID	= 1L;

	@Id
	@GeneratedValue
	private Long	id;

	@NotNull
	private String	name;
	
	@NotNull
	private HindranceSeverity severity;
	
	@NotNull
	private String effects;

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

	public HindranceSeverity getSeverity() {
		return severity;
	}

	public void setSeverity(HindranceSeverity severity) {
		this.severity = severity;
	}

	public String getEffects() {
		return effects;
	}

	public void setEffects(String effects) {
		this.effects = effects;
	}
	
	
}

package org.savageworlds.game.model;

import javax.persistence.Entity;
import javax.validation.constraints.NotNull;
import javax.xml.bind.annotation.XmlRootElement;

import jdo.model.BasePersistentModel;

@Entity
@XmlRootElement
public class HindranceDescription extends BasePersistentModel{

	/**
	 * 
	 */
	private static final long	serialVersionUID	= 1L;	

	@NotNull
	private String	name;
	
	@NotNull
	private HindranceSeverity severity;
	
	@NotNull
	private String effects;	

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

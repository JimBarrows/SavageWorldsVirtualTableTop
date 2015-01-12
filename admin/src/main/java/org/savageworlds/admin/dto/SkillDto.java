package org.savageworlds.admin.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonRootName;

@JsonRootName("skill")
public class SkillDto implements Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private Long id;
	private Long version;
	private Long description;
	private String dice;
	private Long edge;
	
	public Long getEdge() {
		return edge;
	}
	public void setEdge(Long edge) {
		this.edge = edge;
	}
	public Long getId() {
		return id;
	}
	public void setId(Long id) {
		this.id = id;
	}
	public Long getVersion() {
		return version;
	}
	public void setVersion(Long version) {
		this.version = version;
	}
	public Long getDescription() {
		return description;
	}
	public void setDescription(Long description) {
		this.description = description;
	}
	public String getDice() {
		return dice;
	}
	public void setDice(String dice) {
		this.dice = dice;
	}
	public Integer getBonus() {
		return bonus;
	}
	public void setBonus(Integer bonus) {
		this.bonus = bonus;
	}
	private Integer bonus;
}

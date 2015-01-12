package org.savageworlds.game.model;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.xml.bind.annotation.XmlRootElement;

import jdo.model.BasePersistentModel;

@Entity
@XmlRootElement
public class Skill extends BasePersistentModel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@NotNull
	@ManyToOne
	private SkillDescription skill;

	@NotNull
	private DiceType dice;

	@Min(0)
	private Integer bonus = 0;

	@ManyToOne
	private EdgeDescription edgeDescription;

	public Skill(SkillDescription skillDescription, DiceType diceType) {
		skill = skillDescription;
		dice = diceType;
	}

	public Skill() {
		super();
	}

	public EdgeDescription getEdgeDescription() {
		return edgeDescription;
	}

	public void setEdgeDescription(EdgeDescription edgeDescription) {
		this.edgeDescription = edgeDescription;
	}

	public SkillDescription getSkill() {
		return skill;
	}

	public void setSkill(SkillDescription skill) {
		this.skill = skill;
	}

	public DiceType getDice() {
		return dice;
	}

	public void setDice(DiceType dice) {
		this.dice = dice;
	}

	public Integer getBonus() {
		return bonus;
	}

	public void setBonus(Integer bonus) {
		this.bonus = bonus;
	}

}

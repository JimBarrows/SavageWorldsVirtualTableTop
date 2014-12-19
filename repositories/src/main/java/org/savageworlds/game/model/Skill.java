package org.savageworlds.game.model;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.xml.bind.annotation.XmlRootElement;

@Entity
@XmlRootElement
public class Skill {

	@Id
	@GeneratedValue
	private Long				id;

	@NotNull
	@ManyToOne
	private SkillDescription	skill;

	@NotNull
	private DiceType			dice;

	@Min(0)
	private Integer				bonus	= 0;

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
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

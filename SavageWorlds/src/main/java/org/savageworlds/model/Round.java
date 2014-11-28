package org.savageworlds.model;

import javax.persistence.Embeddable;

@Embeddable
public class Round {

	private RoundType			round;

	private Die					dice;

	private Integer				armorPierce;

	private BurstTemplateType	template;

	public RoundType getRound() {
		return round;
	}

	public void setRound(RoundType round) {
		this.round = round;
	}

	public Die getDice() {
		return dice;
	}

	public void setDice(Die dice) {
		this.dice = dice;
	}

	public BurstTemplateType getTemplate() {
		return template;
	}

	public void setTemplate(BurstTemplateType template) {
		this.template = template;
	}

	public Integer getArmorPierce() {
		return armorPierce;
	}

	public void setArmorPierce(Integer armorPierce) {
		this.armorPierce = armorPierce;
	}
}

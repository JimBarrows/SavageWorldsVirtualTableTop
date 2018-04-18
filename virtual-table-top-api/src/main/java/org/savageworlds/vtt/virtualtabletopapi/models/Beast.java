package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.Entity;
import javax.persistence.OneToMany;
import java.util.ArrayList;
import java.util.List;

@Entity
public class Beast extends Creature {

	private boolean animalIntelligence = true;

	@OneToMany
	private List<BeastAbility> abilities = new ArrayList<>();

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("animalIntelligence", animalIntelligence)
				.append("abilities", abilities)
				.toString();
	}

	public boolean isAnimalIntelligence() {
		return animalIntelligence;
	}

	public void setAnimalIntelligence(final boolean animalIntelligence) {
		this.animalIntelligence = animalIntelligence;
	}

	public List<BeastAbility> getAbilities() {
		return abilities;
	}

	public void setAbilities(final List<BeastAbility> abilities) {
		this.abilities = abilities;
	}
}

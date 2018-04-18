package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.PositiveOrZero;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@MappedSuperclass
public class Creature {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;
	@Version
	private long version;
	@NotEmpty
	@Column(nullable = false)
	private String name;
	private String description;
	@Embedded
	@AttributeOverrides({
			@AttributeOverride(name = "dice", column = @Column(name = "attributeDice")),
			@AttributeOverride(name = "bonus", column = @Column(name = "attributeBonus"))
	})
	private Attribute agility = new Attribute(Dice.d4);
	@Embedded
	@AttributeOverrides({
			@AttributeOverride(name = "dice", column = @Column(name = "smartsDice")),
			@AttributeOverride(name = "bonus", column = @Column(name = "smartsBonus"))
	})
	private Attribute smarts = new Attribute(Dice.d4);
	@Embedded
	@AttributeOverrides({
			@AttributeOverride(name = "dice", column = @Column(name = "spiritDice")),
			@AttributeOverride(name = "bonus", column = @Column(name = "spiritBonus"))
	})
	private Attribute spirit = new Attribute(Dice.d4);
	@Embedded
	@AttributeOverrides({
			@AttributeOverride(name = "dice", column = @Column(name = "strengthDice")),
			@AttributeOverride(name = "bonus", column = @Column(name = "strengthBonus"))
	})
	private Attribute strength = new Attribute(Dice.d4);
	@Embedded
	@AttributeOverrides({
			@AttributeOverride(name = "dice", column = @Column(name = "vigorDice")),
			@AttributeOverride(name = "bonus", column = @Column(name = "vigorBonus"))
	})
	private Attribute vigor = new Attribute(Dice.d4);
	@OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
	private List<CreatureSkill> skills = new ArrayList<>();
	@PositiveOrZero
	private int pace;
	private boolean wildCard = false;

	public Creature(@NotEmpty final String name, final String description, final Attribute agility, final Attribute smarts, final Attribute spirit, final Attribute strength, final Attribute vigor, final List<CreatureSkill> skills, @PositiveOrZero final int pace, final boolean wildCard) {
		this.name = name;
		this.description = description;
		this.agility = agility;
		this.smarts = smarts;
		this.spirit = spirit;
		this.strength = strength;
		this.vigor = vigor;
		this.skills = skills;
		this.pace = pace;
		this.wildCard = wildCard;
	}

	public Creature() {
	}

	@Transient
	public int parry() {
		return (skills.stream()
				.filter(s -> s.getSkill().getName().equals("Fighting"))
				.map(s -> s.getAttribute().getDice().getValue())
				.findFirst()
				.orElse(0) / 2) + 2;
	}

	@Transient
	public int toughness() {
		return (vigor.getDice().getValue() / 2) + 2;
	}

	public boolean isWildCard() {
		return wildCard;
	}

	public void setWildCard(final boolean wildCard) {
		this.wildCard = wildCard;
	}

	@Override
	public int hashCode() {

		return Objects.hash(getId(), getVersion());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof Creature)) return false;
		final Creature creature = (Creature) o;
		return getId() == creature.getId() &&
				getVersion() == creature.getVersion();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("version", version)
				.append("name", name)
				.append("description", description)
				.append("agility", agility)
				.append("smarts", smarts)
				.append("spirit", spirit)
				.append("strength", strength)
				.append("vigor", vigor)
				.append("skills", skills)
				.append("pace", pace)
				.append("wildCard", wildCard)
				.toString();
	}

	public long getId() {
		return id;
	}

	public void setId(final long id) {
		this.id = id;
	}

	public long getVersion() {
		return version;
	}

	public void setVersion(final long version) {
		this.version = version;
	}

	public String getName() {
		return name;
	}

	public void setName(final String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(final String description) {
		this.description = description;
	}

	public Attribute getAgility() {
		return agility;
	}

	public void setAgility(final Attribute agility) {
		this.agility = agility;
	}

	public Attribute getSmarts() {
		return smarts;
	}

	public void setSmarts(final Attribute smarts) {
		this.smarts = smarts;
	}

	public Attribute getSpirit() {
		return spirit;
	}

	public void setSpirit(final Attribute spirit) {
		this.spirit = spirit;
	}

	public Attribute getStrength() {
		return strength;
	}

	public void setStrength(final Attribute strength) {
		this.strength = strength;
	}

	public Attribute getVigor() {
		return vigor;
	}

	public void setVigor(final Attribute vigor) {
		this.vigor = vigor;
	}

	public List<CreatureSkill> getSkills() {
		return skills;
	}

	public void setSkills(final List<CreatureSkill> skills) {
		this.skills = skills;
	}

	public int getPace() {
		return pace;
	}

	public void setPace(final int pace) {
		this.pace = pace;
	}
}
